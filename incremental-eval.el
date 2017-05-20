;;; incremental-eval --- incrementally, and interactively evaluate lisp forms -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(defun incremental-eval (p)
  "Evaluate a Lisp form incrementally and interactively,

Either evaluates the active region or the top level form for
point, P, or the last sexp."
  (interactive "d")
  (pcase (incremental-eval-find-region p)
    (`(,beg ,end) (incremental-eval-region beg end))))

(defvar incremental-eval-engine '()
  "The backend for eval'ing an sexp.

It should eval the next sexp, by convention.

This should be set for the desired mode by requiring a setup file
for that mode.  e.g. add (require 'incremental-eval-emacs-lisp)
to get the hook to set up incremental eval for Emacs Lisp.  Note
that the hook is automatically added by requiring that file.")

(defun incremental-eval-region (beg end)
  "Incrementally evaluate the region described by BEG END."
  (incremental-eval-region-setup beg end (buffer-substring-no-properties beg end)))

(defun incremental-eval-stop (p)
  "Stop evaluating the region at point P."
  (interactive "d")
  (progn
    (goto-char p)
    (let* ((overlay           (incremental-eval-get-overlay-at-point))
           (start             (overlay-start overlay))
           (end               (overlay-end   overlay))
           (inhibit-read-only t))
      (when overlay
        (remove-text-properties start end '(read-only t))
        (delete-region start end)
        (insert (overlay-get overlay 'original-form))))))

(defun incremental-eval-region-setup (beg end original-form)
  "Setup an overlay and read only for doing incremental eval.

Region for overlay is [BEG, END].

If an overlay exists already then ensure that the region in that
overlay is read only

Put the ORIGINAL-FORM on the overlay so that it can be restored
when done."
  (let ((incremental-eval-overlay
         (or (incremental-eval-get-overlay-at-point)
             (incremental-eval-new-overlay beg end original-form))))
    (add-text-properties (overlay-start incremental-eval-overlay)
                         (overlay-end   incremental-eval-overlay)
                         '(read-only t))))

(defvar incremental-eval-keymap '()
  "A keymap for overlays where incremental eval is happening.")

(unless incremental-eval-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'incremental-eval-go-forwards)
    (define-key map "p" #'incremental-eval-go-backwards)
    (define-key map "q" #'incremental-eval-stop)
    (setq incremental-eval-keymap map)))

(defun incremental-eval-new-overlay (beg end original-form)
  "Create an overlay for doing incremental eval.

Region for ovelay is [BEG, END].

Put ORIGINAL-FORM on the overlay so that it can be restored."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'incremental-eval t)
    (overlay-put overlay 'keymap           incremental-eval-keymap)
    (overlay-put overlay 'original-form    original-form)
    overlay))

(defun incremental-eval-get-overlay-at-point ()
  "Get the `incremental-eval' overlay at point."
  (let* ((overlays                  (overlays-at (point)))
         (incremental-eval-overlays (cl-remove-if-not (lambda (overlay)
                                                        (overlay-get overlay 'incremental-eval))
                                                      overlays)))
    (if (not (null incremental-eval-overlays))
        (car incremental-eval-overlays)
      nil)))

(defun incremental-eval-go-forwards ()
  "Go down until something needs to be evaluated and evaluate it."
  (interactive)
  (progn
    (incremental-eval-go-to-toplevel)
    (let (current-state)
      (while (eq 'CONTINUE (setq current-state
                                 (incremental-eval-iterate-sexp (point)))))
      (when (eq current-state 'EVAL-NOW)
        (incremental-eval-backwards-up-list-w/o-error))
      (pcase current-state
        ('EVAL-NOW (incremental-eval-replace-next-sexp-with
                    (funcall incremental-eval-engine (incremental-eval-next-sexp))))
        ('DONE     'DONE)
        (_         (error "Unmapped iteration state"))))))

(defun incremental-eval-go-backwards ()
  "Go backward from the current state of incremental evaluation of the current form."
  (interactive)
  (let ((overlay (incremental-eval-get-overlay-at-point)))
    (pcase (overlay-get overlay 'history)
      (`((,previous-sexp ,beg) ,rest)
       (progn (goto-char beg)
              (kill-sexp)
              (insert previous-sexp)
              (overlay-put overlay 'history rest))))))

(defun incremental-eval-replace-next-sexp-with (new-form)
  "Replace the sexp which follows point in the current buffer with NEW-FORM."
  (let ((inhibit-read-only t))
    (incremental-eval-kill-sexp-to-overlay-stack)
    (insert (if (stringp new-form)
                (format "\"%s\"" new-form)
              (format "%s" new-form)))))

(defun incremental-eval-kill-sexp-to-overlay-stack ()
  "Kill the next sexp and record it on this overlays stack.

The old form is put onto the stack along with it's original
starting position so that it can be restored later by going
there, killing the form it evaled to and putting it there."
  (let ((overlay   (incremental-eval-get-overlay-at-point)))
    (pcase (incremental-eval-bounds-of-following-sexp)
      (`(,beg ,end)
       (progn
         (overlay-put overlay 'history
                      (cons (cons (buffer-substring-no-properties beg end) beg)
                            (overlay-get overlay 'history)))
         (delete-region beg end))))))

(defun incremental-eval-next-sexp ()
  "Produce the next sexp as a string."
  (pcase (incremental-eval-bounds-of-following-sexp)
    (`(,beg ,end)
     (buffer-substring-no-properties beg end))))

(defvar incremental-eval-next-form-is-a-macro-p '()
  "A function which should produce t if the next form is a macro.
Should be setup by a hook for the current mode.

See: `incremental-eval-engine'")

(defvar incremental-eval-next-form-is-a-branch-p '()
  "A function which should produce t if the next form is a conditional.
Should be setup by a hook for the current mode.

See: `incremental-eval-engine'")

(defvar incremental-eval-done-evaluating-p '()
  "A function which should produce t if we're done evaluating.
Should be setup by a hook for the current mode.

See: `incremental-eval-engine'")

(defun incremental-eval-iterate-sexp (p)
  "Iterate through the form at P by sexp, left to right, bottom to top.

Produces one of two values:
 - EVAL-NOW
 - CONTINUE
 - DONE

EVAL-NOW means that we've either reached a point where a form
must be evaluated in order to make a conditional choice, hit the
bottom of nested sexps and we need to start evaluating or we've
hit a macro and we need to expand it before we carry on.

The convention for EVAL-NOW is that the point will be placed just
before the sexp which needs to be evaluated.  i.e. if we were to
go `forward-sexp' then we would jump over the form which should be
evaluated.

A value of DONE is produced if there is no more evaluating to be
done."
  (progn
    (goto-char p)
    (incremental-eval-down-list-w/o-error)
    (cond
     ((funcall incremental-eval-next-form-is-a-macro-p)  'EVAL-NOW)
     ((funcall incremental-eval-next-form-is-a-branch-p) 'EVAL-NOW)
     ((incremental-eval-at-bottom-p)                     'EVAL-NOW)
     ((funcall incremental-eval-done-evaluating-p)       'DONE)
     (t                                                  'CONTINUE))))

(defun incremental-eval-at-bottom-p ()
  "Produce t if the point can't move forward list anymore."
  (save-excursion (= (point) (incremental-eval-down-list-w/o-error))))

(defun incremental-eval-down-list-w/o-error ()
  "Move down list without producing errors and report the new value of point."
  (condition-case nil
      (progn (down-list)
             (point))
    (error (point))))

(defun incremental-eval-find-region (p)
  "Find the region on which to start incremental eval around point P."
  (save-excursion
    (goto-char p)
    (cond
     ((region-active-p)            `(,(region-beginning) ,(region-end)))
     ((incremental-eval-in-sexp-p) (incremental-eval-bounds-of-top-level-form p))
     (t                            (incremental-eval-bounds-of-previous-sexp)))))

(defun incremental-eval-bounds-of-previous-sexp ()
  "Produce the bounds of the last sexp."
  (save-excursion
    (backward-sexp)
    (incremental-eval-bounds-of-following-sexp)))

(defun incremental-eval-bounds-of-following-sexp ()
  "Produce the bounds of the sexp following the current point."
  (save-excursion
    `(,(point) ,(progn (forward-sexp) (point)))))

(defun incremental-eval-bounds-of-top-level-form (p)
  "Produce the bounds of the top level form in which the point P is found.
P is defaulted to the current point when nil is supplied."
  (save-excursion
    (goto-char (if p p (point)))
    (incremental-eval-go-to-toplevel)
    (incremental-eval-bounds-of-following-sexp)))

(defun incremental-eval-go-to-toplevel ()
  "Move the point to the toplevel of the form which it's in."
  (while (/= (point)
             (save-excursion
               (incremental-eval-backwards-up-list-w/o-error)))
    (incremental-eval-backwards-up-list-w/o-error)))

(defun incremental-eval-backwards-up-list-w/o-error ()
  "Go backwards up list without errors.
Produce point when done.  Point will be the point before going
backwards up list if it couldn't go backwards up list."
  (condition-case nil
      (progn (backward-up-list)
             (point))
    (error (point))))

(defun incremental-eval-in-sexp-p ()
  "Produce t if the current point is in an sexp in the curent beffer."
  (not (null (save-excursion
               (ignore-errors
                 (backward-up-list)
                 (point))))))

(provide 'incremental-eval)
;;; incremental-eval ends here
