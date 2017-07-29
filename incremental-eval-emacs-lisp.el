;;; incremental-eval-emacs-lisp --- setup hooks for incremental eval in Emacs Lisp mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path "."))

(require 'incremental-eval-interface)

(defvar incremental-eval-emacs-lisp-impacts '(message) "A list of symbols TOOD which you want to make a decision.")

(defun incremenatl-eval-setup-emacs-lisp ()
  "Setup Emacs Lisp mode for incremental eval mode."
  (interactive)
  (setq-local incremental-eval-engine                  #'incremental-eval-emacs-lisp)
  (setq-local incremental-eval-next-form-is-a-macro-p  #'incremental-eval-next-form-is-a-macro-emacs-lisp-p)
  (setq-local incremental-eval-next-form-is-a-branch-p #'incremental-eval-next-form-is-a-branch-emacs-lisp-p)
  (setq-local incremental-eval-done-evaluating-p       #'incremental-eval-done-evaluating-emacs-lisp-p))

(defun incremental-eval-emacs-lisp (string-expression)
  "Evauate STRING-EXPRESSION using read and eval."
  (if (incremental-eval--side-impact-p string-expression)
    (when (should-continue-prompt) (incremental-eval--eval string-expression))
    (incremental-eval--eval string-expression)))

(defun incremental-eval--eval (string-expression)
  "Read and evaluate STRING-EXPRESSION."
  (eval (read string-expression)))

(defun incremental-eval--should-continue-prompt ()
  (y-or-n-p (format "Do you want to eval: %s")))

(defun incremental-eval--side-impact-p (str)
  ;;  TODO
  t)

(defun incremental-eval-next-form-is-a-macro-emacs-lisp-p ()
  "Produce t if the next form is a macro in Emacs Lisp.

TODO!"
  nil)

(defun incremental-eval-next-form-is-a-branch-emacs-lisp-p ()
  "Produce t if the next form is a conditional in Emacs Lisp.

TODO!"
  nil)

(defun incremental-eval-done-evaluating-emacs-lisp-p ()
  "Produce t if we're don evaluating in Emcas Lisp.

TODO!"
  nil)

(add-hook 'emacs-lisp-mode-hook #'incremenatl-eval-setup-emacs-lisp)

(provide 'incremental-eval-emacs-lisp)
;;; incremental-eval-emacs-lisp ends here
