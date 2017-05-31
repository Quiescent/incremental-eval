;;; incremental-eval-interface --- an interface to the evaluation engine for incremental eval -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar incremental-eval-engine '()
  "The backend for eval'ing an sexp.

It should eval the next sexp, by convention.

This should be set for the desired mode by requiring a setup file
for that mode.  e.g. add (require 'incremental-eval-emacs-lisp)
to get the hook to set up incremental eval for Emacs Lisp.  Note
that the hook is automatically added by requiring that file.")

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

(provide 'incremental-eval-interface)
;;; incremental-eval-interface ends here
