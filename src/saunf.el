;;; saunf --- Personal project manager -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Saunf is a personal-project manager. Its purpose is to help you exercise
;;; your personal freedom for project management, while allowing your team and
;;; teammate to do the same.
;;;
;;; Code:
(require 'sly)

(defun saunf--insert-org-link (url description)
  "Insert a new org-link to URL with DESCRIPTION."
  (insert (format "[[%s][%s]]" url description)))

(defun saunf--cl-eval (cl-code)
  "Eval common-lisp CL-CODE."
  (sly-eval
   `(cl:eval
     (cl:read-from-string
      ,(prin1-to-string cl-code)))))

(provide 'saunf)
;;; saunf.el ends here
