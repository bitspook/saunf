;;; saunf --- Personal project manager -*- lexical-binding: t; -*-
;;;
;; Author: bitspook
;; Homepage: https://github.com/bitspook/saunf
;; Keywords: project-management
;; Version: 0.0.1
;; Package-Requires: ((sly "1.0.43"))
;;;
;;; Commentary:
;;; Saunf is a personal-project manager. Its purpose is to help you exercise
;;; your personal freedom for project management, while allowing your team and
;;; teammate to do the same.
;;;
;;; Code:
(require 'sly)

(defun saunf--cl-eval (cl-code)
  "Eval common-lisp CL-CODE."
  (sly-eval
   `(cl:progn
     (cl:when
      (cl:not (cl:find-package :saunf))
      (asdf:load-system "saunf"))
     (cl:let ((cl:*package* (cl:find-package :saunf)))
      (cl:eval
       (cl:read-from-string
        ,(prin1-to-string cl-code)))))))

(defvar saunf-projects '()
  "List of active projects. An active project is a symbol representing a live saunf-project in CL.")

(message "SAUNF IS LOADING")

(provide 'saunf)
;;; saunf.el ends here
