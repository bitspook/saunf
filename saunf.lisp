(defpackage saunf
  (:use :cl :serapeum/bundle)
  (:export))
(in-package :saunf)

(defvar *claims* (make-hash-table))

(defmacro defclaim (name &body body)
  "Define NAME claim verified by BODY"
  `(setf
    (@ *claims* ,name)
    (lambda () ,@body)))

(defun claimant (name))
