(defpackage saunf/tests
  (:use :cl :parachute)
  (:local-nicknames (#:s #:saunf)))

(in-package :saunf/tests)

(define-test "square"
  (is = (s:square 4) 16))
