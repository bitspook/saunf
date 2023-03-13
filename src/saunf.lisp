(defpackage saunf
  (:use #:cl #:serapeum/bundle #:40ants-doc)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:documentation "Personal software project manager."))

(in-package #:saunf)

(defsection @index (:title "Saunf")
  (saunf system)

  (@managing-issues section))
