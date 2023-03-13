(defpackage saunf
  (:use #:cl #:serapeum/bundle #:40ants-doc)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:documentation "Personal software project manager."))

(in-package #:saunf)

(defsection @index (:title "Saunf")
  (saunf system)

  (@managing-issues section)

  "# API"
  (to-plist generic-function)

  (from-jira-response generic-function))

(export-always 'to-plist)
(defgeneric to-plist (obj)
  (:documentation "Convert OBJ to a plist.
Useful for serializing an object across RPC calls, e.g for accessing it in
Emacs."))

