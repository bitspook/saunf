(asdf:load-system "saunf")
(ql:quickload "40ants-doc-full")
(in-package #:saunf)

(defun generate-docs ()
  (let ((base-dir (asdf:system-source-directory :saunf)))
    (uiop:with-current-directory (base-dir)
      (40ants-doc-full/builder:render-to-files
       @index
       :base-dir (path-join "./docs/")))))

(generate-docs)
