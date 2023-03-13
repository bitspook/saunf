(defsystem "saunf"
  :version "0.1.0"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (#:serapeum
               #:40ants-doc
               #:com.inuoe.jzon
               #:dexador)
  :components ((:module "src"
                :components
                ((:file "saunf")
                 (:file "issues"))))
  :description "Personal software project manager."
  :in-order-to ((test-op (test-op :saunf/tests))))

(defsystem "saunf/tests"
  :author "Charanjit Singh"
  :license "AGPL-3.0-only"
  :depends-on (:saunf :parachute)
  :components ((:module "tests"
                :components
                ((:file "saunf"))))
  :description "Test system for saunf"
  :perform (test-op (op c) (symbol-call :parachute :test :saunf/tests)))
