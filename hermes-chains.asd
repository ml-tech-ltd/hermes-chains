(defsystem "hermes-chains"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "hermes-chains/tests"))))

(defsystem "hermes-chains/tests"
  :author ""
  :license ""
  :depends-on ("hermes-chains"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hermes-chains"
  :perform (test-op (op c) (symbol-call :rove :run c)))
