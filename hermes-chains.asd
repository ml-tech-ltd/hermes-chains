(ql-dist:enable (ql-dist:find-dist "ultralisp"))
(defsystem "hermes-chains"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:ciel
               :hu.dwim.def
               :ironclad
               :cl-intbytes)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "wallet")
                 (:file "block"))))
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
