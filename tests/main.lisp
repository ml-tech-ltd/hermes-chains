(defpackage hermes-chains/tests/main
  (:use :cl
        :hermes-chains
        :rove))
(in-package :hermes-chains/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hermes-chains)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
