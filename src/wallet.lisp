(defpackage hermes-chains.wallet
  (:use #:cl
        #:ciel
        #:hu.dwim.def
        #:ironclad))
(in-package :hermes-chains.wallet)

(def (class ea) wallet ()
  ((private-key :initarg :private-key :reader private-key)
   (public-key :initarg :public-key :reader public-key)))

(def (function d) make-wallet ()
  (bind (((:values private public)
          (generate-key-pair :rsa :num-bits 512))
         (wallet (make-instance 'wallet :private-key (getf (destructure-private-key private) :d)
                                        :public-key (getf (destructure-public-key public) :e))))
    wallet))
