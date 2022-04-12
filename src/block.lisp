(defpackage hermes-chains.block
  (:use #:cl
        #:ciel
        #:hu.dwim.def))
(in-package :hermes-chains.block)

(deftype bytea () '(vector (unsigned-byte 8)))

(def (class ea) <block> ()
  ((current-hash :initarg :current-hash :reader current-hash)
   (previous-hash :initarg :previous-hash :reader previous-hash)
   (data :initarg :data :reader data)
   (nonce :initarg :nonce :reader nonce)
   (timestamp :initarg :timestamp :reader timestamp)))

(def (class ea) <transaction> ()
  ((signature :initarg :signature :reader signature)
   (from :initarg :from :reader from)
   (to :initarg :to :reader to)
   (value :initarg :value :reader value)))

(def (generic) hash (obj))

(def (method) hash ((block <block>))
  (bind ((hash (sha256
                (concatenate 'bytea
                             (previous-hash block)
                             (int->octets (timestamp block))
                             (data block)
                             (int->octets (nonce block))))))
    hash))

(--> sha256 (bytea) bytea)
(def (function d) sha256 (data)
  (bind ((digester (ironclad:make-digest :sha256))
         (digest (ironclad:digest-sequence digester data)))
    digest))

(--> int->octets (integer) bytea)
(def (function d) int->octets (number)
  (bind ((octets (cl-intbytes:int->octets number (1+ (floor (log number 256))))))
    octets))

(--> make-block (bytea bytea bytea integer integer) <block>)
(def (function d) make-block (current-hash previous-hash data nonce timestamp)
  (bind ((hash (make-instance '<block>
                              :current-hash current-hash
                              :previous-hash previous-hash
                              :data data
                              :nonce nonce
                              :timestamp timestamp)))
    hash))

(--> make-transaction (bytea bytea bytea bytea) <block>)
(def (function d) make-transaction (signature from to value)
  )
