;; (ql:quickload :hermes-chains)
(defpackage hermes-chains
  (:use #:cl
        #:ciel
        #:hu.dwim.def))
(in-package :hermes-chains)

(def (special-variable) *coin-name* "Hermes")

(defun interp (x &optional env)
  "Interpret (evaluate) the expression x in the environment env."
  (cond
    ((symbolp x) (get-var x env))
    ((atom x) x)
    ((scheme-macro (first x))
     (interp (scheme-macro-expand x) env))
    ((case (first x)
       (quote (second x))
       (begin (last1 (mapcar #'(lambda (y) (interp y env))
                             (rest x))))
       (set! (set-var! (second x) (interp (third x) env) env))
       (if (if (interp (second x) env)
               (interp (third x) env)
               (interp (fourth x) env)))
       (lambda (let ((parms (second x))
                     (code (maybe-add 'begin (cddr x))))
                 #'(lambda (&rest args)
                     (interp code (extend-env parms args env)))))
       (t ;; a procedure application
        (apply (interp (first x) env)
               (mapcar #'(lambda (v) (interp v env))
                       (rest x))))))))

(defun scheme-macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme-macro)))

(defmacro def-scheme-macro (name parmlist &body body)
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,parmlist .,body)))

(defun scheme-macro-expand (x)
  (if (and (listp x) (scheme-macro (first x)))
      (scheme-macro-expand
       (apply (scheme-macro (first x)) (rest x)))
      x))

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var! var val))
  val)

(defun get-var (var env)
  (if (assoc var env)
      (second (assoc var env))
      (get-global-var var)))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound scheme variable: ~A" var)
        val)))

(defun extend-env (vars vals env)
  "Add some variables and values to and environment."
  (nconc (mapcar #'list vars vals) env))

(def (special-variable) *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-scheme-interp ()
  (mapc #'init-scheme-proc *scheme-procs*)
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun init-scheme-proc (f)
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
      (set-global-var! f (symbol-function f))))

(defun maybe-add (op exps &optional if-nil)
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defun length=1 (x)
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  (first (last list)))

(defun scheme ()
  (init-scheme-interp)
  (loop (format t "~&==> ")
        (print (interp (read) nil))))

(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin . ,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(init-scheme-interp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; and there we have a scheme interpreter with macros. ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct blck
  (index 0) (timestamp 0) data (previous-hash "") hash)

(defstruct transaction
  from to (value 0) (accuracy 1)
  (duration 0)
  data hash previous-hash)

(defun to-byte-array (x)
  (let ((retval (make-array 0 :adjustable t
                              :fill-pointer t
                              :element-type '(unsigned-byte 8))))
    (map 'nil (lambda (c) (vector-push-extend (char-code c) retval))
         (format nil "~A" x)) ;
    (coerce retval 'ironclad::simple-octet-vector)))

(defun make-address (x)
  (let ((digester (ironclad:make-digest :sha3)))
    (ironclad:update-digest digester
                            (to-byte-array x))
    (ironclad:produce-digest digester)))

(defun hash-blck (blck)
  (let ((digester (ironclad:make-digest :sha3)))
    (ironclad:update-digest digester
                            (to-byte-array (blck-index blck)))
    (ironclad:update-digest digester
                            (to-byte-array (blck-timestamp blck)))
    (ironclad:update-digest digester
                            (to-byte-array (blck-data blck)))
    (ironclad:update-digest digester
                            (to-byte-array (blck-previous-hash blck)))
    (ironclad:produce-digest digester)))

(defun hash-transaction (blck)
  (let ((digester (ironclad:make-digest :sha3)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-from blck)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-to blck)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-value blck)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-accuracy blck)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-duration blck)))
    (ironclad:update-digest digester
                            (to-byte-array (transaction-data blck)))
    (ironclad:produce-digest digester)))

(defun make-genesis-blck (data time)
  (let* ((blck (make-blck
                 :index 0
                 :timestamp time
                 :data data
                 :hash 0))
         (hash (hash-blck blck)))
    (setf (blck-hash blck) hash)
    blck))

(defmacro create-genesis-blck (data)
  `(let ((time (get-universal-time)))
     (make-genesis-blck ,data time)))

(defun next-blck (last-blck data)
  (let ((blck (make-blck :index (1+ (blck-index last-blck))
                           :timestamp (get-universal-time)
                           :data data
                           :previous-hash (hash-blck last-blck))))
    (setf (blck-hash blck) (hash-blck blck))
    (push  blck *blckchain*)
    blck))

(setf *print-base* 16)

(def (special-variable) *base-code* '(set! x 0))

(def (special-variable) *network-address* (make-address *coin-name*))
(def (special-variable) *quester-address* (make-address "quester"))
(def (special-variable) *miner-address* (make-address "miner"))
(def (special-variable) *contract-address* (make-address "contract"))

(def (special-variable) *blck-transactions*
  (let ((transaction (make-transaction :from *network-address*
                                       :to *quester-address*
                                       :value (* 10000 10000 10000)
                                       :data *base-code*)))
    (setf (transaction-hash transaction)
          (hash-transaction transaction))
    (list transaction)))

(def (special-variable) *blckchain*
  (list (create-genesis-blck *blck-transactions*)))

(def (special-variable) *previous-blck* (car *blckchain*))

(def (special-variable) *solved-transactions* (make-hash-table :test #'equalp))
(eval-when (compile load)
  (defun new-transaction (&key from to (value 0) accuracy data
                            previous-hash duration)
    (let ((transaction (make-transaction :from from :to to :value value
                                         :accuracy accuracy :data data
                                         :previous-hash previous-hash
                                         :duration duration)))
      (setf (transaction-hash transaction)
            (hash-transaction transaction))
      (when previous-hash
        (setf (gethash
               (transaction-hash transaction)
               *solved-transactions*)
              t))
      transaction)))

(defmacro submit-answer (from transaction data)
  `(push (new-transaction :from ,from :to *contract-address*
                          :previous-hash  (transaction-hash transaction)
                          :data ,data)
         *blck-transactions*))

(defun has-transaction-not-been-solved (transaction)
  (if (gethash (transaction-hash transaction)
               *solved-transactions*)
      (not (setf (gethash (transaction-hash transaction)
                          *solved-transactions*)
                 transaction))
      t))

(defun viable-transaction (transaction)
  (and (has-transaction-not-been-solved transaction)
       (<= (blck-index (car *blckchain*))
           (or (transaction-duration transaction)
               (get-universal-time))))) ;; can still submit

(defun verify-transaction (transaction)
  (handler-case
      (interp (transaction-data transaction))
    (error (e) e)))

(defun execute-transactions (miner-address)
  (dolist (transaction *blck-transactions*)
    (when (viable-transaction transaction)
      (print :submitting-answer)
      (submit-answer miner-address transaction
                     (verify-transaction transaction))
      )))

(defun mine ()
  (when *blck-transactions*
    (execute-transactions *miner-address*)
    (transfer *network-address* *miner-address* 1)
    (setf *previous-blck*
          (next-blck *previous-blck* *blck-transactions*))
    (setf *blck-transactions* nil)))

(defmacro transfer (from to value)
  `(push (new-transaction :from ,from :to ,to
                          :value ,value)
         *blck-transactions*))

(defmacro execute (from value code &key (accuracy value)
                                     (duration (+ 2 (blck-index (car *blckchain*)))))
  `(push (new-transaction :from ,from :to *contract-address*
                          :value ,value
                          :accuracy ,accuracy :data ',code
                          :duration ,duration)
         *blck-transactions*))

(defun process-transfer-request (request stream)
  (destructuring-bind (from to value)
      request
    (transfer from to value)))

(defun process-execute-request (request stream)
  (destructuring-bind (from value data &key (accuracy value)
                                         (duration (+ 2 (blck-index (car *blckchain*)))))
      request
    (execute from value data :accuracy accuracy :duration duration)))

(defun process-blcks-request (request stream)
  (print *blckchain* stream))

(defun process-coin-server-request (stream)
  (let ((request (read stream)))
    (case request
      (transfer (process-transfer-request (cdr request) stream))
      (execute (process-execute-request (cdr request) stream))
      (blcks (process-blcks-request (cdr request) stream)))))

;; (defun coin-server (handle)
;;   (let ((stream (make-instance 'comm:socket-stream
;;                                :socket handle
;;                                :direction :io
;;                                :element-type
;;                                'base-char)))
;;     (process-coin-server-request stream)))

;; (defvar *server* (comm:start-up-server :function #'coin-server
;;                                        :service 9999
;;                                        :process-name
;;                                        (format nil "~A server" *coin-name*)))

;; (loop
;;   (mine)
;;   (sleep 1))
