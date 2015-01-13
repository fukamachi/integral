(in-package :cl-user)
(defpackage integral.error
  (:use :cl))
(in-package :integral.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition <integral-error> (simple-error) ())

@export
(define-condition <connection-not-established-error> (<integral-error>)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream
             "No connection is established."))))

@export
(define-condition <unknown-primary-key-error> (<integral-error>)
  ((table-name :initarg :table-name :type string))
  (:report
   (lambda (condition stream)
     (format stream
             "Unknown primary key for table \"~A\"."
             (slot-value condition 'table-name)))))

@export
(define-condition <type-missing-error> (<integral-error>)
  ((slot-name :initarg :slot-name :type symbol))
  (:report
   (lambda (condition stream)
     (format stream
             "`:type' or `:col-type' is required for a slot ~A."
             (slot-value condition 'slot-name)))))

@export
(define-condition <migration-error> (<integral-error>) ())
