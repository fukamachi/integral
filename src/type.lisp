#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.type
  (:use :cl)
  (:import-from :integral.connection
                :database-type)
  (:import-from :integral.util
                :symbol-name-literally))
(in-package :integral.type)

(cl-syntax:use-syntax :annot)

@export
(deftype serial () 'integer)

@export
(deftype tinyint (&optional width)
  (declare (ignore width))
  'integer)

@export
(deftype smallint (&optional width)
  (declare (ignore width))
  'integer)

@export
(deftype mediumint (&optional width)
  (declare (ignore width))
  'integer)

@export
(deftype int (&optional width)
  (declare (ignore width))
  'integer)

@export
(deftype bigint (&optional width)
  (declare (ignore width))
  'integer)

@export
(deftype text () 'string)

@export
(deftype varchar (length)
  (declare (ignore length))
  'string)

@export
(deftype enum (&rest candidates)
  (declare (ignore candidates))
  'string)


;;
;; Conversion from CL data types to DB's.

(defgeneric column-type (type &rest args)
  (:method ((type t) &rest args)
    (if args
        `(,(intern (string type) :keyword) ,@args)
        type))
  (:method ((type list) &rest args)
    (declare (ignore args))
    (apply #'column-type type)))

@export
(defmacro define-column-type (type-symbol lambda-list &body body)
  (let ((type (gensym "TYPE"))
        (args (gensym "ARGS")))
    `(defmethod column-type ((,type (eql ',type-symbol)) &rest ,args)
       (declare (ignore ,type))
       (destructuring-bind ,lambda-list ,args
         ,@body))))

(define-column-type string (&optional length)
  (if length
      `(:varchar ,length)
      'text))

(define-column-type serial ()
  (if (eq (database-type) :postgres)
      'serial
      'integer))

(define-column-type enum (&rest candidates)
  `(:enum ,@(mapcar #'(lambda (candidate)
                        (if (stringp candidate)
                            candidate
                            (symbol-name-literally candidate)))
                    candidates)))
