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
(deftype char (length)
  (declare (ignore length))
  'string)

@export
(deftype enum (&rest candidates)
  (declare (ignore candidates))
  'string)

(defvar *type-alias-map* (make-hash-table :test 'equal))

@export
(defun type-alias (type)
  (nth-value 0
             (gethash (string-upcase type) *type-alias-map*)))

@export
(defun (setf type-alias) (alias type)
  (setf (gethash (string-upcase type) *type-alias-map*)
        alias))

(setf (type-alias "CHARACTER") 'char)
(setf (type-alias "CHARACTER VARYING") 'varchar)

;;
;; Conversion between CL data types and DB's.

@export
(defgeneric cltype-to-dbtype (type &rest args)
  (:method ((type t) &rest args)
    (if args
        `(,(intern (string type) :keyword) ,@args)
        type))
  (:method ((type list) &rest args)
    (declare (ignore args))
    (apply #'cltype-to-dbtype type)))

@export
(defun dbtype-to-cltype (type &optional (package :integral.type))
  (declare (type string type))
  (let* ((open-paren-pos (position #\( type))
         (close-paren-pos (and open-paren-pos
                               (position #\) type :start (1+ open-paren-pos)))))
    (multiple-value-bind (cltype arg)
        (if open-paren-pos
            (values
             (string-upcase (subseq type 0 open-paren-pos))
             (parse-integer (subseq type (1+ open-paren-pos) close-paren-pos)))
            (string-upcase type))
      (let ((cltype (or (gethash cltype *type-alias-map*)
                        (intern cltype package))))
        (if arg
            (list cltype arg)
            cltype)))))

@export
(defmacro define-column-type (type-symbol lambda-list &body body)
  (let ((type (gensym "TYPE"))
        (args (gensym "ARGS")))
    `(defmethod cltype-to-dbtype ((,type (eql ',type-symbol)) &rest ,args)
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
