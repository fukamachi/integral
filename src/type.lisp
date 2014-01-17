#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.type
  (:use :cl)
  (:import-from :integral.util
                :symbol-name-literally)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :ensure-list))
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

(defvar *type-alias-map* (make-hash-table :test 'eq))

@export
(defun type-alias (type)
  (nth-value 0
             (gethash type *type-alias-map*)))

@export
(defun (setf type-alias) (alias type)
  (setf (gethash type *type-alias-map*)
        alias))

(setf (type-alias :CHARACTER) :CHAR)
(setf (type-alias :CHARACTER\ VARYING) :VARCHAR)
(setf (type-alias :INT) :INTEGER)

;;
;; Conversion between CL data types and DB's.

@export
(defgeneric cltype-to-dbtype (type &rest args)
  (:method ((type t) &rest args)
    (let ((dbtype (intern (string type) :keyword)))
      (if args
          `(,dbtype ,@args)
          dbtype)))
  (:method ((type list) &rest args)
    (declare (ignore args))
    (apply #'cltype-to-dbtype type)))

@export
(defun string-to-dbtype (type)
  (setf type (string-upcase type))
  (let* ((open-paren-pos (position #\( type))
         (close-paren-pos (and open-paren-pos
                               (position #\) type :start (1+ open-paren-pos)))))
    (destructuring-bind (dbtype &rest args)
        (if open-paren-pos
            (remove
             nil
             (list* (subseq type 0 open-paren-pos)
                    (parse-integer (subseq type (1+ open-paren-pos) close-paren-pos))
                    (if (< (1+ close-paren-pos) (length type))
                        (mapcar (lambda (val) (intern val :keyword))
                                (split-sequence #\Space type :start (1+ close-paren-pos) :remove-empty-subseqs t))
                        nil)))
            (list type))
      (let* ((dbtype (intern dbtype :keyword))
             (dbtype (or (type-alias dbtype)
                         dbtype)))
        (if args
            (cons dbtype args)
            dbtype)))))

@export
(defun is-type-equal (a b)
  (cond
    ((and (listp a) (listp b))
     (and (is-type-equal (car a) (car b))
          (equal (cdr a) (cdr b))))
    ;; Check only its type. 'length' is ignored.
    ((or (listp a) (listp b))
     (is-type-equal (car (ensure-list a))
                    (car (ensure-list b))))
    (T (string= (cltype-to-dbtype (or (type-alias a) a))
                (cltype-to-dbtype (or (type-alias b) b))))))

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
      :text))

(define-column-type serial ()
  :integer)

(define-column-type enum (&rest candidates)
  `(:enum ,@(mapcar #'(lambda (candidate)
                        (if (stringp candidate)
                            candidate
                            (symbol-name-literally candidate)))
                    candidates)))
