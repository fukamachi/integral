(in-package :cl-user)
(defpackage integral.type
  (:use :cl)
  (:import-from :integral.util
                :symbol-name-literally)
  (:import-from :alexandria
                :ensure-list
                :make-keyword
                :compose))
(in-package :integral.type)

(cl-syntax:use-syntax :annot)

@export
(deftype serial () 'integer)

@export
(deftype bigserial () 'bigint)

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

@export
(deftype datetime () 'integer)

@export
(deftype date () 'integer)

@export
(deftype timestamp (&optional length)
  (declare (ignore length))
  'integer)

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
    (let ((dbtype (make-keyword type)))
      (if args
          `(,dbtype ,@args)
          dbtype)))
  (:method ((type list) &rest args)
    (declare (ignore args))
    (apply #'cltype-to-dbtype type)))

(defun parse-type-vars (vars)
  (ppcre:split "(?:'|\")\\s*,\\s*(?:'|\")"
               (string-trim #(#\' #\") vars)))

@export
(defun string-to-dbtype (type)
  (let ((matches
          (nth-value 1
                     (ppcre:scan-to-strings "^([^(]+)(?:\\(([^)]+)\\))?(?:\\s+(.+))?$" type))))
    (unless matches
      (error ""))
    (let ((type (string-upcase (aref matches 0)))
          (vars (aref matches 1))
          (attrs (aref matches 2)))
      (let* ((dbtype (make-keyword type))
             (dbtype (or (type-alias dbtype)
                         dbtype))
             (vars (and vars
                        (or (ignore-errors (list (parse-integer vars)))
                            (parse-type-vars vars))))
             (attrs (and attrs
                         (mapcar (compose #'make-keyword #'string-upcase)
                                 (ppcre:split "\\s+" attrs)))))
        (cond
          ((or vars attrs)
           `(,dbtype
             ,@vars
             ,@attrs))
          (T dbtype))))))

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
    ((eq a :serial)
     (or (eq b :serial)
         (let ((b (or (type-alias b) b)))
           (or (eq b :integer)
               (eq b :bigint)))))
    ((eq b :serial)
     (let ((a (or (type-alias a) a)))
       (or (eq a :integer)
           (eq a :bigint))))
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

(define-column-type bigserial ()
  '(:bigint 20 :unsigned))

(define-column-type enum (&rest candidates)
  `(:enum ,@(mapcar #'(lambda (candidate)
                        (if (stringp candidate)
                            candidate
                            (symbol-name-literally candidate)))
                    candidates)))



