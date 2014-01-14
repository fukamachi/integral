#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.table
  (:use :cl)
  (:import-from :integral.connection
                :get-connection
                :database-type
                :retrieve-table-column-definitions-by-name)
  (:import-from :integral.column
                :table-column-definition
                :auto-increment-p
                :primary-key-p
                :ghost-slot-p
                :column-info-for-create-table)
  (:import-from :integral.util
                :symbol-name-literally
                :class-inherit-p)
  (:import-from :closer-mop
                :validate-superclass
                :ensure-class-using-class
                :direct-slot-definition-class
                :slot-definition-name
                :class-direct-slots)
  (:import-from :sxql
                :make-statement
                :primary-key
                :unique-key
                :index-key
                :yield)
  (:import-from :alexandria
                :ensure-list))
(in-package :integral.table)

(cl-syntax:use-syntax :annot)

@export
(defclass dao-class () ()
  (:documentation "Base class of classes whose metaclass is DAO-TABLE-CLASS. The inheritance will be done implicitly.
If you want to use another class, specify it as a superclass in the usual way."))

@export
(defclass dao-table-class (standard-class)
  ((primary-key :initarg :primary-key)
   (unique-keys :initarg :unique-keys)
   (keys :type list
         :initarg :keys)
   (table-name :type (proper-list 'string)
               :initarg :table-name
               :initform nil))
  (:documentation "Metaclass to define classes for your database-access objects as regular CLOS classes."))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf initargs :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class initargs))

(defmethod c2mop:direct-slot-definition-class ((class dao-table-class) &key)
  'table-column-definition)

(defmethod c2mop:validate-superclass ((class dao-table-class) (super standard-class))
  t)

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defun contains-class-or-subclasses (class target-classes)
  (let ((class (find-class class)))
    (find-if (lambda (target-class)
               (let ((target-class (find-class target-class nil)))
                 (and target-class
                      (class-inherit-p target-class class))))
             target-classes)))


@export
(defgeneric table-name (class)
  (:documentation "Return the table name of `CLASS' as a string.")
  (:method ((class dao-table-class))
    (if (slot-value class 'table-name)
        (string (car (slot-value class 'table-name)))
        (symbol-name-literally (class-name class))))
  (:method ((obj dao-class))
    (table-name (class-of obj)))
  (:method ((class symbol))
    (table-name (find-class class))))

@export
(defgeneric table-primary-key (class)
  (:documentation "Return the primary key as a list.")
  (:method ((class dao-table-class))
    (if (slot-boundp class 'primary-key)
        (ensure-list (car (slot-value class 'primary-key)))
        (let ((slot (find-if
                     #'primary-key-p
                     (database-column-slots class))))
          (if slot
              (list (c2mop:slot-definition-name slot))
              nil)))))

@export
(defgeneric table-serial-key (class)
  (:documentation "Return the serial key as a symbol or NIL if there's no serial keys.")
  (:method ((class dao-table-class))
    (let* ((primary-key (table-primary-key class))
           (slot (find-if
                  #'(lambda (slot)
                      (and (auto-increment-p slot)
                           (member (c2mop:slot-definition-name slot)
                                   primary-key :test #'eq)))
                  (database-column-slots class))))
      (if slot
          (c2mop:slot-definition-name slot)
          nil))))

@export
(defgeneric table-definition (class)
  (:method ((class symbol))
    (table-definition (find-class class)))
  (:method ((class dao-table-class))
    (let ((sqlite3-p (eq :sqlite3 (database-type))))
      (yield
       (apply #'sxql:make-statement
              :create-table
              (intern (table-name class) :keyword)
              (mapcar
               #'column-info-for-create-table
               (database-column-slots class))
              (append
               (if (slot-boundp class 'primary-key)
                   (list (apply #'sxql:primary-key
                                (slot-value class 'primary-key)))
                   nil)
               (if (slot-boundp class 'unique-keys)
                   (mapcar #'sxql:unique-key (slot-value class 'unique-keys))
                   nil)
               (if (and (slot-boundp class 'keys)
                        (not sqlite3-p)) ;; ignoring :keys when using SQLite3
                   (mapcar #'sxql:index-key (slot-value class 'keys))
                   nil)))))))

@export
(defgeneric database-column-slots (class)
  (:documentation "Same as C2MOP:CLASS-DIRECT-SLOTS except to remove ghost columns.")
  (:method ((class dao-table-class))
    (remove-if #'ghost-slot-p
               (c2mop:class-direct-slots class))))

@export
(defgeneric database-column-slot-names (class)
  (:method ((class dao-table-class))
    (mapcar #'c2mop:slot-definition-name
            (database-column-slots class))))

@export
(defgeneric validate-table-column-definitions (class)
  (:method ((class dao-table-class))
    (let ((slot-names (mapcar #'symbol-name-literally (database-column-slot-names class)))
          (db-columns (retrieve-table-column-definitions-by-name
                       (get-connection)
                       (table-name class))))
      (loop for slot-name in slot-names
            unless (find slot-name db-columns :test #'string= :key #'car)
              collect slot-name into missing-slots
            finally
               (when missing-slots
                 (error "Class slots ~A of ~A are missing in database table '~A'."
                        missing-slots
                        (class-name class)
                        (table-name class)))))))
