#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.column
  (:use :cl
        :annot.class)
  (:import-from :integral.connection
                :database-type)
  (:import-from :integral.type
                :cltype-to-dbtype
                :serial)
  (:import-from :integral.error
                :type-missing-error)
  (:import-from :closer-mop
                :standard-direct-slot-definition
                :standard-slot-definition
                :slot-definition-name
                :slot-definition-type
                :slot-definition-initform
                :slot-definition-initfunction
                :slot-definition-allocation
                :slot-definition-initargs
                :slot-definition-readers
                :slot-definition-writers))
(in-package :integral.column)

(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass table-column-definition (standard-direct-slot-definition)
  ((col-type :type (or symbol cons)
             :initarg :col-type)
   (primary-key :type boolean
                :initarg :primary-key
                :initform nil
                :accessor primary-key-p)
   (auto-increment :type boolean
                   :initarg :auto-increment
                   :initform nil
                   :accessor auto-increment-p)
   (not-null :type boolean
             :initarg :not-null
             :initform nil)
   (ghost :type boolean
          :initarg :ghost
          :initform nil
          :accessor ghost-slot-p
          :documentation "Option to specify slots as ghost slots. Ghost slots do not depend on a database.")))

(defmethod initialize-instance :after ((column table-column-definition) &key)
  (when (eq (table-column-type column) 'serial)
    (setf (auto-increment-p column) t))
  (when (primary-key-p column)
    (setf (slot-value column 'not-null) t)))

(defgeneric table-column-name (column)
  (:method ((column table-column-definition))
    (c2mop:slot-definition-name column)))

(defgeneric table-column-type (column)
  (:method ((column table-column-definition))
    (if (slot-boundp column 'col-type)
        (slot-value column 'col-type)
        (c2mop:slot-definition-type column))))

@export
(defgeneric column-info-for-create-table (column)
  (:method ((column table-column-definition))
    (with-slots (auto-increment not-null)
        column
      (let ((col-type (table-column-type column)))
        (when (eq col-type t)
          (error 'type-missing-error
                 :slot-name (c2mop:slot-definition-name column)))

        `(,(table-column-name column)
          :type ,(cltype-to-dbtype col-type)
          :auto-increment ,(case (database-type)
                             (:postgres nil)
                             (:sqlite3 nil)
                             (:mysql (or (eq col-type 'serial)
                                         auto-increment)))
          :primary-key ,(primary-key-p column)
          :not-null ,not-null)))))

@export
(defgeneric slot-definition-to-plist (slot)
  (:method ((slot standard-slot-definition))
    (list :name (slot-definition-name slot)
          :initform (slot-definition-initform slot)
          :initfunction (slot-definition-initfunction slot)
          :type (slot-definition-type slot)
          :allocation (slot-definition-allocation slot)
          :initargs (slot-definition-initargs slot)
          :readers (slot-definition-readers slot)
          :writers (slot-definition-writers slot))))
