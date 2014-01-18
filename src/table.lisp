#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.table
  (:use :cl)
  (:import-from :integral.connection
                :get-connection
                :connected-p
                :database-type
                :with-quote-char
                :retrieve-table-column-definitions-by-name)
  (:import-from :integral.column
                :table-column-name
                :table-column-definition
                :auto-increment-p
                :primary-key-p
                :ghost-slot-p
                :column-info-for-create-table
                :slot-definition-to-plist)
  (:import-from :integral.database
                :*sql-log-stream*
                :execute-sql)
  (:import-from :integral.variable
                :*auto-migration-mode*)
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
                :drop-table
                :primary-key
                :unique-key
                :index-key
                :yield)
  (:import-from :alexandria
                :ensure-list))
(in-package :integral.table)

(cl-syntax:use-syntax :annot)

@export
(defclass <dao-class> () ()
  (:documentation "Base class of classes whose metaclass is <DAO-TABLE-CLASS>. The inheritance will be done implicitly.
If you want to use another class, specify it as a superclass in the usual way."))

@export
(defclass <dao-table-class> (standard-class)
  ((primary-key :initarg :primary-key)
   (unique-keys :initarg :unique-keys)
   (keys :type list
         :initarg :keys)
   (table-name :type (proper-list 'string)
               :initarg :table-name
               :initform nil)
   (generate-slots :type (proper-list 'boolean)
                   :initarg :generate-slots
                   :initform nil)
   (auto-pk :type (proper-list 'boolean)
            :initarg :auto-pk
            :initform '(t))
   (%initialized :type (proper-list 'boolean)
                 :initform nil
                 :accessor initializedp))
  (:documentation "Metaclass to define classes for your database-access objects as regular CLOS classes."))

(defparameter *oid-slot-definition*
  '(:name %oid :col-type serial :auto-increment t :primary-key t :readers (getoid)))

(defun initargs-contains-primary-key (initargs)
  (or (car (getf initargs :generate-slots))
      (getf initargs :primary-key)
      (find-if (lambda (slot)
                 (getf slot :primary-key))
               (getf initargs :direct-slots))))

(defmethod make-instance :before ((class <dao-table-class>) &key &allow-other-keys)
  (unless (and (slot-boundp class '%initialized)
               (initializedp class))
    (initialize-dao-table-class class)))

(defmethod initialize-instance :around ((class <dao-table-class>) &rest initargs &key direct-superclasses &allow-other-keys)
  (unless (or (car (getf initargs :generate-slots))
              (not (car (or (getf initargs :auto-pk)
                            '(t))))
              (initargs-contains-primary-key initargs))
    (push *oid-slot-definition* (getf initargs :direct-slots)))

  (unless (contains-class-or-subclasses '<dao-class> direct-superclasses)
    (setf (getf initargs :direct-superclasses)
          (cons (find-class '<dao-class>) direct-superclasses)))
  (apply #'call-next-method class initargs))

(defmethod initialize-instance :after ((class <dao-table-class>) &key)
  (let ((generate-slots (car (slot-value class 'generate-slots))))
    (unless generate-slots
      (setf (initializedp class) t))
    (when (and *auto-migration-mode*
               (not generate-slots)
               (connected-p))
      (funcall (symbol-function (intern #.(string :migrate-table) (find-package :integral.migration)))
               class))))

(defmethod reinitialize-instance :around ((class <dao-table-class>) &rest initargs)
  (let ((generate-slots (car (getf initargs :generate-slots))))
    (if (or generate-slots
            (not (car (or (getf initargs :auto-pk)
                          '(t))))
            (initargs-contains-primary-key initargs))
        (setf (getf initargs :direct-slots)
              (remove '%oid (getf initargs :direct-slots)
                      :key #'car
                      :test #'eq))
        (push *oid-slot-definition* (getf initargs :direct-slots)))
    (prog1
        (apply #'call-next-method class initargs)
      (when generate-slots
        (setf (initializedp class) nil))
      (when (and *auto-migration-mode*
                 (not generate-slots)
                 (connected-p))
        (funcall (symbol-function (intern #.(string :migrate-table) (find-package :integral.migration)))
                 class)))))

(defmethod c2mop:direct-slot-definition-class ((class <dao-table-class>) &key)
  'table-column-definition)

(defmethod c2mop:validate-superclass ((class <dao-table-class>) (super standard-class))
  t)

(defmethod c2mop:ensure-class-using-class :around ((class <dao-table-class>) name &rest keys &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses '<dao-class> direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class '<dao-class>) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defun contains-class-or-subclasses (class target-classes)
  (let ((class (if (typep class 'class)
                   class
                   (find-class class))))
    (find-if (lambda (target-class)
               (let ((target-class (if (typep target-class 'class)
                                       target-class
                                       (find-class target-class nil))))
                 (and target-class
                      (or (eq target-class class)
                          (class-inherit-p target-class class)))))
             target-classes)))


@export
(defgeneric table-name (class)
  (:documentation "Return the table name of `CLASS' as a string.")
  (:method ((class <dao-table-class>))
    (if (slot-value class 'table-name)
        (string (car (slot-value class 'table-name)))
        (symbol-name-literally (class-name class))))
  (:method ((obj <dao-class>))
    (table-name (class-of obj)))
  (:method ((class symbol))
    (table-name (find-class class))))

@export
(defgeneric table-primary-key (class)
  (:documentation "Return the primary key as a list.")
  (:method ((class <dao-table-class>))
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
  (:method ((class <dao-table-class>))
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
(defgeneric table-definition (class &key yield if-not-exists)
  (:method ((class symbol) &key (yield t) if-not-exists)
    (table-definition (find-class class) :yield yield :if-not-exists if-not-exists))
  (:method ((class <dao-table-class>) &key (yield t) if-not-exists)
    (with-quote-char
      (let* ((sqlite3-p (eq :sqlite3 (database-type)))
             (query (apply #'sxql:make-statement
                           :create-table
                           (list
                            (intern (table-name class) :keyword)
                            :if-not-exists if-not-exists)
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
                                nil)))))
        (if yield
            (yield query)
            query)))))

@export
(defun table-class-indices (class)
  (let* ((slots (database-column-slots class))
         (column-primary-slot (find-if #'primary-key-p slots)))
    (append
     (if column-primary-slot
         (list `(:unique-key t :primary-key t :columns (,(symbol-name-literally
                                                          (table-column-name column-primary-slot)))))
         nil)
     (if (slot-boundp class 'primary-key)
         (list (list :unique-key t
                     :primary-key t
                     :columns (mapcar #'symbol-name-literally
                                      (ensure-list (car (slot-value class 'primary-key))))))
         nil)
     (if (slot-boundp class 'unique-keys)
         (mapcar (lambda (key)
                   (list :unique-key t
                         :primary-key nil
                         :columns (mapcar #'symbol-name-literally (ensure-list key))))
                 (slot-value class 'unique-keys))
         nil)
     (if (and (slot-boundp class 'keys)
              (not (eq (database-type) :sqlite3)))
         (mapcar (lambda (key)
                   (list :unique-key nil
                         :primary-key nil
                         :columns (mapcar #'symbol-name-literally (ensure-list key))))
                 (slot-value class 'keys))
         nil))))

@export
(defgeneric database-column-slots (class)
  (:documentation "Same as C2MOP:CLASS-DIRECT-SLOTS except to remove ghost columns.")
  (:method ((class <dao-table-class>))
    (remove-if #'ghost-slot-p
               (c2mop:class-direct-slots class))))

@export
(defgeneric database-column-slot-names (class)
  (:method ((class <dao-table-class>))
    (mapcar #'c2mop:slot-definition-name
            (database-column-slots class))))

@export
(defgeneric ensure-table-exists (class)
  (:method ((class symbol))
    (ensure-table-exists (find-class class)))
  (:method ((class <dao-table-class>))
    (let ((*sql-log-stream* (or *sql-log-stream* t)))
      (execute-sql (table-definition class
                                     :yield nil
                                     :if-not-exists t)))))

@export
(defgeneric recreate-table (class)
  (:method ((class symbol))
    (recreate-table (find-class class)))
  (:method ((class <dao-table-class>))
    (let ((*sql-log-stream* (or *sql-log-stream* t)))
      (execute-sql (drop-table (intern (table-name class) :keyword)))
      (ensure-table-exists class))))

(defgeneric initialize-dao-table-class (class)
  (:method ((class <dao-table-class>))
    (when (initializedp class)
      (return-from initialize-dao-table-class))

    (let ((db-columns (retrieve-table-column-definitions-by-name
                       (get-connection)
                       (table-name class)))
          (package (symbol-package (class-name class))))
      (c2mop:ensure-class-using-class
       class
       (class-name class)
       :direct-superclasses (delete-duplicates
                             (mapcar #'class-name (c2mop:class-direct-superclasses class))
                             :test #'eq
                             :from-end t)
       :direct-slots
       (delete-duplicates
        (append
         (mapcar #'slot-definition-to-plist (c2mop:class-direct-slots class))
         (mapcar (lambda (column)
                   (destructuring-bind (name &key type not-null primary-key auto-increment &allow-other-keys) column
                     (let ((name-string (string-upcase name)))
                       (list :name (intern name-string package)
                             :col-type type
                             :initargs (list (intern name-string :keyword))
                             :not-null not-null
                             :auto-increment auto-increment
                             :primary-key primary-key))))
                 db-columns))
        :test #'eq
        :key #'cadr
        :from-end t)
       :metaclass (class-name (class-of class))
       :generate-slots '(t)))

    (setf (initializedp class) t)
    class))
