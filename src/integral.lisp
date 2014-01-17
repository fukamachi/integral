#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral
  (:use :cl
        :sxql)
  (:import-from :integral.table
                :dao-table-class
                :dao-class
                :table-name
                :table-definition
                :table-serial-key
                :table-primary-key
                :database-column-slot-names)
  (:import-from :integral.error
                :integral-error
                :connection-not-established-error
                :unknown-primary-key-error
                :type-missing-error
                :migration-error)
  (:import-from :integral.type
                :serial
                :tinyint
                :smallint
                :mediumint
                :int
                :bigint
                :text
                :varchar
                :enum
                :datetime
                :date
                :timestamp)
  (:import-from :integral.connection
                :get-connection
                :connect-toplevel
                :disconnect-toplevel
                :last-insert-id
                :database-type
                :with-quote-char)
  (:import-from :integral.migration
                :migrate-table)
  (:import-from :integral.variable
                :*auto-migrating-mode*)
  (:import-from :integral.util
                :symbol-name-literally)
  (:import-from :dbi
                :prepare
                :execute
                :fetch
                :do-sql)
  (:import-from :sxql.sql-type
                :sql-statement)
  (:import-from :alexandria
                :when-let)
  (:export :connect-toplevel
           :disconnect-toplevel

           :dao-class
           :dao-table-class
           :table-name
           :table-definition

           :*auto-migrating-mode*
           :migrate-table

           ;; DataTypes
           :serial
           :tinyint
           :smallint
           :mediumint
           :int
           :bigint
           :text
           :varchar
           :enum
           :datetime
           :date
           :timestamp

           :define-column-type

           ;; Errors
           :integral-error
           :connection-not-established-error
           :unknown-primary-key-error
           :type-missing-error
           :migration-error

           ;; from SxQL
           :where
           :order-by
           :group-by
           :limit
           :offset))
(in-package :integral)

(cl-syntax:use-syntax :annot)

(defmethod make-insert-sql ((obj dao-class))
  (with-quote-char
    (yield
     (insert-into (intern (table-name obj) :keyword)
       (apply #'set=
              (mapcan
               #'(lambda (slot-name)
                   (if (slot-boundp obj slot-name)
                       (list (intern (symbol-name slot-name) :keyword)
                             (slot-value obj slot-name))
                       nil))
               (database-column-slot-names (class-of obj))))))))

@export
(defmethod insert-dao ((obj dao-class))
  (let ((serial-key (table-serial-key (class-of obj)))
        (sqlite3-p (eq :sqlite3 (database-type))))
    (labels ((get-pk-value ()
               (if serial-key
                   (last-insert-id (get-connection)
                                   (table-name obj)
                                   (string-downcase serial-key))
                   nil)))
      (when sqlite3-p
        (let ((pk-value (get-pk-value)))
          (when (integerp pk-value)
            (setf (slot-value obj serial-key) (1+ pk-value)))))
      (multiple-value-bind (sql bind) (make-insert-sql obj)
        (apply #'dbi:do-sql
               (get-connection)
               sql bind))
      (unless sqlite3-p
        (when-let (pk-value (get-pk-value))
          (setf (slot-value obj serial-key) pk-value)))
      obj)))

(defmethod make-update-sql ((obj dao-class))
  (let ((primary-key (table-primary-key (class-of obj))))
    (unless primary-key
      (error 'unknown-primary-key-error
             :table-name (table-name obj)))

    (with-quote-char
      (yield (update (intern (table-name obj) :keyword)
               (apply #'set=
                      (mapcan
                       #'(lambda (slot-name)
                           (if (slot-boundp obj slot-name)
                               (list (intern (symbol-name slot-name) :keyword)
                                     (slot-value obj slot-name))
                               nil))
                       (database-column-slot-names (class-of obj))))
               (where (if (cdr primary-key)
                          `(:and ,@(mapcar #'(lambda (key)
                                               `(:= ,key ,(slot-value obj key)))
                                           primary-key))
                          `(:= ,(car primary-key) ,(slot-value obj (car primary-key))))))))))

@export
(defmethod update-dao ((obj dao-class))
  (multiple-value-bind (sql bind) (make-update-sql obj)
    (apply #'dbi:do-sql
           (get-connection)
           sql bind)))

(defmethod make-delete-sql ((obj dao-class))
  (let ((primary-key (table-primary-key (class-of obj))))
    (unless primary-key
      (error 'unknown-primary-key-error
             :table-name (table-name obj)))

    (with-quote-char
      (yield (delete-from (intern (table-name obj) :keyword)
               (where (if (cdr primary-key)
                          `(:and ,@(mapcar #'(lambda (key)
                                               `(:= ,key ,(slot-value obj key)))
                                           primary-key))
                          `(:= ,(car primary-key) ,(slot-value obj (car primary-key))))))))))

@export
(defmethod delete-dao ((obj dao-class))
  (multiple-value-bind (sql bind) (make-delete-sql obj)
    (apply #'dbi:do-sql
           (get-connection)
           sql bind)))

(defun plist-to-dao (class plist)
  (let ((obj (make-instance class)))
    ;; Ignore columns which is not defined in defclass as a slot.
    (loop with undef = '#:undef
          for column-name in (database-column-slot-names class)
          for val = (getf plist (intern (symbol-name-literally column-name) :keyword)
                          undef)
          unless (eq val undef)
            do (setf (slot-value obj column-name) val))
    obj))

@export
(defmethod select-dao ((class dao-table-class) &rest expressions)
  (let ((select-sql (select :*
                      (from (intern (table-name class) :keyword)))))

    (dolist (ex expressions)
      (add-child select-sql ex))

    (multiple-value-bind (sql bind)
        (with-quote-char (yield select-sql))

      (let* ((query (dbi:prepare (get-connection) sql))
             (result (apply #'dbi:execute query bind)))
        (mapcar #'(lambda (plist)
                    (plist-to-dao class plist))
                (dbi:fetch-all result))))))

@export
(defmethod select-dao ((class symbol) &rest expressions)
  (apply #'select-dao (find-class class) expressions))

(defmethod make-find-sql ((class dao-table-class) &rest pk-values)
  (let ((primary-key (table-primary-key class)))
    (unless primary-key
      (error 'unknown-primary-key-error
             :table-name (table-name class)))

    (with-quote-char
      (yield
       (select :*
         (from (intern (table-name class) :keyword))
         (where (if (cdr primary-key)
                    `(:and ,@(mapcar #'(lambda (key val)
                                         `(:= ,key ,val))
                                     primary-key
                                     pk-values))
                    `(:= ,(car primary-key) ,(car pk-values))))
         (limit 1))))))

@export
(defmethod find-dao ((class dao-table-class) &rest pk-values)
  (multiple-value-bind (sql bind)
      (apply #'make-find-sql class pk-values)
    (let* ((query (dbi:prepare (get-connection) sql))
           (result (apply #'dbi:execute query bind)))
      (plist-to-dao class (dbi:fetch result)))))

@export
(defmethod find-dao ((class symbol) &rest pk-values)
  (apply #'find-dao (find-class class) pk-values))

@export
(defmethod create-dao ((class dao-table-class) &rest initargs)
  (let ((obj (apply #'make-instance class initargs)))
    (insert-dao obj)))

@export
(defmethod create-dao ((class symbol) &rest initargs)
  (apply #'create-dao (find-class class) initargs))

@export
(defmethod execute-sql ((sql string) &rest bind)
  (apply #'dbi:do-sql (get-connection) sql bind))

@export
(defmethod execute-sql ((sql sql-statement) &rest bind)
  (apply #'dbi:do-sql (get-connection) sql bind))
