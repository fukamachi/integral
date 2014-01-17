#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.migration
  (:use :cl)
  (:import-from :integral.connection
                :get-connection
                :database-type
                :retrieve-table-column-definitions-by-name
                :with-quote-char)
  (:import-from :integral.table
                :table-name
                :database-column-slots
                :dao-table-class
                :table-definition)
  (:import-from :integral.column
                :table-column-name
                :column-info-for-create-table)
  (:import-from :integral.type
                :is-type-equal)
  (:import-from :integral.error
                :migration-error)
  (:import-from :integral.util
                :list-diff
                :symbol-name-literally)
  (:import-from :dbi
                :with-transaction
                :do-sql)
  (:import-from :sxql
                :make-statement
                :insert-into
                :select
                :from
                :alter-table
                :yield
                :add-column
                :modify-column
                :alter-column
                :drop-column
                :rename-to))
(in-package :integral.migration)

(cl-syntax:use-syntax :annot)

@export
(defvar *auto-migrating-mode* nil
  "Whether use auto-migrating mode or not.")

(defun compute-migrate-table-columns (class)
  (let ((column-definitions (retrieve-table-column-definitions-by-name
                             (get-connection)
                             (table-name class)))
        (slots (database-column-slots class)))
    (multiple-value-bind (same new old)
        (list-diff (mapcar
                    (lambda (slot)
                      (let ((info (column-info-for-create-table slot)))
                        (rplaca info (symbol-name-literally (car info)))
                        info))
                    slots)
                   column-definitions
                   :sort-key #'car
                   :test (lambda (a b)
                           (and (string= (car a) (car b))
                                (is-type-equal (third a) (third b))
                                (equal (cdddr a) (cdddr b)))))
      (declare (ignore same))
      (multiple-value-bind (modify add drop)
          (list-diff new old :sort-key #'car :key #'car :test #'string=)
        (values add modify drop)))))

@export
(defun generate-migration-sql (class)
  (if (eq (database-type) :sqlite3)
      (generate-migration-sql-for-sqlite3 class)
      (generate-migration-sql-for-others class)))

(defun generate-migration-sql-for-others (class)
  (multiple-value-bind (new-columns modify-columns old-columns)
      (compute-migrate-table-columns class)
    (remove
     nil
     (list
      (if new-columns
          (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                 (mapcar (lambda (column)
                           (rplaca column (intern (car column) :keyword))
                           (apply #'add-column column))
                         new-columns))
          nil)
      (if modify-columns
          (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                 (mapcan (lambda (column)
                           (rplaca column (intern (car column) :keyword))
                           (if (getf (cdr column) :primary-key)
                               (cerror "Ignore the column modification."
                                       'migration-error
                                       :format-control "Modification of a primary key doesn't supported yet.")
                               (cond
                                 ((eq (database-type) :postgres)
                                  (list
                                   (alter-column (car column) :type (getf (cdr column) :type))
                                   (alter-column (car column) :not-null (getf (cdr column) :not-null))))
                                 (T (list (apply #'modify-column column))))))
                         modify-columns))
          nil)
      (if old-columns
          (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                 (mapcar (lambda (column)
                           (drop-column (intern (car column) :keyword))) old-columns))
          nil)))))

(defun generate-migration-sql-for-sqlite3 (class)
  (let ((column-definitions (retrieve-table-column-definitions-by-name
                             (get-connection)
                             (table-name class)))
        (orig-table-name (intern (table-name class) :keyword))
        (tmp-table-name (gensym (table-name class))))

    (list
     (alter-table orig-table-name
       (rename-to tmp-table-name))

     (table-definition class :yield nil)

     (insert-into orig-table-name (mapcar (lambda (slot)
                                            (intern (symbol-name-literally (table-column-name slot))
                                                    :keyword))
                                          (database-column-slots class))
       (select (mapcar (lambda (column)
                         (intern (string-upcase (car column)) :keyword))
                       column-definitions)
         (from tmp-table-name))))))

@export
(defun migrate-table-using-class (class)
  (let ((sql-list (generate-migration-sql class)))
    (dbi:with-transaction (get-connection)
      (dolist (sql sql-list)
        (multiple-value-bind (sql bind)
            (with-quote-char (yield sql))
          (apply #'dbi:do-sql (get-connection) sql bind))))))

#+nil
(defmethod initialize-instance :after ((class dao-table-class) &key)
  (when *auto-migrating-mode*
    (migrate-table-using-class class)))

#+nil
(defmethod reinitialize-instance :after ((class dao-table-class) &key)
  (when *auto-migrating-mode*
    (migrate-table-using-class class)))
