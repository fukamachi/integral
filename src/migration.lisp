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
                :retrieve-table-column-definitions-by-name)
  (:import-from :integral.table
                :table-name
                :database-column-slots
                :dao-table-class)
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
                :yield
                :add-column
                :modify-column
                :drop-column))
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

(defun generate-migration-sql (class)
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
          (if (eq (database-type) :sqlite3)
              (cerror "Ignore the column modification."
                      'migration-error
                      :format-control "Column modification wasdetected, but SQLite3 doesn't support MODIFY COLUMN.")
              (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                     (mapcan (lambda (column)
                               (rplaca column (intern (car column) :keyword))
                               (if (getf (cdr column) :primary-key)
                                   (cerror "Ignore the column modification."
                                           'migration-error
                                           :format-control "Modification of a primary key doesn't supported yet.")
                                   (list (apply #'modify-column column))))
                             modify-columns)))
          nil)
      (if (eq (database-type) :sqlite3)
          (cerror "Ignore the column modification."
                  'migration-error
                  :format-control "Column modification wasdetected, but SQLite3 doesn't support DROP COLUMN.")
          (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                 (mapcar (lambda (column)
                           (drop-column (intern (car column) :keyword))) old-columns)))))))

@export
(defun migrate-table-using-class (class)
  (let ((sql-list (generate-migration-sql class)))
    (dbi:with-transaction (get-connection)
      (dolist (sql sql-list)
        (multiple-value-bind (sql bind) (yield sql)
          (apply #'dbi:do-sql (get-connection) sql bind))))))

(defmethod initialize-instance :after ((class dao-table-class) &key)
  (when *auto-migrating-mode*
    (migrate-table-using-class class)))

(defmethod reinitialize-instance :after ((class dao-table-class) &key)
  (when *auto-migrating-mode*
    (migrate-table-using-class class)))
