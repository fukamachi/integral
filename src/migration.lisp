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
                :database-column-slots)
  (:import-from :integral.column
                :table-column-name
                :column-info-for-create-table)
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
                :drop-column))
(in-package :integral.migration)

(cl-syntax:use-syntax :annot)

(defun compute-migrate-table-columns (class)
  (let ((column-definitions (retrieve-table-column-definitions-by-name
                             (get-connection)
                             (table-name class)))
        (slots (database-column-slots class)))
    (multiple-value-bind (same new old)
        (list-diff (mapcar (lambda (slot)
                             (symbol-name-literally (table-column-name slot)))
                           slots)
                   (mapcar #'car column-definitions))
      (declare (ignore same))
      (values new old))))

(defun generate-migration-sql (class)
  (let ((slots (database-column-slots class)))
    (multiple-value-bind (new-columns old-columns)
        (compute-migrate-table-columns class)
      (cons
       (apply #'make-statement :alter-table (intern (table-name class) :keyword)
              (mapcar (lambda (column)
                        (apply #'add-column
                               (column-info-for-create-table
                                (find-if
                                 (lambda (slot)
                                   (string= (symbol-name-literally (table-column-name slot))
                                            column))
                                 slots))))
                      new-columns))
       ;; SQLite3 doesn't support DROP COLUMN
       (if (eq (database-type) :sqlite3)
           nil
           (list
            (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                   (mapcar (lambda (column)
                             (drop-column (intern column :keyword))) old-columns))))))))

@export
(defun migrate-table-using-class (class)
  (let ((sql-list (generate-migration-sql class)))
    (dbi:with-transaction (get-connection)
      (dolist (sql sql-list)
        (multiple-value-bind (sql bind) (yield sql)
          (apply #'dbi:do-sql (get-connection) sql bind))))))
