#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.connection.mysql
  (:use :cl)
  (:import-from :integral.type
                :string-to-dbtype)
  (:import-from :integral.util
                :group-by-plist-key)
  (:import-from :dbi
                :prepare
                :execute
                :fetch
                :connection-database-name))
(in-package :integral.connection.mysql)

(cl-syntax:use-syntax :annot)

@export
(defun last-insert-id (conn)
  (getf (dbi:fetch
         (dbi:execute
          (dbi:prepare conn "SELECT LAST_INSERT_ID() AS last_insert_id")))
        :|last_insert_id|))

@export
(defun column-definitions (conn table-name)
  ;; FIXME: quote
  (let* ((sql (format nil "SHOW FULL FIELDS FROM `~A`" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (loop for column = (dbi:fetch query)
          while column
          collect (list (getf column :|Field|)
                        :type (string-to-dbtype (getf column :|Type|))
                        :auto-increment (string= (getf column :|Extra|) "auto_increment")
                        :primary-key (string= (getf column :|Key|) "PRI")
                        :not-null (or (string= (getf column :|Key|) "PRI")
                                      (string= (getf column :|Null|) "NO"))))))

@export
(defun table-indices (conn table-name)
  (let ((query
          (dbi:prepare conn
                       (format nil "SELECT index_name, column_name, non_unique
                                 FROM information_schema.statistics
                                 WHERE table_schema = '~A'
                                   AND table_name = '~A'
                                 ORDER BY index_name, seq_in_index"
                               (connection-database-name conn)
                               table-name))))
    (mapcar #'(lambda (plist)
                (destructuring-bind (index-name &rest column-list) plist
                  (list index-name
                        :unique-key (or (string= index-name "PRIMARY")
                                        (= (getf (first column-list) :|non_unique|) 0))
                        :primary-key (string= index-name "PRIMARY")
                        :columns (mapcar #'(lambda (column)
                                             (getf column :|column_name|))
                                         column-list))))
            (group-by-plist-key
             (dbi:fetch-all (dbi:execute query))
             :key :|index_name|
             :test #'string=))))
