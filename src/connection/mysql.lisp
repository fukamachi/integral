#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.connection.mysql
  (:use :cl)
  (:import-from :dbi
                :prepare
                :execute
                :fetch))
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
                        :type (getf column :|Type|)
                        :not-null (string= (getf column :|Null|) "NO")
                        :primary-key (string= (getf column :|Key|) "PRI")))))
