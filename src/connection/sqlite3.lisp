#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.connection.sqlite3
  (:use :cl)
  (:import-from :dbi
                :prepare
                :execute
                :fetch))
(in-package :integral.connection.sqlite3)

(cl-syntax:use-syntax :annot)

@export
(defun last-insert-id (conn)
  (getf (dbi:fetch
         (dbi:execute
          (dbi:prepare conn "SELECT last_insert_rowid() AS last_insert_id")))
        :|last_insert_id|))

@export
(defun column-definitions (conn table-name &optional errorp)
  ;; FIXME: quote
  (let* ((sql (format nil "PRAGMA table_info(\"~A\")" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (or (loop for column = (dbi:fetch query)
              while column
              collect (let* ((type (getf column :|type|))
                             (pos (search "AUTO_INCREMENT" type :test #'string-equal)))
                        (list (getf column :|name|)
                              :type (if pos
                                        (subseq type 0 (1- pos))
                                        type)
                              :not-null (getf column :|notnull|)
                              :primary-key (= (getf column :|pk|) 0))))
        (error "Table \"~A\" doesn't exist." table-name))))
