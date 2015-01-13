(in-package :cl-user)
(defpackage integral.database
  (:use :cl)
  (:import-from :integral.connection
                :get-connection
                :with-quote-char)
  (:import-from :dbi
                :prepare
                :execute
                :fetch-all
                :do-sql)
  (:import-from :sxql
                :yield)
  (:import-from :sxql.sql-type
                :sql-statement))
(in-package :integral.database)

(cl-syntax:use-syntax :annot)

@export
(defvar *sql-log-stream* nil)

@export
(defmethod retrieve-by-raw-sql ((sql string) &optional binds)
  (dbi:fetch-all
   (apply #'dbi:execute (dbi:prepare (get-connection) sql)
          binds)))

@export
(defmethod retrieve-by-raw-sql ((sql sql-statement) &optional binds)
  (declare (ignore binds))
  (multiple-value-bind (sql binds)
      (with-quote-char (sxql:yield sql))
    (retrieve-by-raw-sql sql binds)))

@export
(defmethod execute-sql ((sql string) &optional binds)
  (format *sql-log-stream* "~&~A;~%" sql)
  (apply #'dbi:do-sql (get-connection) sql binds))

@export
(defmethod execute-sql ((sql sql-statement) &optional binds)
  (declare (ignore binds))
  (multiple-value-bind (sql binds)
      (with-quote-char (sxql:yield sql))
    (execute-sql sql binds)))
