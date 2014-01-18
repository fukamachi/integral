#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

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
(defmethod retrieve-sql ((sql string) &rest bind)
  (dbi:fetch-all
   (apply #'dbi:execute (dbi:prepare (get-connection) sql)
          bind)))

@export
(defmethod retrieve-sql ((sql sql-statement) &rest bind)
  (declare (ignore bind))
  (multiple-value-bind (sql bind)
      (with-quote-char (sxql:yield sql))
    (apply #'retrieve-sql sql bind)))

@export
(defmethod execute-sql ((sql string) &rest bind)
  (apply #'dbi:do-sql (get-connection) sql bind))

@export
(defmethod execute-sql ((sql sql-statement) &rest bind)
  (declare (ignore bind))
  (multiple-value-bind (sql bind)
      (with-quote-char (sxql:yield sql))
    (apply #'execute-sql sql bind)))
