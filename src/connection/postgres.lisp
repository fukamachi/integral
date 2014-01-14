#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.connection.postgres
  (:use :cl)
  (:import-from :dbi
                :prepare
                :execute
                :fetch
                :<dbi-programming-error>))
(in-package :integral.connection.postgres)

(cl-syntax:use-syntax :annot)

@export
(defun last-insert-id (conn table-name serial-key-name &key sequence-name)
  (let* ((sequence-name (or sequence-name
                            (default-sequence-name conn table-name serial-key-name)))
         (sql (format nil "SELECT currval('~A')" sequence-name)))
    (handler-case
        (getf (dbi:fetch (dbi:execute (dbi:prepare conn sql)))
              :|currval|)
      (error (e)
        (if (eq (type-of e)
                (intern #.(string :object-state-error)
                        :cl-postgres-error))
            0
            (error e))))))

@export
(defun column-definitions (conn table-name)
  (let* ((sql (format nil "SELECT
                              f.attname AS name,
                              pg_catalog.format_type(f.atttypid,f.atttypmod) AS type,
                              f.attnotnull AS notnull,
                              CASE
                                  WHEN p.contype = 'p' THEN 't'
                                  ELSE 'f'
                              END AS primary,
                          FROM pg_attribute f
                              JOIN pg_class c ON c.oid = f.attrelid
                              LEFT JOIN pg_constraint p ON p.conrelid = f.attrelid AND f.attnum = ANY (p.conkey)
                          WHERE c.relkind = 'r'::char
                              AND c.relname = '~A'
                              AND f.attnum > 0
                          ORDER BY f.attnum" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (loop for column = (dbi:fetch query)
          while column
          collect (list (getf column :|name|)
                        :type (getf column :|format_type|)
                        :not-null (string= (getf column :|notnull|) "t")
                        :primary-key (string= (getf column :|primary|) "t"))))

  )

(defvar *sequence-name-cache* (make-hash-table :test 'equal))

(defun default-sequence-name (conn table-name serial-key-name)
  (let ((cache (gethash table-name *sequence-name-cache*)))
    (when (gethash serial-key-name cache)
      (return-from default-sequence-name
        (gethash serial-key-name cache))))

  (let* ((sql (format nil "SELECT pg_get_serial_sequence('~A', '~A')"
                      table-name serial-key-name))
         (sequence-name
           (handler-case (getf (dbi:fetch
                                (dbi:execute
                                 (dbi:prepare conn sql)))
                               :|pg_get_serial_sequence|)
             (<dbi-programming-error> ()
               (format nil "~A_~A_seq" table-name serial-key-name)))))
    (symbol-macrolet ((cache (gethash table-name *sequence-name-cache*)))
      (unless cache
        (setf cache (make-hash-table :test 'equal)))
      (setf (gethash serial-key-name cache) sequence-name))

    sequence-name))
