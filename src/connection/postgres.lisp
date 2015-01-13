(in-package :cl-user)
(defpackage integral.connection.postgres
  (:use :cl)
  (:import-from :integral.type
                :string-to-dbtype)
  (:import-from :integral.util
                :group-by-plist-key)
  (:import-from :dbi
                :prepare
                :execute
                :fetch
                :<dbi-programming-error>)
  (:import-from :split-sequence
                :split-sequence))
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

(defun get-serial-keys (conn table-name)
  (let ((query
          (dbi:execute
           (dbi:prepare conn
                        (format nil "SELECT relname FROM pg_class WHERE relkind = 'S' AND relname LIKE '~A_%'"
                                table-name)))))
    (loop for row = (dbi:fetch query)
          while row
          collect (second
                   (split-sequence #\_ (getf row :|relname|)
                                   :count 2)))))

@export
(defun column-definitions (conn table-name)
  (let* ((serial-keys (get-serial-keys conn table-name))
         (sql (format nil "SELECT~
                        ~%    f.attname AS name,~
                        ~%    pg_catalog.format_type(f.atttypid,f.atttypmod) AS type,~
                        ~%    f.attnotnull AS notnull,~
                        ~%    CASE~
                        ~%        WHEN p.contype = 'p' THEN true~
                        ~%        ELSE false~
                        ~%    END AS primary~
                        ~%FROM pg_attribute f~
                        ~%    JOIN pg_class c ON c.oid = f.attrelid~
                        ~%    LEFT JOIN pg_constraint p ON p.conrelid = f.attrelid AND f.attnum = ANY (p.conkey)~
                        ~%WHERE c.relkind = 'r'::char~
                        ~%    AND c.relname = '~A'~
                        ~%    AND f.attnum > 0~
                        ~%    AND f.atttypid != 0~
                        ~%ORDER BY f.attnum, p.contype" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (delete-duplicates
     (loop for column = (dbi:fetch query)
           while column
           collect (list (getf column :|name|)
                         :type (string-to-dbtype (getf column :|type|))
                         :auto-increment (not (null (member (getf column :|name|)
                                                            serial-keys
                                                            :test #'string=)))
                         :primary-key (getf column :|primary|)
                         :not-null (or (getf column :|primary|)
                                       (getf column :|notnull|))))
     :key #'car
     :test #'string=
     :from-end t)))

@export
(defun table-indices (conn table-name)
  (let ((columns (mapcar #'car (column-definitions conn table-name)))
        (query (dbi:execute (dbi:prepare conn
                                         (format nil
                                                 "SELECT~
                                                ~%    i.relname AS index_name,~
                                                ~%    a.attname AS column_name,~
                                                ~%    ix.indisunique AS is_unique,~
                                                ~%    ix.indisprimary AS is_primary~
                                                ~%FROM~
                                                ~%    pg_class t,~
                                                ~%    pg_class i,~
                                                ~%    pg_index ix,~
                                                ~%    pg_attribute a~
                                                ~%WHERE~
                                                ~%    t.oid = ix.indrelid~
                                                ~%    and i.oid = ix.indexrelid~
                                                ~%    and a.attrelid = t.oid~
                                                ~%    and a.attnum = ANY(ix.indkey)~
                                                ~%    and t.relkind = 'r'~
                                                ~%    and t.relname LIKE '~A'~
                                                ~%ORDER BY i.relname" table-name)))))
    (mapcar #'(lambda (plist)
                (destructuring-bind (index-name &rest column-list) plist
                  (list index-name
                        :unique-key (getf (first column-list) :|is_unique|)
                        :primary-key (getf (first column-list) :|is_primary|)
                        :columns (sort (mapcar #'(lambda (column)
                                                   (getf column :|column_name|))
                                               column-list)
                                       (lambda (a b)
                                           (< (position a columns :test #'string=)
                                              (position b columns :test #'string=)))))))
            (group-by-plist-key (dbi:fetch-all query)
                                :key :|index_name|
                                :test #'string=))))

(defvar *sequence-name-cache* (make-hash-table :test 'equal))

(defun default-sequence-name (conn table-name serial-key-name)
  (let ((cache (gethash table-name *sequence-name-cache*)))
    (when (and cache
               (gethash serial-key-name cache))
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
