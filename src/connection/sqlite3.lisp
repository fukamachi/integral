#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.connection.sqlite3
  (:use :cl)
  (:import-from :integral.type
                :string-to-dbtype)
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
(defun column-definitions (conn table-name)
  ;; FIXME: quote
  (let* ((sql (format nil "PRAGMA table_info(\"~A\")" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (or (loop for column = (dbi:fetch query)
              while column
              collect (let* ((type (getf column :|type|))
                             (pos (search "AUTO_INCREMENT" type :test #'string-equal)))
                        (list (getf column :|name|)
                              :type (string-to-dbtype
                                     (if pos
                                         (subseq type 0 (1- pos))
                                         type))
                              :auto-increment (not (null pos))
                              :primary-key (= (getf column :|pk|) 1)
                              :not-null (or (= (getf column :|pk|) 1)
                                            (not (= (getf column :|notnull|) 0))))))
        (error "Table \"~A\" doesn't exist." table-name))))

(defun table-primary-keys (conn table-name)
  (mapcar #'(lambda (column) (getf column :|name|))
          (remove-if-not (lambda (column)
                           (= (getf column :|pk|) 1))
                         (dbi:fetch-all
                          (dbi:execute
                           (dbi:prepare conn (format nil "PRAGMA table_info(~A)" table-name)))))))

@export
(defun table-indices (conn table-name)
  (let ((primary-keys (table-primary-keys conn table-name))
        (query (dbi:execute
                (dbi:prepare conn (format nil "PRAGMA index_list(~A)" table-name)))))
    (append
     (loop for index = (dbi:fetch query)
           while index
           collect
           (let* ((columns (mapcar
                            (lambda (info) (getf info :|name|))
                            (dbi:fetch-all
                             (dbi:execute (dbi:prepare conn (format nil "PRAGMA index_info('~A')"
                                                                    (getf index :|name|)))))))
                  (unique-key (= (getf index :|unique|) 1))
                  (primary-key (and unique-key
                                    primary-keys
                                    (equal columns primary-keys))))
             (when primary-key
               (setf primary-keys nil))
             (list (getf index :|name|)
                   :unique-key unique-key
                   :primary-key primary-key
                   :columns columns)))
     (if primary-keys
         (list (list "PRIMARY" :unique-key t :primary-key t :columns primary-keys))
         nil))))
