(in-package :cl-user)
(defpackage integral.connection.sqlite3
  (:use :cl)
  (:import-from :integral.type
                :string-to-dbtype)
  (:import-from :dbi
                :prepare
                :execute
                :fetch)
  (:import-from :sxql
                :select
                :from
                :order-by
                :limit))
(in-package :integral.connection.sqlite3)

(cl-syntax:use-syntax :annot)

(defun table-info (conn table-name)
  (let* ((sql (format nil "PRAGMA table_info(\"~A\")" table-name)))
    (or (dbi:fetch-all (dbi:execute (dbi:prepare conn sql)))
        (error "Table \"~A\" doesn't exist." table-name))))

@export
(defun last-insert-id (conn table-name)
  (let ((primary-keys (table-primary-keys conn table-name)))
    (when (cdr primary-keys)
      (error "last-insert-id doesn't support composite primary keys."))
    (let ((primary-key (intern (car primary-keys) :keyword)))
      (getf (dbi:fetch
             (dbi:execute
              (dbi:prepare conn
                           (sxql:yield
                            (select ((:as primary-key :last_insert_id))
                              (from (intern table-name :keyword))
                              (order-by (:desc primary-key))
                              (limit 1))))))
            :|last_insert_id|
            0))))

@export
(defun column-definitions (conn table-name)
  ;; FIXME: quote
  (loop for column in (table-info conn table-name)
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
                                      (not (= (getf column :|notnull|) 0)))))))

(defun table-primary-keys (conn table-name)
  (mapcar #'(lambda (column) (getf column :|name|))
          (remove-if-not (lambda (column)
                           (= (getf column :|pk|) 1))
                         (table-info conn table-name))))

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
