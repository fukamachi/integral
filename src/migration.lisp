(in-package :cl-user)
(defpackage integral.migration
  (:use :cl)
  (:import-from :integral.connection
                :get-connection
                :database-type
                :retrieve-table-column-definitions-by-name
                :retrieve-table-indices
                :with-quote-char)
  (:import-from :integral.table
                :table-name
                :database-column-slots
                :<dao-table-class>
                :table-definition
                :table-class-indices)
  (:import-from :integral.database
                :*sql-log-stream*
                :execute-sql)
  (:import-from :integral.column
                :table-column-name
                :column-info-for-create-table
                :primary-key-p)
  (:import-from :integral.type
                :is-type-equal)
  (:import-from :integral.util
                :list-diff
                :symbol-name-literally
                :unlispify)
  (:import-from :dbi
                :with-transaction)
  (:import-from :sxql
                :make-statement
                :insert-into
                :select
                :from
                :alter-table
                :yield
                :add-column
                :modify-column
                :alter-column
                :drop-column
                :add-primary-key
                :drop-primary-key
                :create-index
                :drop-index
                :rename-to)
  (:import-from :alexandria
                :remove-from-plist))
(in-package :integral.migration)

(cl-syntax:use-syntax :annot)

@export
(defgeneric migrate-table (class)
  (:method ((class symbol))
    (migrate-table (find-class class)))
  (:method ((class <dao-table-class>))
    (let ((*sql-log-stream* (or *sql-log-stream* t)))
      (dbi:with-transaction (get-connection)
        (dolist (sql (make-migration-sql class :yield nil))
          (execute-sql sql))))))

@export
(defgeneric make-migration-sql (class &key yield)
  (:method ((class symbol) &key (yield t))
    (make-migration-sql (find-class class) :yield yield))
  (:method ((class <dao-table-class>) &key (yield t))
    (let ((sql-list (arranged-migration-sql class)))
      (if yield
          (mapcar #'yield sql-list)
          sql-list))))

(defun arranged-migration-sql (class)
  (if (eq (database-type) :sqlite3)
      (%generate-migration-sql-for-sqlite3 class)
      (destructuring-bind (add-column modify-column drop-column)
          (%generate-migration-sql-for-table-columns-others class)
        (destructuring-bind (add-index drop-primary-key drop-index)
            (generate-migration-sql-for-table-indices class)
          (append
           drop-index
           (and drop-column
                (list drop-column))
           (and drop-primary-key
                (list drop-primary-key))
           (and modify-column
                (list modify-column))
           (and add-column
                (list add-column))
           add-index)))))

(defun generate-migration-sql-for-table-indices (class)
  (when (eq (database-type) :sqlite3)
    (return-from generate-migration-sql-for-table-indices (values nil nil nil)))
  (let ((db-indices (retrieve-table-indices (get-connection)
                                            (table-name class)))
        (class-indices (table-class-indices class)))
    (multiple-value-bind (same new-keys old-keys)
        (list-diff class-indices db-indices
                   :sort-key-b (lambda (index)
                                 (princ-to-string (cdr index)))
                   :sort-key #'princ-to-string
                   :test #'(lambda (a b)
                             (equal a (cdr b))))
      (declare (ignore same))
      (multiple-value-bind (old-primary-key old-keys)
          (loop for old-key in old-keys
                when (getf (cdr old-key) :primary-key)
                  collect old-key into old-primary-key
                else
                  collect old-key into old-keys
                finally
                (return (values (car old-primary-key)
                                old-keys)))
        (list
         (remove
          nil
          (mapcar (lambda (new)
                    (let ((columns (mapcar (lambda (column)
                                             (intern (string (unlispify column)) :keyword))
                                           (getf new :columns))))
                      (if (getf new :primary-key)
                          (if (and (not (eq (database-type) :postgres))
                                   (null (cdr columns))
                                   (let ((primary-key (find-if #'primary-key-p (database-column-slots class))))
                                     (and primary-key
                                          (not (string= (symbol-name-literally (table-column-name primary-key))
                                                        (car columns))))))
                              (alter-table (intern (table-name class) :keyword)
                                (apply #'add-primary-key columns))

                              ;; Ignore this PRIMARY KEY because it must be already set in ADD/MODIFY COLUMN before.
                              nil)
                          (create-index (format nil "~{~A~^_and_~}" columns)
                                        :unique (getf new :unique-key)
                                        :on (list* (intern (table-name class) :keyword)
                                                   columns)))))
                  new-keys))
         (if (and old-primary-key
                  (find-if (lambda (slot)
                             (string=
                              (symbol-name-literally (table-column-name slot))
                              (car (getf (cdr old-primary-key) :columns))))
                           (database-column-slots class)))
             (alter-table (intern (table-name class) :keyword)
               (drop-primary-key))
             nil)
         (mapcar (lambda (old)
                   (apply #'drop-index (car old)
                          (if (eq (database-type) :mysql)
                              (list :on (intern (table-name class) :keyword))
                              nil)))
                 old-keys))))))

(defun compute-migrate-table-columns (class)
  (let ((column-definitions (retrieve-table-column-definitions-by-name
                             (get-connection)
                             (table-name class)))
        (slots (database-column-slots class)))
    (multiple-value-bind (same new old)
        (list-diff (mapcar
                    (lambda (slot)
                      (let ((info (column-info-for-create-table slot)))
                        (rplaca info (symbol-name (car info)))
                        info))
                    slots)
                   column-definitions
                   :sort-key #'car
                   :test (lambda (slot-def column-def)
                           (and (string= (car slot-def) (car column-def))
                                (or (is-type-equal (third slot-def) (third column-def))
                                    (and (eq (database-type) :mysql)
                                         (is-type-equal (third slot-def) :boolean)
                                         (is-type-equal (third column-def) '(:tinyint 1))))
                                (equal (cdddr slot-def) (cdddr column-def)))))
      (declare (ignore same))
      (multiple-value-bind (modify add drop)
          (list-diff new old :sort-key #'car :key #'car :test #'string=)
        (values add modify drop)))))

(defun %generate-migration-sql-for-table-columns-others (class)
  (let* ((column-definitions (retrieve-table-column-definitions-by-name
                              (get-connection)
                              (table-name class)))
         (db-primary-key (car (find-if (lambda (column)
                                         (getf (cdr column) :primary-key))
                                       column-definitions)))
         (slots (database-column-slots class)))
    (flet ((slot-column-position-arg (name)
             (let ((pos (position name slots
                                  :key (lambda (slot)
                                         (symbol-name-literally (table-column-name slot)))
                                  :test #'string=)))
               (cond
                 ((= pos 0) '(:first t))
                 ((null pos) nil)
                 (T (list :after
                          (intern (string (unlispify (table-column-name (nth (1- pos) slots))))
                                  :keyword)))))))
      (multiple-value-bind (new-columns modify-columns old-columns)
          (compute-migrate-table-columns class)
        ;; TODO: Return DROP TABLE and CREATE TABLE queries if all columns will be dropped.
        (list
         (if new-columns
             (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                    (mapcar (lambda (column)
                              (rplaca column (intern (string (unlispify (car column))) :keyword))
                              (apply #'add-column
                                     (append column
                                             (if (eq (database-type) :postgres)
                                                 nil
                                                 (slot-column-position-arg (car column))))))
                            new-columns))
             nil)
         (if modify-columns
             (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                    (mapcan (lambda (column)
                              (rplaca column (intern (string (unlispify (car column))) :keyword))
                              (cond
                                ((eq (database-type) :postgres)
                                 (list
                                  (alter-column (car column) :type (getf (cdr column) :type))
                                  (alter-column (car column) :not-null (getf (cdr column) :not-null))))
                                ;; Remove :primary-key if the column already has PRIMARY KEY.
                                ((and (getf (cdr column) :primary-key)
                                      (string= db-primary-key (car column)))
                                 (list
                                  (apply #'modify-column
                                         (car column)
                                         (remove-from-plist (cdr column) :primary-key))))
                                (T (list (apply #'modify-column column)))))
                            modify-columns))
             nil)
         (if old-columns
             (apply #'make-statement :alter-table (intern (table-name class) :keyword)
                    (mapcar (lambda (column)
                              (drop-column (intern (string (unlispify (car column))) :keyword)))
                            old-columns))
             nil))))))

(defun %generate-migration-sql-for-sqlite3 (class)
  (let ((column-definitions (retrieve-table-column-definitions-by-name
                             (get-connection)
                             (table-name class)))
        (orig-table-name (intern (table-name class) :keyword))
        (tmp-table-name (gensym (table-name class))))

    (list
     (alter-table orig-table-name
       (rename-to tmp-table-name))

     (table-definition class :yield nil)

     (let* ((column-names (mapcar (lambda (column)
                                    (intern (car column) :keyword))
                                  column-definitions))
            (slot-names (mapcar (lambda (slot)
                                  (intern (symbol-name
                                           (unlispify (table-column-name slot)))
                                          :keyword))
                                (database-column-slots class)))
            (same (list-diff column-names slot-names)))
       (insert-into orig-table-name same
         (select same
           (from tmp-table-name)))))))
