(in-package :cl-user)
(defpackage integral
  (:use :cl
        :sxql
        :iterate)
  (:import-from :integral.validation
                :validate-presence-of
                :validate-length-of
                :validate-format-of
                :valid-p)
  (:import-from :integral.table
                :<dao-table-class>
                :<dao-class>
                :table-name
                :table-definition
                :table-serial-key
                :table-primary-key
                :database-column-slot-names
                :ensure-table-exists
                :recreate-table
                :inflate
                :deflate
                :type-inflate
                :type-deflate
                :initialize-dao-table-class
                :dao-synced
                :make-dao-instance
                :generate-defclass)
  (:import-from :integral.database
                :retrieve-by-raw-sql
                :execute-sql
                :*sql-log-stream*)
  (:import-from :integral.error
                :<integral-error>
                :<connection-not-established-error>
                :<unknown-primary-key-error>
                :<type-missing-error>
                :<migration-error>)
  (:import-from :integral.type
                :serial
                :bigserial
                :tinyint
                :smallint
                :mediumint
                :bigint
                :text
                :varchar
                :enum
                :datetime
                :date
                :timestamp)
  (:import-from :integral.connection
                :*db*
                :make-connection
                :get-connection
                :connect-toplevel
                :disconnect-toplevel
                :last-insert-id
                :database-type
                :with-quote-char)
  (:import-from :integral.migration
                :migrate-table
                :make-migration-sql)
  (:import-from :integral.variable
                :*auto-migration-mode*)
  (:import-from :integral.util
                :lispify
                :unlispify)
  (:import-from :sxql.sql-type
                :sql-statement)
  (:import-from :alexandria
                :when-let)
  (:export :connect-toplevel
           :disconnect-toplevel
           :*db*
           :make-connection
           :get-connection

           :<dao-class>
           :<dao-table-class>
           :generate-defclass

           :inflate
           :deflate
           :type-inflate
           :type-deflate
           :table-name
           :table-definition
           :ensure-table-exists
           :recreate-table

           :retrieve-by-sql
           :execute-sql
           :*sql-log-stream*

           :*auto-migration-mode*
           :migrate-table
           :make-migration-sql

           ;; DataTypes
           :serial
           :bigserial
           :tinyint
           :smallint
           :mediumint
           :bigint
           :text
           :varchar
           :enum
           :datetime
           :date
           :timestamp

           ;;validation
           :validate-presence-of
           :validate-uniqueness-of
           :validate-length-of
           :validate-format-of
           ;; Errors
           :<integral-error>
           :<connection-not-established-error>
           :<unknown-primary-key-error>
           :<type-missing-error>
           :<migration-error>

           ;; from SxQL
           :where
           :order-by
           :group-by
           :limit
           :offset))
(in-package :integral)

(cl-syntax:use-syntax :annot)

(defmethod make-insert-sql ((obj <dao-class>))
  (insert-into (intern (table-name obj) :keyword)
      (make-set-clause obj)))

(defun make-set-clause (obj)
  (apply #'set=
         (mapcan
          #'(lambda (slot-name)
              (if (slot-boundp obj slot-name)
                  (let ((value (slot-value obj slot-name)))
                    (list (intern (symbol-name (unlispify slot-name)) :keyword)
                          (deflate obj slot-name value)))
                  nil))
          (database-column-slot-names (class-of obj)))))

@export
(defmethod insert-dao ((obj <dao-class>))
  (let ((serial-key (table-serial-key (class-of obj)))
        (sqlite3-p (eq :sqlite3 (database-type))))
    (labels ((get-pk-value ()
               (if serial-key
                   (last-insert-id (get-connection)
                                   (table-name obj)
                                   (symbol-name (unlispify serial-key)))
                   nil)))
      (when sqlite3-p
        (let ((pk-value (get-pk-value)))
          (when (integerp pk-value)
            (setf (slot-value obj serial-key) (1+ pk-value)))))
      (execute-sql (make-insert-sql obj))
      (unless sqlite3-p
        (when-let (pk-value (get-pk-value))
                  (setf (slot-value obj serial-key) pk-value)))
      (setf (dao-synced obj) T)
      obj)))

(defmethod insert-dao :around ((obj <dao-class>))
  (when (valid-p obj)
    (call-next-method)))

(defmethod make-update-sql ((obj <dao-class>))
  (let ((primary-key (table-primary-key (class-of obj))))
    (unless primary-key
      (error '<unknown-primary-key-error>
             :table-name (table-name obj)))

    (update (intern (table-name obj) :keyword)
      (make-set-clause obj)
      (where
       `(:and ,@(mapcar #'(lambda (key)
                            `(:= ,(unlispify key) ,(slot-value obj key)))
                        primary-key))))))

@export
(defmethod update-dao ((obj <dao-class>))
  (execute-sql (make-update-sql obj)))

(defmethod update-dao :around ((obj <dao-class>))
  (when (valid-p obj)
    (call-next-method)))

(defmethod make-delete-sql ((obj <dao-class>))
  (let ((primary-key (table-primary-key (class-of obj))))
    (unless primary-key
      (error '<unknown-primary-key-error>
             :table-name (table-name obj)))

    (delete-from (intern (table-name obj) :keyword)
      (where `(:and ,@(mapcar #'(lambda (key)
                                  `(:= ,(unlispify key) ,(slot-value obj key)))
                              primary-key))))))

@export
(defmethod delete-dao ((obj <dao-class>))
  (prog1
      (execute-sql (make-delete-sql obj))
    (setf (dao-synced obj) nil)))

@export
(defmethod select-dao ((class <dao-table-class>) &rest expressions)
  (let ((select-sql (select :*
                      (from (intern (table-name class) :keyword)))))

    (dolist (ex expressions)
      (add-child select-sql ex))

    (retrieve-by-sql select-sql :as class)))

@export
(defmethod select-dao ((class symbol) &rest expressions)
  (apply #'select-dao (find-class class) expressions))

(defmethod make-find-sql ((class <dao-table-class>) &rest pk-values)
  (initialize-dao-table-class class)
  (let ((primary-key (table-primary-key class)))
    (unless primary-key
      (error '<unknown-primary-key-error>
             :table-name (table-name class)))

    (select :*
      (from (intern (table-name class) :keyword))
      (where `(:and ,@(mapcar #'(lambda (key val)
                                  `(:= ,(unlispify key) ,val))
                              primary-key
                              pk-values)))
      (limit 1))))

@export
(defmethod find-dao ((class <dao-table-class>) &rest pk-values)
  (car
   (retrieve-by-sql (apply #'make-find-sql class pk-values)
                    :as class)))

@export
(defmethod find-dao ((class symbol) &rest pk-values)
  (apply #'find-dao (find-class class) pk-values))

@export
(defmethod create-dao ((class <dao-table-class>) &rest initargs)
  (let ((obj (apply #'make-instance class initargs)))
    (insert-dao obj)))

@export
(defmethod create-dao ((class symbol) &rest initargs)
  (apply #'create-dao (find-class class) initargs))

@export
(defmethod save-dao ((obj <dao-class>))
  (if (dao-synced obj)
      (update-dao obj)
      (insert-dao obj)))

@export
(defmethod retrieve-by-sql ((sql string) &key binds as)
  (let ((results (retrieve-by-raw-sql sql binds)))
    (setf results
          (iter (for result in results)
            (collect
                (iter (for (column value) on result by #'cddr)
                  (collect (lispify column))
                  (collect value)))))
    (if as
        (mapcar (lambda (result)
                  (apply #'make-dao-instance as result))
                results)
        results)))

@export
(defmethod retrieve-by-sql ((sql sql-statement) &key binds as)
  (declare (ignore binds))
  (multiple-value-bind (sql binds)
      (with-quote-char (sxql:yield sql))
    (retrieve-by-sql sql :binds binds :as as)))
