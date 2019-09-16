(in-package :cl-user)
(defpackage integral-test.migration.mysql
  (:use :cl
        :integral
        :integral.migration
        :integral-test.init
        :prove)
  (:import-from :integral.migration
                :compute-migrate-table-columns
                :make-migration-sql)
  (:import-from :integral.table
                :table-definition))
(in-package :integral-test.migration.mysql)

(plan 11)

(disconnect-toplevel)

(connect-to-testdb :mysql)

(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil)) 
(execute-sql "DROP TABLE IF EXISTS tweets")

(subtest "first definition (no explicit primary key)"
  (defclass tweet ()
    ((user :type (varchar 128)
           :accessor tweet-user))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))
  (execute-sql (table-definition 'tweet))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)
      "No migration at first"))

(subtest "redefinition as the same"
  (defclass tweet ()
    ((user :type (varchar 128)
           :accessor tweet-user))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)
      "No migration at first"))

(subtest "redefinition with :auto-pk nil"
  (defclass tweet ()
    ((user :type (varchar 128)
           :accessor tweet-user))
    (:metaclass <dao-table-class>)
    (:table-name "tweets")
    (:auto-pk nil))

  (multiple-value-bind (new modify old)
      (compute-migrate-table-columns (find-class 'tweet))
    (is new nil)
    (is modify nil)
    (is old '(("%oid" :type (:bigint 20 :unsigned) :auto-increment t :primary-key t :not-null t)))))

(subtest "redefinition (with explicit primary key)"
  (defclass tweet ()
    ((id :type serial
         :primary-key t
         :reader tweet-id)
     (status :type string
             :accessor :tweet-status)
     (user :type (varchar 64)
           :accessor :tweet-user))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))

  (multiple-value-bind (new modify old)
      (compute-migrate-table-columns (find-class 'tweet))
    (is new '(("id" :type :integer :auto-increment t :primary-key t :not-null t)
              ("status" :type :text :auto-increment nil :primary-key nil :not-null nil)))
    (is modify '(("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null nil)))
    (is old '(("%oid" :type (:bigint 20 :unsigned) :auto-increment t :primary-key t :not-null t))))

  (is (make-migration-sql (find-class 'tweet))
      (list "ALTER TABLE tweets DROP COLUMN %oid"
            "ALTER TABLE tweets MODIFY COLUMN user VARCHAR(64)"
            "ALTER TABLE tweets ADD COLUMN id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY FIRST, ADD COLUMN status TEXT AFTER id"))

  (migrate-table (find-class 'tweet))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)
      "No migration after migrating"))

(subtest "redefinition"
  (defclass tweet ()
    ((id :type serial
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 64)
           :accessor tweet-user)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))

  (multiple-value-bind (new modify old)
      (compute-migrate-table-columns (find-class 'tweet))
    (is (mapcar #'car new) '("created_at") "Add created_at")
    (is modify nil "No modification")
    (is (mapcar #'car old) '("status") "Delete status"))

  (is (make-migration-sql (find-class 'tweet))
      (list "ALTER TABLE tweets DROP COLUMN status"
            "ALTER TABLE tweets ADD COLUMN created_at CHAR(8) AFTER user"))

  (migrate-table (find-class 'tweet))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)
      "No migration after migrating"))

(subtest "redefinition (modifying the column type)"
  (defclass tweet ()
    ((id :type serial
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 128)
           :accessor tweet-user)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))

  (multiple-value-bind (new modify old)
      (compute-migrate-table-columns (find-class 'tweet))
    (is new nil)
    (is modify '(("user" :TYPE (:VARCHAR 128) :AUTO-INCREMENT NIL :PRIMARY-KEY NIL :NOT-NULL NIL)))
    (is old nil))

  (migrate-table (find-class 'tweet))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)))

(subtest "redefinition of primary key"
  (defclass tweet ()
    ((id :type bigint
         :auto-increment t
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 128)
           :accessor :tweet-user)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets"))

  (multiple-value-bind (new modify old)
      (compute-migrate-table-columns (find-class 'tweet))
    (is new nil)
    (is modify '(("id" :TYPE :BIGINT :AUTO-INCREMENT T :PRIMARY-KEY T :NOT-NULL T)))
    (is old nil))

  (migrate-table (find-class 'tweet))

  (is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
      '(nil nil nil)
      "No migration after migrating"))

(subtest "add :unique-keys"
  (is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
      '(nil nil nil))

  (defclass tweet ()
    ((id :type bigint
         :auto-increment t
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 128)
           :accessor :tweet-user)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets")
    (:unique-keys (user created-at)))

  (is (sxql:yield (caar (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))))
      "CREATE UNIQUE INDEX user_and_created_at ON tweets (user, created_at)")

  (migrate-table (find-class 'tweet))

  (is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
      '(nil nil nil)))

(subtest "modify :unique-keys"
  (defclass tweet ()
    ((id :type bigint
         :auto-increment t
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 128)
           :accessor :tweet-user)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets")
    (:unique-keys (id user created-at)))

  (destructuring-bind (add-index drop-primary-key drop-index)
      (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    (is (sxql:yield (car drop-index))
        "DROP INDEX user_and_created_at ON tweets")
    (is drop-primary-key nil)
    (is (sxql:yield (car add-index))
        "CREATE UNIQUE INDEX id_and_user_and_created_at ON tweets (id, user, created_at)"))

  (migrate-table (find-class 'tweet))

  (is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
      '(nil nil nil)))

(subtest "delete :unique-keys and add :keys"
  (defclass tweet ()
    ((id :type bigint
         :auto-increment t
         :primary-key t
         :reader tweet-id)
     (user :type (varchar 128)
           :accessor tweet-user)
     (active-p :type boolean
               :accessor tweet-active-p)
     (created-at :type (char 8)))
    (:metaclass <dao-table-class>)
    (:table-name "tweets")
    (:unique-keys)
    (:keys (user created-at)))

  (destructuring-bind (add-index drop-primary-key drop-index)
      (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    (is (sxql:yield (car drop-index))
        "DROP INDEX id_and_user_and_created_at ON tweets")
    (is drop-primary-key nil)
    (is (sxql:yield (car add-index))
        "CREATE INDEX user_and_created_at ON tweets (user, created_at)"))

  (migrate-table (find-class 'tweet))

  (is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
      '(nil nil nil)))

(subtest "redifinition to the totally different table"
  (defclass tweet ()
    ((user :type (varchar 128)
           :accessor tweet-user))
    (:metaclass <dao-table-class>)
    (:unique-keys)
    (:keys)
    (:table-name "tweets"))

  (migrate-table (find-class 'tweet))

  (defclass tweet ()
    ((created-at :type (char 8))
     (active-p :type boolean
               :accessor tweet-active-p))
    (:metaclass <dao-table-class>)
    (:unique-keys)
    (:keys)
    (:table-name "tweets"))

  (is (cadr (make-migration-sql (find-class 'tweet)))
      "ALTER TABLE tweets ADD COLUMN created_at CHAR(8) AFTER %oid, ADD COLUMN active_p BOOLEAN AFTER created_at"))

(finalize)
