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

(plan 27)

(disconnect-toplevel)

(connect-to-testdb :mysql)

(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil))

(defclass tweet ()
  ((user :type (varchar 128)
         :accessor :tweet-user))
  (:metaclass <dao-table-class>)
  (:table-name "tweets"))

(execute-sql "DROP TABLE IF EXISTS tweets")
(execute-sql (table-definition 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(nil nil nil))

(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil))

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

(execute-sql "DROP TABLE IF EXISTS tweets")
(execute-sql (table-definition 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(nil nil nil))

(defclass tweet ()
  ((id :type serial
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 64)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets"))

(multiple-value-bind (new modify old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is (mapcar #'car new) '("created_at"))
  (is modify nil)
  (is (mapcar #'car old) '("status")))

(is (car (make-migration-sql (find-class 'tweet)))
    "ALTER TABLE tweets DROP COLUMN status")

(migrate-table (find-class 'tweet))

(is (compute-migrate-table-columns (find-class 'tweet))
    NIL)

(defclass tweet ()
  ((id :type serial
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets"))

(multiple-value-bind (new modify old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is new nil)
  (is modify '(("user" :TYPE (:VARCHAR 128) :AUTO-INCREMENT NIL :PRIMARY-KEY NIL :NOT-NULL NIL)))
  (is old nil))

(migrate-table (find-class 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(nil nil nil))

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets"))

(multiple-value-bind (new modify old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is new nil)
  (is modify '(("id" :TYPE :BIGINT :AUTO-INCREMENT T :PRIMARY-KEY T :NOT-NULL T)))
  (is old nil))

(migrate-table (find-class 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(nil nil nil))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    '(nil nil nil))

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets")
  (:unique-keys (user created_at)))

(is (sxql:yield (caar (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))))
    "CREATE UNIQUE INDEX user_and_created_at ON tweets (user, created_at)")

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    '(nil nil nil))

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets")
  (:unique-keys (id user created_at)))

(destructuring-bind (add-index drop-primary-key drop-index)
    (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
  (is (sxql:yield (car drop-index))
      "DROP INDEX user_and_created_at ON tweets")
  (is drop-primary-key nil)
  (is (sxql:yield (car add-index))
      "CREATE UNIQUE INDEX id_and_user_and_created_at ON tweets (id, user, created_at)"))

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    '(nil nil nil))

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass <dao-table-class>)
  (:table-name "tweets")
  (:unique-keys)
  (:keys (user created_at)))

(destructuring-bind (add-index drop-primary-key drop-index)
    (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
  (is (sxql:yield (car drop-index))
      "DROP INDEX id_and_user_and_created_at ON tweets")
  (is drop-primary-key nil)
  (is (sxql:yield (car add-index))
      "CREATE INDEX user_and_created_at ON tweets (user, created_at)"))

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    '(nil nil nil))

(defmacro define-migration-test (before after &rest migration-sqls)
  (let ((class (gensym "CLASS")))
    `(let ((,class ,before))
       (progn
         (ensure-table-exists ,class)
         (migrate-table ,class)
         (let ((,class ,after))
           (is (make-migration-sql ,class)
               (list ,@migration-sqls)))))))

(setf (find-class 'tweet) nil)

(define-migration-test
    (defclass tweet ()
      ((user :type (varchar 128)
             :accessor :tweet-user))
      (:metaclass <dao-table-class>)
      (:table-name "tweets"))
    (defclass tweet ()
      ((user :type (varchar 128)
             :accessor :tweet-user))
      (:metaclass <dao-table-class>)
      (:table-name "tweets")))

(finalize)
