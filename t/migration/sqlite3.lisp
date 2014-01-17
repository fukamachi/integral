#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.migration.sqlite3
  (:use :cl
        :integral
        :integral.migration
        :integral-test.init
        :cl-test-more)
  (:import-from :integral.migration
                :compute-migrate-table-columns
                :generate-migration-sql)
  (:import-from :integral.table
                :table-definition))
(in-package :integral-test.migration.sqlite3)

(plan 12)

(disconnect-toplevel)

(connect-to-testdb :sqlite3)

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
  (:metaclass dao-table-class)
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
  (:metaclass dao-table-class)
  (:table-name "tweets"))

(multiple-value-bind (new modify old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is (mapcar #'car new) '("created_at"))
  (is modify nil)
  (is (mapcar #'car old) '("status")))

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
  (:metaclass dao-table-class)
  (:table-name "tweets"))

(multiple-value-bind (new modify old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is new nil)
  (is modify '(("user" :TYPE (:VARCHAR 128) :AUTO-INCREMENT NIL :PRIMARY-KEY NIL :NOT-NULL NIL)))
  (is old nil))

(migrate-table (find-class 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(NIL nil nil))

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:unique-keys (user created_at)))

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    NIL)

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:unique-keys (id user created_at)))

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    NIL)

(defclass tweet ()
  ((id :type bigint
       :auto-increment t
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 128)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:unique-keys)
  (:keys (user created_at)))

(migrate-table (find-class 'tweet))

(is (integral.migration::generate-migration-sql-for-table-indices (find-class 'tweet))
    NIL)

(finalize)
