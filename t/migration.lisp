#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.migration
  (:use :cl
        :integral
        :integral.migration
        :cl-test-more)
  (:import-from :integral.migration
                :compute-migrate-table-columns
                :generate-migration-sql)
  (:import-from :integral-test.init
                :connect-to-testdb)
  (:import-from :integral.table
                :table-definition))
(in-package :integral-test.migration)

(plan 5)

(disconnect-toplevel)

(connect-to-testdb)

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

(execute-sql (table-definition 'tweet))

(is (multiple-value-list (compute-migrate-table-columns (find-class 'tweet)))
    '(nil nil))

(defclass tweet ()
  ((id :type serial
       :primary-key t
       :reader tweet-id)
   (user :type (varchar 64)
         :accessor :tweet-user)
   (created_at :type (char 8)))
  (:metaclass dao-table-class)
  (:table-name "tweets"))

(multiple-value-bind (new old)
    (compute-migrate-table-columns (find-class 'tweet))
  (is new '("created_at"))
  (is old '("status")))

(is (sxql:yield (car (generate-migration-sql (find-class 'tweet))))
    "ALTER TABLE tweets ADD COLUMN created_at CHAR(8)")

(migrate-table-using-class (find-class 'tweet))

(is (compute-migrate-table-columns (find-class 'tweet))
    NIL)

(finalize)
