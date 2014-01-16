#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.table
  (:use :cl
        :integral
        :integral.table
        :integral-test.init
        :cl-test-more)
  (:import-from :integral.column
                :table-column-definition)
  (:import-from :integral.table
                :initializedp))
(in-package :integral-test.table)

(plan nil)

(disconnect-toplevel)

(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil))

(defclass tweet () ()
  (:metaclass dao-table-class))

(is (table-name 'tweet) "tweet")
(is (c2mop:class-direct-superclasses (find-class 'tweet))
    (list (find-class 'dao-class))
    "tweet inherits dao-class implicitly")

(defclass my-dao-class (dao-class) ())

(defclass tweet (my-dao-class) ()
  (:metaclass dao-table-class))

(is (c2mop:class-direct-superclasses (find-class 'tweet))
    (list (find-class 'my-dao-class))
    "tweet inherits my-dao-class")

(defclass tweet ()
  ((id :primary-key t)
   (status)
   (user))
  (:metaclass dao-table-class))

(is-error (table-definition 'tweet)
          'type-missing-error)

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

(is (table-name 'tweet) "tweets")

(let ((sxql:*quote-character* #\`)
      (sxql:*use-placeholder* nil))
  (is (table-definition 'tweet)
      "CREATE TABLE `tweets` (`id` INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64))")

  (setf (find-class 'tweet) nil)
  (defclass tweet ()
    ((id :type serial
         :primary-key t
         :reader tweet-id)
     (status :type string
             :accessor :tweet-status)
     (user :type (varchar 64)
           :accessor :tweet-user)
     (%cache :type hash-table
             :initform (make-hash-table)
             :ghost t))
    (:metaclass dao-table-class)
    (:table-name "tweets"))

  (is (table-definition 'tweet)
      "CREATE TABLE `tweets` (`id` INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64))")

  (setf (find-class 'tweet) nil)
  (defclass tweet ()
    ((id :col-type bigint
         :auto-increment t
         :primary-key t
         :reader tweet-id)
     (status :col-type text
             :accessor :tweet-status)
     (user :col-type (:varchar 64)
           :accessor :tweet-user))
    (:metaclass dao-table-class)
    (:table-name "tweets")
    (:keys id))

  (is (table-definition 'tweet)
      "CREATE TABLE `tweets` (`id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64), KEY (`id`))")

  (setf (find-class 'tweet) nil)
  (defclass tweet ()
    ((id :col-type bigint
         :auto-increment t
         :reader tweet-id)
     (status :col-type text
             :accessor :tweet-status)
     (user :col-type (:varchar 64)
           :accessor :tweet-user))
    (:metaclass dao-table-class)
    (:table-name "tweets")
    (:primary-key (id))
    (:keys id))

  (is (table-definition 'tweet)
      "CREATE TABLE `tweets` (`id` BIGINT AUTO_INCREMENT, `status` TEXT, `user` VARCHAR(64), PRIMARY KEY (`id`), KEY (`id`))")

  (setf (find-class 'tweet) nil)
  (defclass tweet ()
    ((id :col-type bigint
         :auto-increment t
         :reader tweet-id)
     (status :col-type text
             :initarg :status
             :accessor :tweet-status)
     (user :col-type (:varchar 64)
           :initarg :user
           :accessor :tweet-user))
    (:metaclass dao-table-class)
    (:table-name "tweets")
    (:primary-key (id))
    (:unique-keys id (status user)))

  (is (table-definition 'tweet)
      "CREATE TABLE `tweets` (`id` BIGINT AUTO_INCREMENT, `status` TEXT, `user` VARCHAR(64), PRIMARY KEY (`id`), UNIQUE (`id`), UNIQUE (`status`, `user`))"))

(diag ":generate-slots t")

(setf (find-class 'tweet) nil)
(defclass tweet () ()
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:generate-slots t))

(is (c2mop:class-direct-slots (find-class 'tweet))
    nil
    "No slots.")

(is-error (make-instance 'tweet)
          'connection-not-established-error
          "Can't allocate an instance before any db connections are established.")

(connect-to-testdb)

(execute-sql "CREATE TABLE tweets (id INTEGER NOT NULL PRIMARY KEY, status TEXT, user VARCHAR(64))")

(ok (make-instance 'tweet))

(let ((slots (c2mop:class-direct-slots (find-class 'tweet))))
  (is (length slots) 3)
  (ok (every (lambda (slot)
               (typep slot 'table-column-definition))
             slots)))

;; Redefinition

(defclass tweet () ()
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:generate-slots t))

(is (c2mop:class-direct-slots (find-class 'tweet))
    nil
    "No slots, again.")
(ok (not (initializedp (find-class 'tweet))))
(ok (make-instance 'tweet))

(defclass tweet ()
  ((user :initarg :user
         :accessor tweet-user))
  (:metaclass dao-table-class)
  (:table-name "tweets")
  (:generate-slots t))

(let ((slots (c2mop:class-direct-slots (find-class 'tweet))))
  (is (length slots) 1))

(ok (make-instance 'tweet))

(let ((slots (c2mop:class-direct-slots (find-class 'tweet))))
  (is (length slots) 3)
  (ok (every (lambda (slot)
               (typep slot 'table-column-definition))
             slots))
  (is (c2mop:slot-definition-name (car slots))
      'user)
  (is (c2mop:slot-definition-readers (car slots))
      '(tweet-user)))

(let ((tw (make-instance 'tweet
                         :status "Is this okay?"
                         :user "nitro_idiot")))
  (is (tweet-user tw) "nitro_idiot"))

(finalize)
