#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.table
  (:use :cl
        :integral
        :integral.table
        :cl-test-more))
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
      "CREATE TABLE `tweets` (`id` INTEGER AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64))")

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
      "CREATE TABLE `tweets` (`id` INTEGER AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64))")

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
      "CREATE TABLE `tweets` (`id` BIGINT AUTO_INCREMENT PRIMARY KEY, `status` TEXT, `user` VARCHAR(64), KEY (`id`))")

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

(finalize)
