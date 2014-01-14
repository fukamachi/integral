#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test
  (:use :cl
        :integral
        :cl-test-more)
  (:import-from :integral
                :insert-sql
                :update-sql
                :delete-sql
                :find-sql))
(in-package :integral-test)

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
      "CREATE TABLE `tweets` (`id` BIGINT AUTO_INCREMENT, `status` TEXT, `user` VARCHAR(64), PRIMARY KEY (`id`), UNIQUE (`id`), UNIQUE (`status`, `user`), KEY (`id`))"))

(let ((sxql:*quote-character* #\`)
      (sxql:*use-placeholder* nil))

  (let ((tweet (make-instance 'tweet
                              :status "This is the first tweet. Yay."
                              :user "nitro_idiot")))
    (is (insert-sql tweet)
        "INSERT INTO `tweets` (`status`, `user`) VALUES ('This is the first tweet. Yay.', 'nitro_idiot')")

    ;; for testing
    (setf (slot-value tweet 'id) 1)

    (is (update-sql tweet)
        "UPDATE `tweets` SET `id` = 1, `status` = 'This is the first tweet. Yay.', `user` = 'nitro_idiot' WHERE (`id` = 1)")

    (is (delete-sql tweet)
        "DELETE FROM `tweets` WHERE (`id` = 1)")

    (is (find-sql (find-class 'tweet) 1)
        "SELECT * FROM `tweets` WHERE (`id` = 1) LIMIT 1")))

(defvar *db-path* (asdf:system-relative-pathname :integral #P"t/test.db"))
(when (uiop:file-exists-p *db-path*)
  (delete-file *db-path*))

(ok (connect-toplevel :sqlite3 :database-name *db-path*))

(execute-sql (table-definition 'tweet))

(let ((tweet (make-instance 'tweet
                            :status "This is the first tweet. Yay."
                            :user "nitro_idiot")))
  (ok (insert-dao tweet))
  (ok (and (slot-boundp tweet 'id)
           (slot-value tweet 'id)))

  (let ((result (first (select-dao 'tweet))))
    (is-type result 'tweet)
    (is (slot-value result 'id)
        (slot-value tweet 'id))
    (is (slot-value result 'user)
        (slot-value tweet 'user))
    (is (slot-value result 'status)
        (slot-value tweet 'status))))

(is-type (find-dao 'tweet 1)
         'tweet)

(finalize)
