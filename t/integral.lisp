#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test
  (:use :cl
        :integral
        :integral-test.init
        :cl-test-more)
  (:import-from :integral
                :make-insert-sql
                :make-update-sql
                :make-delete-sql
                :make-find-sql))
(in-package :integral-test)

(plan nil)

(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil))

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

(let ((sxql:*quote-character* #\`)
      (sxql:*use-placeholder* nil))

  (let ((tweet (make-instance 'tweet
                              :status "This is the first tweet. Yay."
                              :user "nitro_idiot")))
    (is (sxql:yield (make-insert-sql tweet))
        "INSERT INTO `tweets` (`status`, `user`) VALUES ('This is the first tweet. Yay.', 'nitro_idiot')")

    ;; for testing
    (setf (slot-value tweet 'id) 1)

    (is (sxql:yield (make-update-sql tweet))
        "UPDATE `tweets` SET `id` = 1, `status` = 'This is the first tweet. Yay.', `user` = 'nitro_idiot' WHERE (`id` = 1)")

    (is (sxql:yield (make-delete-sql tweet))
        "DELETE FROM `tweets` WHERE (`id` = 1)")

    (is (sxql:yield (make-find-sql (find-class 'tweet) 1))
        "SELECT * FROM `tweets` WHERE (`id` = 1) LIMIT 1")))

(connect-to-testdb)

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
