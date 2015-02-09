(in-package :cl-user)
(defpackage integral-test.validation
  (:use :cl
        :integral
        :integral-test.init
        :prove))

(in-package :integral-test.validation)

(plan nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation using validation-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (find-class 'tweet nil)
  (setf (find-class 'tweet) nil))

(defclass tweet ()
  ((id :col-type bigint
       :auto-increment t
       :reader tweet-id)
   (status :col-type (:varchar 140)
           :initarg :status
           :accessor :tweet-status)
   (user :col-type (:varchar 64)
         :initarg :user
         :accessor :tweet-user))
  (:metaclass <dao-table-class>)
  (:table-name "tweets")
  (:primary-key (id))
  (:unique-keys id (status user)))

(connect-to-testdb)

(execute-sql (sxql:drop-table (intern (table-name 'tweet) :keyword) :if-exists t))
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

(reconnect-to-testdb)

;; Adding the second record.
(let ((tweet (make-instance 'tweet
                            :status "Second tweet. Woohoo."
                            :user "nitro_idiot")))
  (ok (not (slot-boundp tweet 'id)))
  (ok (insert-dao tweet)
      "Can insert")
  (is (slot-value tweet 'id) 2
      "The auto increment ID should be incremented"))

(is-type (find-dao 'tweet 1)
         'tweet)

(reconnect-to-testdb)

(execute-sql (sxql:drop-table (intern (table-name 'tweet) :keyword) :if-exists t))
(execute-sql (table-definition 'tweet))

;; Add presence validation.
(defmethod insert-dao :around ((obj tweet))
  (if (validate-presence-of obj 'status)
      (call-next-method)
      (error "Failed to validate-presence-of status")))

(let ((tweet (make-instance 'tweet
                            :user "nitro_idiot"))
      (tweet2 (make-instance 'tweet
                             :status "Fuge"
                             :user "nitro_idiot")))
  (is-error (insert-dao tweet) 'simple-error)
  (ok (insert-dao tweet2))
  (setf (:tweet-status tweet) "Status yay")
  (ok (insert-dao tweet) "Can insert now"))

;;Add length validation.
(reconnect-to-testdb)

(execute-sql (sxql:drop-table (intern (table-name 'tweet) :keyword) :if-exists t))
(execute-sql (table-definition 'tweet))

(defmethod insert-dao :around ((obj tweet))
  (if (validate-length-of obj 'status :max 140)
      (call-next-method)
      (error "Failed to validate-length-of status")))


(let ((tweet (make-instance 'tweet
                            :status (concatenate 'string "Very l" (make-string 140 :initial-element #\o) "ng")
                            :user "nitro_idiot"))
      (tweet2 (make-instance 'tweet
                             :status "Fuge"
                             :user "nitro_idiot")))
  (is-error (insert-dao tweet) 'simple-error)
  (ok (insert-dao tweet2))
  (setf (:tweet-status tweet) "Short status.")
  (ok (insert-dao tweet) "Can insert now with short"))

;;Add validate-formats
(reconnect-to-testdb)

(execute-sql (sxql:drop-table (intern (table-name 'tweet) :keyword) :if-exists t))
(execute-sql (table-definition 'tweet))

(defmethod insert-dao :around ((obj tweet))
  (if (validate-format-of obj 'user "^[A-Za-z0-9_]{1,15}$")
      (call-next-method)
      (error "Failed to validate-format-of user")))

(let ((tweet (make-instance 'tweet
                            :status "First"
                            :user "@invalid  username")))
  (is-error (insert-dao tweet) 'simple-error)
  (setf (:tweet-user tweet) "Hannibal7878")
  (ok (insert-dao tweet) "Can insert now"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation using class validation slot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-class 'secure-account nil)
  (setf (find-class 'secure-account) nil))

(defvar *validate-fn-results* t)

(defun validate-account (obj)
  *validate-fn-results*)

(defclass secure-account ()
  ((id :col-type bigint
       :auto-increment t
       :reader tweet-id)
   (name :col-type (:varchar 100)
         :initarg :name
         :accessor :account-name)
   (password :col-type (:varchar 30)
             :initarg :pass
             :accessor :account-pass)
   (email :col-type (:varchar 64)
          :initarg :email
          :accessor :account-email))
  (:validations
   (:presence name)
   (:length password :min 8 :max 30)
   ;;This is not correct validatin regex but...
   (:format email "^([a-z0-9\+_\-]+)(\.[a-z0-9\+_\-]+)*@([a-z0-9\-]+\.)+[a-z]{2,6}$")
   (:fn #'validate-account))
  (:metaclass <dao-table-class>)
  (:table-name "accounts")
  (:primary-key (id))
  (:unique-keys id))

(connect-to-testdb)

(execute-sql (sxql:drop-table (intern (table-name 'secure-account) :keyword) :if-exists t))
(execute-sql (table-definition 'secure-account))

(let ((without-name (make-instance 'secure-account
                                   :pass "test1234"
                                   :email "poketo7878@gmail.com"))
      (too-short-pass (make-instance 'secure-account
                                     :pass "short"
                                     :email "poketo7878@gmail.com"
                                     :name "Pocket7878"))
      (malformed-email (make-instance 'secure-account
                                      :pass "test1234"
                                      :email "wrong-email"
                                      :name "Pocket7878"))
      (valid-account1 (make-instance 'secure-account
                                    :name "Pocket7878"
                                    :pass "test1234"
                                    :email "poketo7878@gmail.com"))
      (valid-account2 (make-instance 'secure-account
                                    :name "Pocket7878"
                                    :pass "test1234"
                                    :email "poketo7878@gmail.com")))
  (ok (not (insert-dao without-name)))
  (ok (not (insert-dao too-short-pass)))
  (ok (not (insert-dao malformed-email)))
  (ok (insert-dao valid-account1))
  (let ((*validate-fn-results* nil))
    (ok (not (insert-dao valid-account2)))))

(finalize)
