#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.connection.mysql
  (:use :cl
        :prove
        :integral-test.init
        :dbi
        :integral
        :integral.connection.mysql))
(in-package :integral-test.connection.mysql)

(plan 3)

(let ((db (connect-to-testdb :mysql)))
  (dbi:do-sql db "DROP TABLE IF EXISTS tweets")
  (dbi:do-sql db "CREATE TABLE tweets (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, status TEXT NOT NULL, user VARCHAR(64) NOT NULL, UNIQUE (id, user))")

  (is (column-definitions db "tweets")
      '(("id" :type (:integer 11) :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil  :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t)))

  (ok (every
       #'equal
       (table-indices db "tweets")
       '(("id" :unique-key t :primary-key nil :columns ("id" "user"))
         ("PRIMARY" :unique-key t :primary-key t :columns ("id")))))

  (dbi:do-sql db "DROP TABLE IF EXISTS users")
  (dbi:do-sql db "CREATE TABLE users (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (ok (every
       #'equal
       (table-indices db "users")
       '(("first_name" :unique-key t :primary-key nil :columns ("first_name" "family_name"))
         ("PRIMARY" :unique-key t :primary-key t :columns ("id"))))))

(finalize)
