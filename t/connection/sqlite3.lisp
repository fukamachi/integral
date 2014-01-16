#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.connection.sqlite3
  (:use :cl
        :cl-test-more
        :integral-test.init
        :dbi
        :integral
        :integral.connection.sqlite3)
  (:import-from :integral.connection.sqlite3
                :table-primary-keys))
(in-package :integral-test.connection.sqlite3)

(plan 5)

(let ((db (connect-to-testdb :sqlite3)))
  (dbi:do-sql db "CREATE TABLE tweets (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, status TEXT NOT NULL, user VARCHAR(64) NOT NULL, UNIQUE (id, user))")

  (is (table-primary-keys db "tweets") '("id"))

  (is (column-definitions db "tweets")
      '(("id" :type :integer :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t)))

  (ok (every
       #'equal
       (table-indices db "tweets")
       '((:unique-key t :primary-key t :columns ("id"))
         (:unique-key t :primary-key nil :columns ("id" "user")))))

  (dbi:do-sql db "CREATE TABLE users (id INTEGER PRIMARY KEY, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (is (table-primary-keys db "users") '("id"))

  (ok (every
       #'equal
       (table-indices db "users")
       '((:unique-key t :primary-key t :columns ("id"))
         (:unique-key t :primary-key nil :columns ("first_name" "family_name"))))))

(finalize)
