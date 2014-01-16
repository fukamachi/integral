#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.connection.sqlite3
  (:use :cl
        :cl-test-more
        :dbi
        :integral
        :integral.connection.sqlite3)
  (:import-from :integral.connection.sqlite3
                :table-primary-keys)
  (:import-from :integral-test.init
                :connect-to-testdb))
(in-package :integral-test.connection.sqlite3)

(plan 5)

(let ((db (connect-to-testdb)))
  (dbi:do-sql db "CREATE TABLE tweets (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, status TEXT NOT NULL, user VARCHAR(64) NOT NULL, UNIQUE (id, user))")

  (is (table-primary-keys db "tweets") '("id"))

  (is (column-definitions db "tweets")
      '(("id" :TYPE INTEGER :AUTO-INCREMENT T :PRIMARY-KEY T :NOT-NULL T)
        ("status" :TYPE integral.type:TEXT :AUTO-INCREMENT nil :PRIMARY-KEY NIL :NOT-NULL T)
        ("user" :TYPE (:VARCHAR 64) :AUTO-INCREMENT NIL :PRIMARY-KEY NIL :NOT-NULL T)))

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
