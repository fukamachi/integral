#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.connection.postgres
  (:use :cl
        :cl-test-more
        :dbi
        :integral
        :integral.connection.postgres))
(in-package :integral-test.connection.postgres)

(plan 3)

(let ((db (dbi:connect :postgres
                       :database-name "integral_test"
                       :username "nitro_idiot")))
  (dbi:do-sql db "DROP TABLE IF EXISTS tweets")
  (dbi:do-sql db "CREATE TABLE tweets (id SERIAL PRIMARY KEY, status TEXT NOT NULL, \"user\" VARCHAR(64) NOT NULL, UNIQUE (id, \"user\"))")

  (is (column-definitions db "tweets")
      '(("id" :TYPE INTEGER :AUTO-INCREMENT T :PRIMARY-KEY T :NOT-NULL T)
        ("status" :TYPE integral.type:TEXT :AUTO-INCREMENT NIL :PRIMARY-KEY NIL :NOT-NULL T)
        ("user" :TYPE (integral.type:VARCHAR 64) :AUTO-INCREMENT NIL :PRIMARY-KEY nil :NOT-NULL T)))

  (ok (every
       #'equal
       (table-indices db "tweets")
       '((:unique-key t :primary-key nil :columns ("id" "user"))
         (:unique-key t :primary-key t :columns ("id")))))

  (dbi:do-sql db "DROP TABLE IF EXISTS users")
  (dbi:do-sql db "CREATE TABLE users (id SERIAL PRIMARY KEY, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (ok (every
       #'equal
       (table-indices db "users")
       '((:unique-key t :primary-key nil :columns ("family_name" "first_name"))
         (:unique-key t :primary-key t :columns ("id")))))

  (dbi:disconnect db))

(finalize)
