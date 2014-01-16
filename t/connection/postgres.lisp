#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.connection.postgres
  (:use :cl
        :cl-test-more
        :integral-test.init
        :dbi
        :integral
        :integral.connection.postgres))
(in-package :integral-test.connection.postgres)

(plan 3)

(let ((db (connect-to-testdb :postgres)))
  (dbi:do-sql db "DROP TABLE IF EXISTS tweets")
  (dbi:do-sql db "CREATE TABLE tweets (id SERIAL PRIMARY KEY, status TEXT NOT NULL, \"user\" VARCHAR(64) NOT NULL, UNIQUE (id, \"user\"))")

  (is (column-definitions db "tweets")
      '(("id" :type :integer :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t)))

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
         (:unique-key t :primary-key t :columns ("id"))))))

(finalize)
