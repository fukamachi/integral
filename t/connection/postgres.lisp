(in-package :cl-user)
(defpackage integral-test.connection.postgres
  (:use :cl
        :prove
        :integral-test.init
        :dbi
        :integral
        :integral.connection.postgres))
(in-package :integral-test.connection.postgres)

(plan 3)

(defparameter *conn* (connect-to-testdb :postgres))

(dbi:do-sql *conn* "DROP TABLE IF EXISTS tweets")
(dbi:do-sql *conn* "CREATE TABLE tweets (id SERIAL PRIMARY KEY, status TEXT NOT NULL, \"user\" VARCHAR(64) NOT NULL, UNIQUE (id, \"user\"))")

(subtest "column-definitions"
  (is (column-definitions *conn* "tweets")
      '(("id" :type :integer :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t))))

(subtest "table-indices"
  (ok (every
       #'equal
       (table-indices *conn* "tweets")
       '(("tweets_id_user_key" :unique-key t :primary-key nil :columns ("id" "user"))
         ("tweets_pkey" :unique-key t :primary-key t :columns ("id")))))

  (dbi:do-sql *conn* "DROP TABLE IF EXISTS users")
  (dbi:do-sql *conn* "CREATE TABLE users (id SERIAL PRIMARY KEY, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (ok (every
       #'equal
       (table-indices *conn* "users")
       '(("users_first_name_family_name_key" :unique-key t :primary-key nil :columns ("first_name" "family_name"))
         ("users_pkey" :unique-key t :primary-key t :columns ("id"))))))

(subtest "last-insert-id"
  (is (last-insert-id *conn* "users" "id") 0
      "Should be 0 when there's no record")
  (dbi:do-sql *conn* "INSERT INTO \"users\" (first_name, family_name) VALUES ('Eitaro', 'Fukamachi')")
  (is (last-insert-id *conn* "users" "id") 1
      "Should be 1 after inserting")
  (setf *conn* (reconnect-to-testdb :postgres))
  (is (last-insert-id *conn* "users" "id") 1
      "Should be still 1 after reconnecting")
  (dbi:do-sql *conn* "INSERT INTO \"users\" (first_name, family_name) VALUES ('Rudolph', 'Miller')")
  (is (last-insert-id *conn* "users" "id") 2
      "Should be 2 after reconnecting once more."))

(finalize)
