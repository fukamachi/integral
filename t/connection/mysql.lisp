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

(defparameter *conn* (connect-to-testdb :mysql))

(dbi:do-sql *conn* "DROP TABLE IF EXISTS tweets")
(dbi:do-sql *conn* "CREATE TABLE tweets (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, status TEXT NOT NULL, user VARCHAR(64) NOT NULL, UNIQUE (id, user))")

(subtest "column-definitions"
  (is (column-definitions *conn* "tweets")
      '(("id" :type (:integer 11) :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil  :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t))))

(subtest "table-indices"
  (ok (every
       #'equal
       (table-indices *conn* "tweets")
       '(("id" :unique-key t :primary-key nil :columns ("id" "user"))
         ("PRIMARY" :unique-key t :primary-key t :columns ("id")))))

  (dbi:do-sql *conn* "DROP TABLE IF EXISTS users")
  (dbi:do-sql *conn* "CREATE TABLE users (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (ok (every
       #'equal
       (table-indices *conn* "users")
       '(("first_name" :unique-key t :primary-key nil :columns ("first_name" "family_name"))
         ("PRIMARY" :unique-key t :primary-key t :columns ("id"))))))

(subtest "last-insert-id"
  (is (last-insert-id *conn* "users" "id") 0
      "Should be 0 when there's no record")
  (dbi:do-sql *conn* "INSERT INTO users (first_name, family_name) VALUES ('Eitaro', 'Fukamachi')")
  (is (last-insert-id *conn* "users" "id") 1
      "Should be 1 after inserting")
  (setf *conn* (reconnect-to-testdb :mysql))
  (is (last-insert-id *conn* "users" "id") 1
      "Should be still 1 after reconnecting")
  (dbi:do-sql *conn* "INSERT INTO users (first_name, family_name) VALUES ('Rudolph', 'Miller')")
  (is (last-insert-id *conn* "users" "id") 2
      "Should be 2 after inserting once more."))

(finalize)
