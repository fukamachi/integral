(in-package :cl-user)
(defpackage integral-test.connection.sqlite3
  (:use :cl
        :prove
        :integral-test.init
        :dbi
        :integral
        :integral.connection.sqlite3)
  (:import-from :integral.connection.sqlite3
                :table-primary-keys))
(in-package :integral-test.connection.sqlite3)

(plan 3)

(defparameter *conn* (connect-to-testdb :sqlite3))

(dbi:do-sql *conn* "CREATE TABLE tweets (id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL, status TEXT NOT NULL, user VARCHAR(64) NOT NULL, UNIQUE (id, user))")

(subtest "column-definitions"
  (is (column-definitions *conn* "tweets")
      '(("id" :type :integer :auto-increment t :primary-key t :not-null t)
        ("status" :type :text :auto-increment nil :primary-key nil :not-null t)
        ("user" :type (:varchar 64) :auto-increment nil :primary-key nil :not-null t))))

(subtest "table-indices & table-primary-keys"
  (ok (every
       #'equal
       (table-indices *conn* "tweets")
       '(("sqlite_autoindex_tweets_2" :unique-key t :primary-key nil :columns ("id" "user"))
         ("sqlite_autoindex_tweets_1" :unique-key t :primary-key t :columns ("id")))))

  (is (table-primary-keys *conn* "tweets") '("id")) 

  (dbi:do-sql *conn* "CREATE TABLE users (id INTEGER PRIMARY KEY, first_name VARCHAR(64) NOT NULL, family_name VARCHAR(64) NOT NULL, UNIQUE(first_name, family_name))")

  (is (table-primary-keys *conn* "users") '("id"))

  (ok (every
       #'equal
       (table-indices *conn* "users")
       '(("sqlite_autoindex_users_1" :unique-key t :primary-key nil :columns ("first_name" "family_name"))
         ("PRIMARY" :unique-key t :primary-key t :columns ("id"))))))

(subtest "last-insert-id"
  (is (last-insert-id *conn* "users") 0
      "Should be 0 when there's no record")
  (dbi:do-sql *conn* "INSERT INTO users (first_name, family_name) VALUES ('Eitaro', 'Fukamachi')")
  (is (last-insert-id *conn* "users") 1
      "Should be 1 after inserting")
  (setf *conn* (reconnect-to-testdb :sqlite3))
  (is (last-insert-id *conn* "users") 1
      "Should be still 1 after reconnecting"))

(finalize)
