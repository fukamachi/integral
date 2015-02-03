# Integral

[![Build Status](https://travis-ci.org/fukamachi/integral.svg?branch=master)](https://travis-ci.org/fukamachi/integral)

Integral is an object relational mapper for Common Lisp based on [CL-DBI](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql).

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

Should work well with MySQL/SQLite3 on SBCL/Clozure CL.

## Usage

```common-lisp
(defclass tweet ()
  ((id :type integer
       :primary-key t
       :auto-increment t
       :reader tweet-id)
   (status :type text
           :initarg :status
           :accessor tweet-status)
   (user :type (varchar 32)
         :initarg :user
         :accessor tweet-user))
  (:metaclass <dao-table-class>))

(connect-toplevel :mysql
                  :database-name "myapp"
                  :username "nitro_idiot"
                  :password "xxxxxxxx")

(let ((tw (make-instance 'tweet
                         :status "Good morning, world."
                         :user "nitro_idiot")))
  (save-dao tw))

;; Same as the above
(create-dao 'tweet
            :status "Good morning, world."
            :user "nitro_idiot")

(let ((tw (find-dao 'tweet 3)))
  (with-slot (status user) tw
    (format t "~A said ~A" user status))
  (setf (tweet-status tw) "Good evening, world.")
  (save-dao tw))

(let ((tw (find-dao 'tweet 3)))
  (delete-dao tw))
```

## Quickstart

### Installation

```common-lisp
(ql:quickload :integral)
```

### Connecting to database

`connect-toplevel` is a function to establish a connection to a database.

```common-lisp
(import 'integral:connect-toplevel)

(connect-toplevel :mysql
                  :database-name "testdb"
                  :username "nitro_idiot"
                  :password "password")
```

Integral is intended to work with MySQL, PostgreSQL and SQLite3. Replace `:mysql` the above by your favorite RDBMS engine name.

### Defining a database table

In Integral, database tables are defined as CLOS classes. A table definition looks like this.

```common-lisp
(import 'integral:<dao-table-class>)

(defclass user ()
  ((name :col-type text
         :initarg :name))
  (:metaclass <dao-table-class>))
```

This `user` class means a "user" table in a database with a single "TEXT" column, "name".

`table-definition` is a function to generate a `CREATE TABLE` SQL for it.

```common-lisp
(import '(integral:table-definition integral:execute-sql))

(table-definition 'user)
;=> "CREATE TABLE `user` (`%oid` SERIAL NOT NULL AUTO_INCREMENT PRIMARY KEY, `name` TEXT)"
;   NIL

(execute-sql (table-definition 'user))

;; Same as the above except ignoring CREATE TABLE if it already exists.
(ensure-table-exists 'user)
```

### Adding records

Table classes can be called with `make-instance` like Common Lisp standard-class.

```common-lisp
(make-instance 'user :name "Eitaro Fukamachi")
;=> #<USER %oid: <unbound>>
```

The instance won't be recorded in a database. Call `save-dao` it to add the record to a database.

```common-lisp
(import 'integral:save-dao)

(save-dao (make-instance 'user :name "Eitaro Fukamachi"))
;=> #<USER %oid: 1>

(save-dao (make-instance 'user :name "Tomohiro Matsuyama"))
;=> #<USER %oid: 2>
```

### Retrieving records

```common-lisp
(import 'integral:select-dao)

(select-dao 'user)
;=> (#<USER %oid: 1> #<USER %oid: 2>)

(mapcar (lambda (row)
          (slot-value row 'name))
        (select-dao 'user))
;=> ("Eitaro Fukamachi" "Tomohiro Matsuyama")
```

`select-dao` takes SxQL clauses. You can specify WHERE, ORDER BY or LIMIT with it.

```common-lisp
(import '(sxql:where sxql:limit))

(select-dao 'user
  (where (:= :name "Eitaro Fukamachi"))
  (limit 1))
;=> (#<USER %oid: 1>)
```

You can also use `find-dao` for retrieving a single row.

```common-lisp
(import 'integral:find-dao)

(find-dao 'user 1)
;=> #<USER %oid: 1>
```

### Updating records

```common-lisp
(let ((user (find-dao 'user 1)))
  (setf (slot-value user 'name) "深町英太郎")
  (save-dao user))
```

### Deleting records

```common-lisp
(import 'integral:delete-dao)

(let ((user (find-dao 'user 1)))
  (setf (slot-value user 'name) "深町英太郎")
  (delete-dao user))
```

### Migration

I introduced Integral generates a table schema from a CLOS class definition. But how can we do when we want to change the table schema after creating it.

Integral has a function to apply the change of the class definition to a table schema. It is generally known as "Migration".

For example, if you want to record a "profile" of users to "user" table, add a slot for it.

```common-lisp
(defclass user ()
  ((name :col-type text
         :initarg :name)
   (profile :col-type text
            :initarg :profile))
  (:metaclass <dao-table-class>))
```

Then call `migrate-table`.

```common-lisp
(import 'integral:migrate-table)

(migrate-table 'user)
;-> ALTER TABLE `user` ADD COLUMN `profile` TEXT AFTER `name`;
;=> NIL
```

All changes of indexes and column types are also followed.

```common-lisp
(defclass user ()
  ((id :col-type serial
       :primary-key t)
   (name :col-type (varchar 64)
         :initarg :name)
   (profile :col-type text
            :initarg :profile))
  (:metaclass <dao-table-class>))
;-> ALTER TABLE `user` DROP COLUMN `%oid`;
;   ALTER TABLE `user` MODIFY COLUMN `name` VARCHAR(64);
;   ALTER TABLE `user` ADD COLUMN `id` SERIAL NOT NULL PRIMARY KEY FIRST;
;=> NIL
```

### Mystique: Auto-migration

In development, class redefinitions are done many times. It's boring to execute `migrate-table` for each single time, isn't it?

Integral has **auto-migration** feature for executing `migrate-table` after redefinitions automatically.

Set `*auto-migration-mode*` T to use the mode.

```common-lisp
(setf integral:*auto-migration-mode* t)
```

### Another Way: define a class from an existing table

If you'd like to administrate a database directly by writing raw SQLs, or wanna use Integral for an existing database, you can generate slot definitions from it.

```common-lisp
(defclass tweet () ()
  (:metaclass <dao-table-class>)
  (:generate-slots t))
```

`:generate-slots` option means slot definitions follow database schema. Note you must establish a database connection before the first `make-instance`.

### inflate/deflate

`inflate` and `deflate` is a feature to convert data between a database and Common Lisp.

```common-lisp
(defclass user ()
  ((name :type string
         :initarg :name)
   (created-at :type timestamp
               :col-type integer
               :initarg :created-at))
  (:metaclass integral:<dao-table-class>))
;=> #<INTEGRAL.TABLE:<DAO-TABLE-CLASS> USER>

(find-dao 'user 1)
;=> #<USER #x302001D9452D>

(slot-value * 'created-at)
;=> 3599088727

;; Define inflate/deflate methods
(defmethod integral:inflate ((object user) (slot-name (eql 'created-at)) value)
  (local-time:universal-to-timestamp value))
(defmethod integral:deflate ((object user) (slot-name (eql 'created-at)) value)
  (local-time:timestamp-to-universal value))

(slot-value (find-dao 'user 1) 'created-at)
;=> @2014-01-19T11:52:07.000000+09:00
```

You can also set `inflate` and `deflate` functions via `:inflate` or `:deflate` keywords in `defclass`.

```common-lisp
(defclass user ()
  ((name :type string
         :initarg :name)
   (created-at :type timestamp
               :col-type integer
               :initarg :created-at
               :inflate #'local-time:universal-to-timestamp
               :deflate #'local-time:timestamp-to-universal))
  (:metaclass integral:<dao-table-class>))
```

### Relations

Although Integral doesn't have a specific feature for relations like `:has-a` and `:has-many`, it can be done with normal methods.

```common-lisp
(defmethod user-config ((user user))
  (find-dao 'user-config (user-id user)))

(defmethod user-entries ((user user))
  (select-dao 'entry (where (:= :user_id (user-id user)))))
```

### Wanna write a raw SQL?

```common-lisp
(import 'integral:retrieve-by-sql)

(retrieve-by-sql "SELECT * FROM user")
;=> ((:%oid 1 :name "深町英太郎"
;     :profile "I love Common Lisp and beer")
;    (:%oid 2 :name "Tomohiro Matsuyama"
;     :profile NIL))
```

`retrieve-by-sql` takes `:as` keyword argument to specify a class of the result record.

```common-lisp
(retrieve-sql "SELECT * FROM user" :as 'user)
;=> (#<USER %oid: 1> #<USER %oid: 2>)
```

## Symbols

### Connections

* connect-toplevel (driver-name &rest args &key database-name &allow-other-keys)
* disconnect-toplevel ()

### Classes

* &lt;dao-class&gt;
* &lt;dao-table-class&gt;
* table-name (class)
* table-definition (class &key (yield t) if-not-exists)
* inflate (object slot-name value)
* deflate (object slot-name value)
* migrate-table (class)
* ensure-table-exists (class)
* recreate-table (class)
* \*auto-migration-mode\*

### SQL

* select-dao ((class &lt;dao-table-class&gt;) &rest expressions)
* insert-dao ((obj &lt;dao-class&gt;))
* create-dao ((class &lt;dao-table-class&gt;) &rest initargs)
* update-dao ((obj &lt;dao-class&gt;))
* delete-dao ((obj &lt;dao-class&gt;))
* execute-sql ((sql string) &optional binds)
* retrieve-by-sql ((sql string) &key binds as)
* save-dao ((obj &lt;dao-class&gt;))
* where
* order-by
* group-by
* limit

### Data types

* serial
* tinyint
* smallint
* mediumint
* bigint
* text
* varchar
* enum
* datetime
* date
* timestamp

### Errors

* &lt;integral-error&gt;
* &lt;connection-not-established-error&gt;
* &lt;unknown-primary-key-error&gt;
* &lt;type-missing-error&gt;
* &lt;migration-error&gt;

## See Also

* [CL-DBI](http://8arrow.org/cl-dbi/) - Database independent interface library.
* [SxQL](http://8arrow.org/sxql/) - SQL builder library.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.
