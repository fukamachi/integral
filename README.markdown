# Integral

Integral is an object relational mapper for Common Lisp based on [CL-DBI](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql).

<span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.

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

## How to use

### Generating database schema from CLOS definitions

```common-lisp
(defclass tweet ()
  ((id :type serial
       :primary-key t
       :reader tweet-id)
   (status :type string
           :initarg :status
           :accessor :tweet-status)
   (user :type (varchar 64)
         :initarg :user
         :accessor :tweet-user))
  (:metaclass <dao-table-class>)
  (:keys user))

(table-definition 'tweet)
;=> "CREATE TABLE tweet (id INTEGER AUTO_INCREMENT PRIMARY KEY, status TEXT, user VARCHAR(64), KEY (user))"

(execute-sql (table-definition 'tweet))
```

### Generating a class definition from DB schema

If you'd like to administrate a database directly by writing raw SQLs, or wanna use Integral for an existing database, you can generate slot definitions from it.

```common-lisp
(defclass tweet () ()
  (:metaclass <dao-table-class>)
  (:generate-slots t))
```

`:generate-slots` option means slot definitions follow database schema. Note you must establish a database connection before the first `allocate-instance`.

### Auto-migration mode

If `integral:*auto-migration-mode*` is set `T`, all class changes will be applied to database tables automatically.

### inflate/deflate

```common-lisp
(defclass user ()
  ((name :type string
         :initarg :name)
   (created_at :type timestamp
               :initarg :created_at))
  (:metaclass integral:<dao-table-class>))
;=> #<INTEGRAL.TABLE:<DAO-TABLE-CLASS> USER>

(find-dao 'user 1)
;=> #<USER #x302001D9452D>

(slot-value * 'created_at)
;=> 3599088727

;; Define inflate/deflate methods
(defmethod integral:inflate ((object user) (slot-name (eql 'created_at)) value)
  (local-time:universal-to-timestamp value))
(defmethod integral:deflate ((object user) (slot-name (eql 'created_at)) value)
  (local-time:format-timestamp nil value))

(slot-value (find-dao 'user 1) 'created_at)
;=> @2014-01-19T11:52:07.000000+09:00
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
* retrieve-sql ((sql string) &key binds as)
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
* int
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

## Installation

As Integral depends on the latest [CL-DBI](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql), you have to download them before installation.

```
$ cd ~/quicklisp/local-projects
$ git clone git://github.com/fukamachi/cl-dbi
$ git clone git://github.com/fukamachi/sxql
$ git clone git://github.com/fukamachi/integral
```

```
(ql:quickload :integral)
```

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.
