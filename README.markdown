# Integral

Integral is an object relational mapper for Common Lisp based on [CL-DBL](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql).

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
  (:metaclass dao-table-class))

(connect-toplevel :mysql
                  :database-name "myapp"
                  :username "nitro_idiot"
                  :password "xxxxxxxx")

(let ((tw (make-instance 'tweet
                         :status "Good morning, world."
                         :user "nitro_idiot")))
  (insert-dao tw))

;; Same as the above
(create-dao 'tweet
            :status "Good morning, world."
            :user "nitro_idiot")

(let ((tw (find-dao 'tweet 3)))
  (with-slot (status user) tw
    (format t "~A said ~A" user status))
  (setf (tweet-status tw) "Good evening, world.")
  (update-dao tw))

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
  (:metaclass dao-table-class)
  (:keys user))

(table-definition 'tweet)
;=> "CREATE TABLE tweet (id INTEGER AUTO_INCREMENT PRIMARY KEY, status TEXT, user VARCHAR(64), KEY (user))"

(execute-sql (table-definition 'tweet))
```

### Generating a class definition from DB schema

If you'd like to administrate a database directly by writing raw SQLs, or wanna use Integral for an existing database, you can generate slot definitions from it.

```common-lisp
(defclass tweet () ()
  (:metaclass dao-table-class)
  (:generate-slots t))
```

`:generate-slots` option means slot definitions follow database schema. Note you must establish a database connection before the first `allocate-instance`.

### Auto-migration mode

If `integral:*auto-migration-mode*` is set `T`, all class changes will be applied to database tables automatically.

## Symbols

### Connections

* connect-toplevel (driver-name &rest args &key database-name &allow-other-keys)
* disconnect-toplevel ()

### Classes

* dao-class
* dao-table-class
* table-name (class)
* table-definition (class &key (yield t) if-not-exists)
* migrate-table (class)
* ensure-table-exists (class)
* \*auto-migration-mode\*

### SQL

* select-dao ((class dao-table-class) &rest expressions)
* insert-dao ((obj dao-class))
* create-dao ((class dao-table-class) &rest initargs)
* update-dao ((obj dao-class))
* delete-dao ((obj dao-class))
* execute-sql ((sql string) &rest bind)
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

* integral-error
* connection-not-established-error
* unknown-primary-key-error
* type-missing-error
* migration-error

## Installation

As Integral depends on the latest [CL-DBL](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql), you have to download them before installation.

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
