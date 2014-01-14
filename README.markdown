# Integral

Integral is a object relational mapper for Common Lisp based on [CL-DBL](https://github.com/fukamachi/cl-dbi) and [SxQL](https://github.com/fukamachi/sxql).

<span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```common-lisp
(defclass tweet ()
    ((id :primary-key t
         :reader tweet-id)
     (status :initarg :status
             :accessor :tweet-status)
     (user :initarg :user
           :accessor :tweet-user))
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
