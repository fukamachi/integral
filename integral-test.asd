#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test-asd
  (:use :cl :asdf))
(in-package :integral-test-asd)

(defsystem integral-test
  :author "Eitarow Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:integral
               :cl-test-more
               :uiop)
  :components ((:module "t"
                :components
                ((:test-file "init")
                 (:test-file "connection/sqlite3")
                 (:test-file "connection/mysql")
                 (:test-file "connection/postgres")
                 (:test-file "type")
                 (:test-file "table")
                 (:test-file "migration")
                 (:test-file "integral"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
