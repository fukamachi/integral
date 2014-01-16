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
                ((:file "init")
                 (:file "connection/sqlite3")
                 (:file "connection/mysql")
                 (:file "connection/postgres")
                 (:file "type")
                 (:file "table")
                 (:file "integral"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
