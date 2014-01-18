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
                ((:file "init/sqlite3")
                 (:file "init/mysql")
                 (:file "init/postgres")
                 (:file "init")
                 (:test-file "connection/sqlite3")
                 (:test-file "connection/mysql")
                 (:test-file "connection/postgres")
                 (:test-file "type")
                 (:test-file "table")
                 (:test-file "migration/sqlite3")
                 (:test-file "migration/mysql")
                 (:test-file "migration/postgres")
                 (:test-file "integral"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (let* ((sql-log (intern #.(string :*sql-log-stream*) (find-package :integral.database)))
                           (val (symbol-value sql-log)))
                      (setf (symbol-value sql-log) nil)
                      (unwind-protect
                           (funcall (intern #.(string :run-test-system) :cl-test-more)
                                    c)
                        (setf (symbol-value sql-log) val)))
                    (asdf:clear-system c)))
