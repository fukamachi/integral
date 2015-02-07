#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test-asd
  (:use :cl :asdf))
(in-package :integral-test-asd)

(defsystem integral-test
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:integral
               :prove
               :uiop

               ;; for inflate/deflate testing
               :local-time
               ;; for type-inflate/type-deflate testing
               :split-sequence)
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
                 (:test-file "validation")
                 (:test-file "integral"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (let* ((sql-log (intern #.(string :*sql-log-stream*) (find-package :integral.database)))
                           (val (symbol-value sql-log)))
                      (setf (symbol-value sql-log) (make-broadcast-stream))
                      (unwind-protect
                           (funcall (intern #.(string :run-test-system) :prove.asdf)
                                    c)
                        (setf (symbol-value sql-log) val)))))
