#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.init
  (:use :cl
        :integral
        :cl-test-more)
  (:export connect-to-testdb))
(in-package :integral-test.init)

(disconnect-toplevel)

(defun connect-to-testdb (&optional (driver-type :sqlite3))
  (ecase driver-type
    (:sqlite3 (integral-test.init.sqlite3:connect-to-testdb))
    (:mysql (integral-test.init.mysql:connect-to-testdb))
    (:postgres (integral-test.init.postgres:connect-to-testdb))))
