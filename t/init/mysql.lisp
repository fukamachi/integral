#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.init.mysql
  (:use :cl
        :integral)
  (:export :connect-to-testdb))
(in-package :integral-test.init.mysql)

(defun connect-to-testdb ()
  (disconnect-toplevel)
  (connect-toplevel :mysql
                    :database-name "integral_test"
                    :username "root"))
