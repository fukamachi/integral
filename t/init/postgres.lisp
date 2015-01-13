(in-package :cl-user)
(defpackage integral-test.init.postgres
  (:use :cl
        :integral)
  (:export :connect-to-testdb))
(in-package :integral-test.init.postgres)

(defun connect-to-testdb ()
  (disconnect-toplevel)
  (connect-toplevel :postgres
                    :database-name "integral_test"
                    :username "nitro_idiot"))
