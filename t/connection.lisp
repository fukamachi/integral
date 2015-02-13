(in-package :cl-user)
(defpackage integral-test.connection
  (:use :cl
        :integral
        :integral-test.init
        :prove)
  (:import-from :integral.connection
                :connection-handle))
(in-package :integral-test.connection)

(plan 2)

(let ((*db* (make-connection :mysql
                             :database-name "integral_test"
                             :username "root")))
  (ok (not (connection-handle *db*)))

  (get-connection)
  (ok (connection-handle *db*)))

(finalize)
