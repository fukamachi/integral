(in-package :cl-user)
(defpackage integral.fixture
  (:use :cl)
  (:import-from :integral
                :insert-dao)
  (:import-from :integral.table
                :<dao-class>)
  (:import-from :clos-fixtures
                :register-fixture))
(in-package :integral.fixture)

(defmethod clos-fixtures:register-fixture ((obj <dao-class>))
  (insert-dao obj))
