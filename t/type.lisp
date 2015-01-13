(in-package :cl-user)
(defpackage integral-test.type
  (:use :cl
        :prove
        :integral
        :integral.type))
(in-package :integral-test.type)

(plan 13)

(is (cltype-to-dbtype 'integer)
    :integer)
(is (cltype-to-dbtype 'integer 10)
    '(:integer 10))
(is (cltype-to-dbtype 'enum "one" "two" "three")
    '(:enum "one" "two" "three"))

(is (cltype-to-dbtype 'bigint 20 :unsigned)
    '(:bigint 20 :unsigned))

(is (string-to-dbtype "CHAR(32)")
    '(:char 32))
(is (string-to-dbtype "CHARACTER(32)")
    '(:char 32))
(is (string-to-dbtype "char(32)")
    '(:char 32))

(is (string-to-dbtype "text")
    :text)

(is (string-to-dbtype "character varying(32)")
    '(:varchar 32))

(is (string-to-dbtype "bigint(20) unsigned")
    '(:bigint 20 :unsigned))

(setf (type-alias :MY\ ORIGINAL\ TYPE) :MYOT)

(is (type-alias :MY\ ORIGINAL\ TYPE)
    :myot
    :test #'eq)

(is (string-to-dbtype "MY ORIGINAL TYPE")
    :myot)
(is (string-to-dbtype "MY ORIGINAL TYPE(128)")
    '(:myot 128))

(finalize)
