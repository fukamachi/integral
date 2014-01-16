#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.type
  (:use :cl
        :cl-test-more
        :integral
        :integral.type))
(in-package :integral-test.type)

(plan 11)

(is (cltype-to-dbtype 'integer)
    'integer)
(is (cltype-to-dbtype 'integer 10)
    '(:integer 10))
(is (cltype-to-dbtype 'enum "one" "two" "three")
    '(:enum "one" "two" "three"))

(is (string-to-cltype "CHAR(32)")
    '(:char 32))
(is (string-to-cltype "CHARACTER(32)")
    '(:char 32))
(is (string-to-cltype "char(32)")
    '(:char 32))

(is (string-to-cltype "text")
    'integral.type:text)

(is (string-to-cltype "character varying(32)")
    '(:varchar 32))

(setf (type-alias "MY ORIGINAL TYPE") 'my-original-type)

(is (type-alias "MY ORIGINAL TYPE")
    'my-original-type
    :test #'eq)

(is (string-to-cltype "MY ORIGINAL TYPE")
    'my-original-type)
(is (string-to-cltype "MY ORIGINAL TYPE(128)")
    '(:my-original-type 128))

(finalize)
