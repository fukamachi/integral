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

(is (dbtype-to-cltype "CHAR(32)")
    '(integral.type:char 32))
(is (dbtype-to-cltype "CHARACTER(32)")
    '(integral.type:char 32))
(is (dbtype-to-cltype "char(32)")
    '(integral.type:char 32))

(is (dbtype-to-cltype "text")
    'integral.type:text)

(is (dbtype-to-cltype "character varying(32)")
    '(integral.type:varchar 32))

(setf (type-alias "MY ORIGINAL TYPE") 'my-original-type)

(is (type-alias "MY ORIGINAL TYPE")
    'my-original-type
    :test #'eq)

(is (dbtype-to-cltype "MY ORIGINAL TYPE")
    'my-original-type)
(is (dbtype-to-cltype "MY ORIGINAL TYPE(128)")
    '(my-original-type 128))

(finalize)
