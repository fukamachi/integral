#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-asd
  (:use :cl :asdf))
(in-package :integral-asd)

(defsystem integral
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:cl-syntax-annot
               :sxql
               :dbi
               :cl-ppcre
               :closer-mop
               :clos-fixtures
               :split-sequence
               :group-by
               :iterate
               :alexandria
               :trivial-types)
  :components ((:module "src"
                :components
                ((:file "integral" :depends-on ("connection" "table" "validation" "database" "migration" "type" "error" "variable"))
                 (:file "validation" :depends-on ("table"))
                 (:file "table" :depends-on ("connection" "column" "database" "variable" "util"))
                 (:file "column" :depends-on ("connection" "type"))
                 (:file "connection" :depends-on ("connection-drivers" "error"))
                 (:module "connection-drivers"
                  :pathname "connection"
                  :depends-on ("type" "util")
                  :components
                  ((:file "mysql")
                   (:file "postgres")
                   (:file "sqlite3")))
                 (:file "database" :depends-on ("connection"))
                 (:file "migration" :depends-on ("connection" "table" "column" "util"))
                 (:file "fixture" :depends-on ("integral" "table"))
                 (:file "type" :depends-on ("util"))
                 (:file "error")
                 (:file "variable")
                 (:file "util"))))
  :description "Object Relational Mapper for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op integral-test))))
