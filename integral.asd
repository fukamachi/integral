#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-asd
  (:use :cl :asdf))
(in-package :integral-asd)

(defsystem integral
  :version "0.0.1"
  :author "Eitarow Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:cl-syntax-annot
               :sxql
               :dbi
               :closer-mop
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "integral" :depends-on ("connection" "table" "type" "error"))
                 (:file "table" :depends-on ("connection" "column" "util"))
                 (:file "column" :depends-on ("connection" "type"))
                 (:file "connection" :depends-on ("connection-drivers" "error"))
                 (:module "connection-drivers"
                  :pathname "connection"
                  :components
                  ((:file "mysql")
                   (:file "postgres")
                   (:file "sqlite3")))
                 (:file "type" :depends-on ("connection" "util"))
                 (:file "error")
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
