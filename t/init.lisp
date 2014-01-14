#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral-test.init
  (:use :cl
        :integral
        :cl-test-more)
  (:import-from :uiop
                :file-exists-p)
  (:export connect-to-testdb))
(in-package :integral-test.init)

(disconnect-toplevel)

(defvar *db-path* (asdf:system-relative-pathname :integral #P"t/test.db"))
(when (uiop:file-exists-p *db-path*)
  (delete-file *db-path*))

(defun initialize-testdb ()
  (disconnect-toplevel)
  (when (uiop:file-exists-p *db-path*)
    (delete-file *db-path*)))

(defun connect-to-testdb ()
  (initialize-testdb)
  (connect-toplevel :sqlite3 :database-name *db-path*))
