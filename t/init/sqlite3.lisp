(in-package :cl-user)
(defpackage integral-test.init.sqlite3
  (:use :cl
        :integral)
  (:import-from :uiop
                :file-exists-p)
  (:export :connect-to-testdb))
(in-package :integral-test.init.sqlite3)

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
