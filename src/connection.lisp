(in-package :cl-user)
(defpackage integral.connection
  (:use :cl)
  (:import-from :integral.error
                :<connection-not-established-error>)
  (:import-from :dbi
                :connection-driver-type
                :connect
                :disconnect
                :ping)
  (:import-from :dbi.driver
                :<dbi-connection>)
  (:import-from :sxql
                :*quote-character*))
(in-package :integral.connection)

(cl-syntax:use-syntax :annot)

@export
(defvar *db* nil
  "Current connection object.")

(defstruct (connection (:constructor %make-connection))
  "Class of database connection"
  (connect-args nil :type list)
  (driver-name nil :type keyword)
  (handle nil :type (or <dbi-connection> null)))

@export
(defun make-connection (driver-name &rest args &key database-name &allow-other-keys)
  (declare (ignore database-name))
  (%make-connection :connect-args args
                    :driver-name driver-name))

@export
(defun connect-toplevel (driver-name &rest args &key database-name &allow-other-keys)
  "Connect to the database with given `ARGS'.
Same as DBI:CONNECT except this has a simple cache mechanizm."

  (declare (ignore database-name))

  (when (and *db*
             (connection-handle *db*)
             (not (dbi:ping (connection-handle *db*))))
    (dbi:disconnect (connection-handle *db*)))
  (setf *db* (apply #'make-connection driver-name args)))

@export
(defun connected-p ()
  "Return whether already connected to a database."
  (not (null *db*)))

@export
(defun disconnect-toplevel ()
  "Disconnect the current connection.
If no connections established, this do nothing."

  (when (connected-p)
    (dbi:disconnect (connection-handle *db*))
    (setf *db* nil)))

(defun make-connection-handle (connection)
  (apply #'connect (connection-driver-name connection) (connection-connect-args *db*)))

@export
(defun get-connection ()
  "Return the current established connection handle."

  (unless (connected-p)
    (error '<connection-not-established-error>))

  (let ((handle (connection-handle *db*)))
    (cond
      ((and handle (dbi:ping handle)) handle)
      (handle (progn
                (dbi:disconnect handle)
                (setf (connection-handle *db*) (make-connection-handle *db*))))
      (t (setf (connection-handle *db*) (make-connection-handle *db*))))))

@export
(defun database-type ()
  "Return the database type of the current connection. It is one of :mysql, :postgres and :sqlite3.
If no connections are established, NIL will be returned."

  (when (connected-p)
    (connection-driver-type (get-connection))))

@export
(defun connection-quote-character (&optional conn)
  (if (and (not conn)
           (not (connected-p)))
      nil
      (ecase (connection-driver-type (or conn (get-connection)))
        (:mysql #\`)
        (:postgres #\")
        (:sqlite3 #\"))))

@export
(defmacro with-quote-char (&body body)
  `(let ((sxql:*quote-character* (or sxql:*quote-character*
                                     (connection-quote-character))))
     ,@body))

@export
(defun retrieve-table-column-definitions-by-name (conn table-name)
  "Retrieve column definitions of `TABLE-NAME' from `CONN'."

  (funcall
   (ecase (connection-driver-type conn)
     (:mysql #'integral.connection.mysql:column-definitions)
     (:postgres #'integral.connection.postgres:column-definitions)
     (:sqlite3 #'integral.connection.sqlite3:column-definitions))
   conn table-name))

@export
(defun retrieve-table-indices (conn table-name)
  (funcall
   (ecase (connection-driver-type conn)
     (:mysql #'integral.connection.mysql:table-indices)
     (:postgres #'integral.connection.postgres:table-indices)
     (:sqlite3 #'integral.connection.sqlite3:table-indices))
   conn table-name))

@export
(defun last-insert-id (conn table-name serial-key-name)
  "Return the last value of a serial column."
  (ecase (connection-driver-type conn)
    (:mysql    (integral.connection.mysql:last-insert-id conn table-name serial-key-name))
    (:postgres (integral.connection.postgres:last-insert-id conn table-name serial-key-name))
    (:sqlite3  (integral.connection.sqlite3:last-insert-id conn table-name))))
