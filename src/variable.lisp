(in-package :cl-user)
(defpackage integral.variable
  (:use :cl))
(in-package :integral.variable)

(cl-syntax:use-syntax :annot)

@export
(defvar *auto-migration-mode* nil
  "Whether use auto-migration mode or not.")
