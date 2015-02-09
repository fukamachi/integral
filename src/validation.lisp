(in-package :cl-user)
(defpackage integral.validation
  (:use :cl 
        :alexandria
        :sxql)
  (:import-from :integral.table
                :table-name
                :<dao-table-class>
                :<dao-class>)
  (:import-from :integral.connection
                :*db*
                :make-connection
                :get-connection
                :connect-toplevel
                :disconnect-toplevel
                :last-insert-id
                :database-type
                :with-quote-char)
  (:import-from :dbi
                :prepare
                :execute
                :fetch))

(in-package integral.validation)

(cl-syntax:use-syntax :annot)

@export
(defgeneric validate-presence-of (object slot-name)
  (:method ((object <dao-class>) slot-name)
    (and (slot-boundp object slot-name)
     (not (null (slot-value object slot-name))))))

@export
(defgeneric validate-length-of (object slot-name &key min max is)
  (:method ((object <dao-class>) slot-name &key (min 0) max is)
    (if (slot-boundp object slot-name)
        (let ((val (slot-value object slot-name)))
          (cond (is (= is (length val)))
                (max
                 (<= min (length val) max))
                (<= min (length val))))
        t)))

@export
(defgeneric validate-format-of (object slot-name format)
  (:method ((object <dao-class>) slot-name format)
    (if (slot-boundp object slot-name)
        (let ((val (slot-value object slot-name)))
          (cl-ppcre:all-matches format val))
        t)))

(defun generate-validation-function (validation-entry)
  (if (listp validation-entry)
      (let ((tag (car validation-entry))
            (args (cdr validation-entry)))
        (ecase tag
          (:presence
           (let ((slot-name (car args)))
             (if (and slot-name (symbolp slot-name))
                 (lambda (obj)
                   (validate-presence-of obj slot-name))
                 (error "Malformed slot-name: ~A" slot-name))))
          (:length
           (let ((slot-name (car args))
                 (min (getf (cdr args) :min 0))
                 (max (getf (cdr args) :max nil))
                 (is  (getf (cdr args) :is nil)))
             (if (and slot-name (symbolp slot-name))
                 (lambda (obj)
                   (validate-length-of obj slot-name :min min :max max :is is))
                 (error "Malformed slot-name: ~A" slot-name))))
          (:format
           (let ((slot-name (car args))
                 (format (cadr args)))
             (cond ((and slot-name (symbolp slot-name) (stringp format))
                    (lambda (obj)
                      (validate-format-of obj slot-name format)))
                   ((not (stringp format))
                    (error "~A is not correct format string" format))
                   (t
                    (error "Malformed slot-name: ~A" slot-name)))))
          (:fn
           (let ((fn (eval (car args))))
             (cond ((functionp fn)
                    (lambda (obj)
                      (funcall fn obj)))
                   (t (error "~A is not function" fn)))))))
      (error "Invalid validation entry: ~A" validation-entry)))
      
(defun generate-validations-function (validation-list)
  (loop 
     for entry in validation-list
     collect (generate-validation-function entry) into fns
     finally (return (lambda (obj)
                       (every (lambda (fn)
                                (funcall fn obj)) fns)))))

(defgeneric validation-function (class)
  (:documentation "Generate Validation Function")
  (:method ((class <dao-table-class>))
    (if (and (slot-exists-p class 'integral.table::validations)
             (slot-boundp class 'integral.table::validations))
        (generate-validations-function (slot-value class 'integral.table::validations))
        nil)))
  
@export
(defgeneric valid-p (object)
  (:method ((obj <dao-class>))
    (let ((validation-fn (validation-function (class-of obj))))
      (if validation-fn
          (funcall validation-fn obj)
          t))))
