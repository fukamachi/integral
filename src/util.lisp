(in-package :cl-user)
(defpackage integral.util
  (:use :cl)
  (:import-from :closer-mop
                :class-finalized-p
                :finalize-inheritance
                :slot-definition-name
                :class-direct-slots
                :class-direct-superclasses)
  (:import-from :group-by
                :group-by)
  (:import-from :alexandria
                :remove-from-plist))
(in-package :integral.util)

(cl-syntax:use-syntax :annot)

@export
(defun finalize-class-if-necessary (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class)))

@export
(defun class-slot-names (class)
  (finalize-class-if-necessary class)
  (mapcar
   #'c2mop:slot-definition-name
   (c2mop:class-direct-slots class)))

@export
(defun escaped-symbol-p (symbol)
  (declare (optimize speed)
           (type symbol symbol))
  (not (string= symbol (string-upcase symbol))))

@export
(defun symbol-name-literally (symbol)
  (if (escaped-symbol-p symbol)
      (symbol-name symbol)
      (string-downcase symbol)))

@export
(defun lispify (object)
  (etypecase object
    (symbol (intern (lispify (string-upcase object))
                    (symbol-package object)))
    (string (substitute #\- #\_ object))))

@export
(defun unlispify (object)
  (etypecase object
    (symbol (intern (unlispify (symbol-name-literally object))
                    (symbol-package object)))
    (string (substitute #\_ #\- object))))

@export
(defun class-inherit-p (target parent)
  (not (null
        (member parent
                (c2mop:class-direct-superclasses target)
                :test #'eq))))

@export
(defun group-by-plist-key (plist &key key (test #'eq))
  (group-by plist :key (lambda (column)
                         (getf column key))
                  :test test
                  :value (lambda (column)
                           (remove-from-plist column key))))

(defun %list-diff (a b &key (key #'identity) (test #'string=))
  (cond
    ((null a)
     (values nil nil b))
    ((null b)
     (values nil a nil))
    ((funcall test
              (funcall key (car a))
              (funcall key (car b)))
     (multiple-value-bind (intersection sub-a sub-b)
         (%list-diff (cdr a) (cdr b)
                     :key key
                     :test test)
       (values (cons (car a) intersection)
               sub-a
               sub-b)))
    (T (let ((pos (position (funcall key (car a)) (cdr b)
                            :key key
                            :test test)))
         (if pos
             (multiple-value-bind (intersection sub-a sub-b)
                 (%list-diff (cdr a) (nthcdr (+ 2 pos) b)
                             :key key
                             :test test)
               (values (cons (car a) intersection)
                       sub-a
                       (append (subseq b 0 (1+ pos)) sub-b)))
             (multiple-value-bind (intersection sub-a sub-b)
                 (%list-diff (cdr a) b
                             :key key
                             :test test)
                 (values intersection
                         (cons (car a) sub-a)
                         sub-b)))))))

@export
(defun list-diff (a b &key sort-key sort-key-a sort-key-b (sort-fn #'string<) (key #'identity) (test #'string=))
  "Compute differences two lists.
Note this can be applied for a list of string-designators."
  (%list-diff (sort (copy-list a) sort-fn :key (or sort-key-a sort-key #'identity))
              (sort (copy-list b) sort-fn :key (or sort-key-b sort-key #'identity))
              :key key
              :test test))
