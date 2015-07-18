(defpackage :cl-functional.data-structures
  (:use :common-lisp
        :cl-functional.utils)
  (:export :mk#
           :mk#!
           :hash-table-equal?
           :hash-table->list
           :hash-table-map))

(in-package :cl-functional.data-structures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HASH TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hash-table-equal? (hm1 hm2)
  (equalp hm1 hm2))

(defun hash-table-map (hash-table apply-func)
  (loop for key being the hash-keys of hash-table
     using (hash-value value)
     collecting (funcall apply-func key value)))

(defun hash-table->list (hash-table)
  (loop for key being the hash-keys of hash-table
     using (hash-value value)
     as result = (cons key (cons value result))
     finally (return result)))

(defun mk#! (table &rest args)
  (loop-partition (key value) args
     (setf (gethash key table) value))
  table)

(defun mk# (&rest args)
  (let ((table (make-hash-table :test 'equalp)))
    (loop-partition (key data) args
       (setf (gethash key table) data))
    table))
