(defpackage :cl-functional-test
  (:use :common-lisp
        :lisp-unit
        :cl-functional.utils
        :cl-functional.data-structures)
  (:export :test-all))
(in-package :cl-functional-test)

(defun test-all ()
  (lisp-unit:remove-tests)
  (setq lisp-unit:*print-failures* t)
  (asdf:load-system :cl-functional-test :force t)
  (let ((test (lisp-unit:run-tests :all :cl-functional-test)))
    (when test (print-errors test))))
