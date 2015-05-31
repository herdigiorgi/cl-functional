(in-package :cl-user)
(defpackage :cl-functional-test.utils
  (:use
   :cl
   :cl-functional.utils
   :lisp-unit2)
  (:export :run-test))
(in-package :cl-functional-test.utils)

(defun run-test ()
  (let (*debugger-hook*)
    (print-summary (run-tests :package :cl-functional-test.utils))))

(define-test test-foward-chain ()
  (assert-eql (/> :FOO) :FOO)
  (assert-eql (/> 1 (- 2)) 1)
  (assert-eql (/> 1 1+) 2)
  (assert-true (equalp (macroexpand '(/> :D :C :B :A)) '(:A (:B (:C :D))))))






