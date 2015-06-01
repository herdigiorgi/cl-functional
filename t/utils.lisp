(in-package :cl-user)
(defpackage :cl-functional-test.utils
  (:use
   :cl
   :cl-functional.utils
   :prove)
  (:export :run-test))
(in-package :cl-functional-test.utils)

(plan nil)

(subtest "Testing forward chain />"
  (is (/> :FOO) :FOO)
  (is (/> 1 (- 2)) 1)
  (is (/> 1 1+) 2)
  (is (/> 1 (list :/> 20)) '(1 20) :test #'equalp))

(subtest "Testing backward chain </" 
  (is (</ :FOO) :FOO)
  (is (</ (- 2) 1) 1))

(finalize)



