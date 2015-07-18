(in-package :cl-user)
(defpackage :cl-functional-test-asd (:use :cl :asdf))
(in-package :cl-functional-test-asd)

(defsystem cl-functional-test
  :depends-on (:cl-functional :lisp-unit)
  :components ((:module "t"
                        :components
                        ((:file "cl-functional")
                         (:file "utils")
                         (:file "data-structures"))))
  :perform (test-op (op c)
                    (eval (read-from-string "(cl-functional-test:test-all)"))))
