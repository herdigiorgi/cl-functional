(in-package :cl-user)
(defpackage cl-functional-test-asd(:use :cl :asdf))
(in-package :cl-functional-test-asd)

(defsystem cl-functional-test
  :depends-on (:lisp-unit2)
  :components ((:module "t"
                        :components
                        ((:file "utils")))))
