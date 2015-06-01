(in-package :cl-user)
(defpackage :cl-functional-test-asd (:use :cl :asdf))
(in-package :cl-functional-test-asd)

(defsystem cl-functional-test
  :depends-on (:cl-functional :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                        :components
                        ((:test-file "utils"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
