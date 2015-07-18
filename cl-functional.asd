(in-package :cl-user)
(defpackage cl-functional-asd (:use :cl :asdf))
(in-package :cl-functional-asd)

(defsystem cl-functional
  :version "0.0.2"
  :description "cl-functional gives some useful functions and macros inspired in
                other languages facilities that improves the code writing, with
                a functional point of view."
  :author "Di Giorgi Hernan Ezequiel <contact@playnu.com.ar>"
  :license "Simplified BSD License"
  :components ((:module "src"
                        :components
                        ((:file "utils")
                         (:file "tco")
                         (:file "data-structures")
                         (:file "cl-functional"))))
  :in-order-to ((test-op (test-op cl-functional-test))))

(let ((project-dir (asdf:system-source-directory :cl-functional)))
  (pushnew project-dir asdf:*central-registry*))
