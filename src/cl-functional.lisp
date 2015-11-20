(in-package :cl-user)
(uiop:define-package :cl-functional
    (:use-reexport :cl-functional.packages
                   :cl-functional.tco
                   :cl-functional.utils
                   :cl-functional.data-structures))
