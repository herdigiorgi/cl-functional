(in-package :cl-user)
(defpackage :cl-functional.packages
  (:use :common-lisp)
  (:export :add-nickname-package
           :define-nicknames))
(in-package :cl-functional.packages)

(defmacro add-nickname-package (package-name &rest new-nicknames)
  (let* ((package (find-package package-name))
         (nicknames (package-nicknames package)))
    (loop for newnick in new-nicknames do
         (pushnew newnick nicknames))
    (rename-package package package-name nicknames)
    (value)))

(defmacro define-nicknames (&body nick-definitions)
  `(progn
     ,(loop for nick-def in nick-definitions
         for package-name = (car nick-def)
         for new-nicknames = (cdr nick-def)
        collecting
           `(add-nickname-package ,package-name ,@new-nicknames))))

