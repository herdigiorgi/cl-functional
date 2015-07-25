(in-package :cl-functional-test)

(define-test hash-creation-test
  (assert-equalp
   (mk# :a 1 :b 2 :c 3)
   (let ((map (make-hash-table :test 'equalp)))
     (setf (gethash :a map) 1)
     (setf (gethash :b map) 2)
     (setf (gethash :c map) 3)
     map))
  (assert-equalp
   (mk#        'a 1 :b 2 :c 3 "d" 4)
   (mk#! (mk#) 'a 1 :b 2 :c 3 "d" 4))
  (assert-eql
   6
   (/> (mk# 'a 1 'b 2 'c 3) hash-table->list length)))

(define-test gethash-test
  (assert-expands (gethash :key hash) (g# :key hash))
  (assert-expands (gethash :key hash default) (g# :key hash default)))
