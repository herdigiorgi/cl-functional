(in-package :cl-functional-test)

(define-test testing-foward-chain-/> 
  (assert-equalp :FOO (/> :FOO))
  (assert-equalp :FOO (funcall (//> identity) :FOO))
  (assert-equalp 1 (/> 1 (- 2)))
  (assert-equalp 1 (funcall (//> (- 2)) 1))
  (assert-equalp 1/25 (/> 1 1+ 1+ 1+ (/ :/> 100)))
  (assert-equalp 1/25 (funcall (//> 1+ 1+ 1+ (/ ://> 100)) 1))
  (assert-equalp '(1 20) (/> 1 (list :/> 20))))

(define-test testing-backward-chain-</
  (assert-eql :FOO (</ :FOO))
  (assert-eql :FOO (funcall (<// identity) :FOO))
  (assert-eql 1 (</ (- 2) 1))
  (assert-eql 1 (funcall (<// (- 2)) 1))
  (assert-equalp 1/25 (</ (/ :</ 100) 1+ 1+ 1+ 1))
  (assert-equalp 1/25 (funcall (<// (/ :<// 100) 1+ 1+ 1+) 1)))

(define-test comment-dispatch-macro-character
  (assert-equalp '(1 2 3 4) '(1 2 3 4 #_5))
  (assert-expands (+ 1 2 3) (+ 1 2 3 #_4))
  (assert-expands (+ 1 0) (+ 1 0 #_(* 100 100))))

(define-test test-let-when
  (assert-expands (let ((a a)) (when a form))
                  (let-when ((a a)) form))
  (assert-expands (let (a) (when a (let (b) (when b f))))
                  (let-when (a b) f))
  (assert-expands (let ((a a))
                    (when a (let ((b b))
                              (when b (let ((c c))
                                        (when c form))))))
                  (let-when ((a a) (b b) (c c)) form))
  (assert-eql nil (let-when ((x nil)) t))
  (assert-eql nil (let-when ((x)) t)))




