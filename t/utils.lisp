(in-package :cl-functional-test)

(define-test test-lazy
  (assert-true (lazyp (lazy (error "Error"))))
  (assert-true (lazyp (lazy-make (lambda () (error "Error")))))
  (let (x y z)
    (with-lazy ((setx (setf x 0)) (sety (setf y 0)) (setz (setf z 0))
                (incy (incf y)) (incz (incf z)) (incz2 (incf z)))
      (assert-equalp 0 setx)
      (assert-equalp nil y)
      (assert-equalp 0 sety)
      (assert-equalp 0 y)
      (assert-equalp 1 incy)
      (assert-equalp 1 y)
      (assert-equalp nil z)
      (assert-equalp z setz)
      (assert-equalp 0 setz)
      (assert-equalp 1 incz)
      (assert-equalp 1 z)
      (assert-equalp 2 incz2)
      (assert-equalp 2 z)
      (assert-equalp 1 incz))))

(define-test test-ifte
  (assert-equalp 1 (ifte nil 0 :else 1))
  (assert-equalp 1 (ifte t 1 2 34 1 :else (error "Error")))
  (assert-equalp 1 (let (x) (ifte t :then (setf x 1) 2 x)))
  (assert-equalp 2 (let (x) (ifte nil :else 0 (setf x 2) 1 x ))))

(define-test test-if-not
  (assert-equalp 1 (if-not nil 1 nil))
  (assert-equalp t (if-not t nil t)))

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

(define-test test-comment-dispatch-macro-character
  (assert-equalp '(1 2 3 4) '(1 2 3 4 #_5))
  (assert-expands (+ 1 2 3) (+ 1 2 3 #_4))
  (assert-expands (+ 1 0) (+ 1 0 #_(* 100 100))))

(define-test test-comp-case
  (assert-equalp 'cool (comp-case "BEHAPPY" #'equalp
                         ("Made in Argentina" 'fail)
                         ("Cool" 'fail)
                         ("BeHappy" 'cool))))

(define-test test-loop-partition
  (assert-equalp '(5 6) (loop-partition (x y) '(1 2 3 4 5 6)
                           (list x y)))
  (assert-equalp '((7 8 9) (4 5 6) (1 2 3))
                 (let (r)
                   (loop-partition (a b c) '(1 2 3 4 5 6 7 8 9)
                      (push (list a b c) r)))))

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

(define-test test-bool
    (assert-eql t (bool 1))
    (assert-eql nil (bool nil)))


