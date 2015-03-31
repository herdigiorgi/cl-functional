(defpackage :cl-functional.utils
  (:use :common-lisp)
  (:export
   :ifte
   :lazy
   :force
   :/>
   :</
   :ignore
   :<<
   :>>
   :memoize
   :lazy-memoize
   :comp-case))

(in-package :cl-f-style)

(defmacro lazy (&body body)
  "Creates a delayed computation, that can be executed with FORCE. Se also
   LAZY-MEMOIZE."
  `(lambda () ,@body))

(defmacro force (&body body)
  "Forces the execution of all the lazy functions in the BODY of this MACRO.
   You can call (force my-lazy) or with multiple lazies (force lazy1 lazy2) and
   the execution will be done in order."
  (labels ((make-funcall (f) `(funcall ,f))
	   (insert-funcalls (functions)
	     (when functions
	       (cons (make-funcall (car functions))
		     (insert-funcalls (cdr functions))))))
    `(progn ,@(insert-funcalls body))))


(defmacro ifte (predicate &body body)
  "Enables the common control flow IF with :THEN and :ELSE found in other 
   languages. An example: (ifte (null arg) :then (create) (fill) :else
   (clear-it)). IFTE puts all after the :THEN and :AFTER in a progn, so no
   progn is needed. "
  (let ((true-part (list))
	(false-part (list))
	(in-then t))
    (loop :for i :in body
	  :do
	     (cond
	       ((equalp i :then) (setf in-then t ))
	       ((equalp i :else) (setf in-then nil))
	       (t (if in-then
		      (push i true-part)
		      (push i false-part)))))
    (setf true-part  (nreverse true-part)
	  false-part (nreverse false-part))
    `(if ,predicate
	 (progn ,@true-part)
	 (progn ,@false-part))))

(defun replace-at-or-push-back (list old-element new-element
				&key (test #'equalp))
  "Creates a new list from LIST, replacing any OLD-ELEMENT by NEW-ELEMENT.
   The elements are compared with :TEST. If any OLD-ELEMENT is found,
   NEW-ELEMENT is inserted at the end."
  (labels
      ((replace-at (list pushback)
	 (let ((car (lazy (car list)))
	       (cdr (lazy (cdr list))))
	   (cond
	     ((equalp list nil) (if pushback (cons new-element nil) nil))
	     ((funcall test (force car) old-element)
	      (cons new-element (replace-at (force cdr) nil)))
	     (t (cons (car list) (replace-at (force cdr) pushback)))))))
    (replace-at list t)))

(defmacro private_/> (symbol &body body)
  (let ((result (car body)))
    (loop :for i :in (cdr body)
	  :do (setf result
		    (replace-at-or-push-back i symbol result)))
    result))

(defmacro /> (&body body)
  "Forward pipelining operator. (/> (a) (b) (c)) is translated to 
   (c (b (a). A practical example: (macroexpand '(/> 1 (* 3) (+ 2) (* 99))) 
   => (* 99 (+ 2 (* 3 1))). The position of the substitution can be changed
   using the symbol :/>, for example (/> (a) (b c :/> d) (j) is translated to
   (j (b c (a) d))."
  `(cl-f-style::private_/> :/> ,@body))

(defmacro </ (&body body)
  "The same as /> but uses the :</ symbol and the body is reversed."
  (let ((rbody (reverse body)))
    `(cl-f-style::private_/> :</ ,@rbody)))

(defmacro ignore (&body body)
  "Evaluates the body and always return nil"
  `(progn ,@body nil))

(defmacro private_>> (symbol &body body)
  (let* ((arg (gensym))
	 (fbody (replace-at-or-push-back (car body) symbol arg)))
    (loop :for i :in (cdr body)
	  :do (setf fbody (replace-at-or-push-back i symbol fbody)))
    `(lambda (,arg) ,fbody)))

(defmacro >> (&body body)
  "Foward function composition. (>> (f) (g) (h)) is tranlated to
   (lambda (x) (h (g (f x))))"
  `(cl-f-style::private_>> :>> ,@body))

(defmacro << (&body body)
  "Backward function composition. (<< (f) (g) (h)) is translated to
   (lambda (x) (f (g (h x))))"
  `(cl-f-style::private_>> :>> ,@(nreverse body)))

(defmacro while (predicate &body body)
  "A simple while found an any C like language."
  `(loop :while ,predicate :do ,@body))


(defun memoize (function)
  "Returns a morization function vection of the argument FUNCTION."
  (let ((hash-table (make-hash-table :test 'equalp)))
    (lambda (&rest args)
      (multiple-value-bind (value foundp)
	  (gethash args hash-table)
	(if foundp
	    value
	    (setf (gethash args hash-table) (apply function args)))))))

(defmacro lazy-memoize (&body body)
  "Creates a lazy computation that also memoizes de result."
  `(memoize (lazy ,@body)))

(defmacro comp-case (exp-to-compare exp-comp-func &body body)
  "Makes a case comparison with a custom function. The syntax is the same as
   the standar case but the second argument is a function. "
  (let ((to-compare (gensym "to-compare"))
	(comp-func (gensym "comp-func")))
    `(let ((,to-compare ,exp-to-compare)
	   (,comp-func ,exp-comp-func))
       (cond ,@ (mapcar (lambda (pair) (list
				   `(funcall ,comp-func ,(first pair)
				     ,to-compare)
				   (second pair)))
			body)))))
