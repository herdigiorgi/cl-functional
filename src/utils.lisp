(defpackage :cl-functional.utils
  (:use :common-lisp)
  (:export
   :ifte
   :if-not
   :lazy
   :with-lazy
   :lazy-force
   :lazy-make
   :lazyp
   :one-time-function
   :/>
   :</
   :<//
   ://>
   :|#_|
   :memoize
   :comp-case
   :loop-partition
   :let-when
   :bool
   :cartesian-product))

(in-package :cl-functional.utils)

(defclass <lazy-computation> ()
  ((computation :initarg :computation
                :accessor lazy-computation
                :documentation "Computation is a function that takes no
                                arguments. And when its is called the first
                                time, it is where the result is stored. If
                                computation stores the result, calculatedp is
                                t.")
   (calculatedp :initform nil :accessor lazy-calculatedp
                :type (BOOLEAN)
                :documentation "Holds if the computation was done or not.")))

(defgeneric lazy-force (<lazy-computation>))

(defun lazyp (object)
  (typep object '<lazy-computation>))

(defun lazy-make (func)
  (unless (functionp func)
    (error "A lazy computation needs a function without arguments
            for its creation."))
  (make-instance '<lazy-computation> :computation func))

(defmethod lazy-force ((lazy <lazy-computation>))
  (with-accessors ((computation lazy-computation)
                   (calculatedp lazy-calculatedp)) lazy
    (unless calculatedp
      (setf computation (let ((f computation)) (funcall f)))
      (setf calculatedp t))
    computation))

(defmacro lazy (&body body)
  "Creates a delayed computation, that can be executed with FORCE."
  `(lazy-make (lambda () ,@body)))

(defmacro with-lazy ((&rest paired-lazy-defs) &body body)
  "Witch lazy is like a let of lazies. The first argument is a list of pairs
   of lazy definitions of the form (var-name body-to-eval+). Later in the BODY.
   Any ocurrence of var-name is replaced with a functional call to lazy-force
   on that lazy computation. An example

   (with-lazy ((a (print 'COMPUTE-SOMETHING-A) 'A)
               (b (print 'b1 'b2) 'b))
     (list a b b))

   Is is also posible to use (with-lazy (a b c) ... ) where a b and c are
   already created lazy computations."
  (let ((letvars (list))
        (macrolets (list)))
    (loop for lazy-def in paired-lazy-defs
       with lazy-name
       with lazy-body
       with gensym-lazy-name
       with existing-lazy ; No need to generate a new lazy computation
       do
         (setf existing-lazy (not (and (listp lazy-def) (rest lazy-def))))
         
         (setf lazy-name (if existing-lazy lazy-def (first lazy-def)))
         (setf lazy-body (if existing-lazy nil (rest lazy-def)))
         (setf gensym-lazy-name  (gensym (string lazy-name)))
         
         (setf letvars
               (if existing-lazy
                   (cons `(,gensym-lazy-name ,lazy-name) letvars)
                   (cons `(,gensym-lazy-name (lazy ,@lazy-body)) letvars)))
         (setf macrolets
               (cons `(,lazy-name (lazy-force ,gensym-lazy-name))
                     macrolets)))
    `(let ,(nreverse letvars)
       (symbol-macrolet ,(nreverse macrolets) ,@body))))

(defmacro one-time-function (function-name &body body)
  "Creates a function that will be executed only one time, the next time you
   will get a cached result"
  (let* ((variable-name (gensym "VARIABLE_NAME_"))
         (doc-string (and (typep (car body) 'STRING) (cdr body) (car body)))
         (body (if doc-string (cdr body) body)))
    `(with-lazy ((,variable-name ,@body))
       (defun ,function-name ()
         ,doc-string
         ,variable-name))))

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

(defmacro if-not (condition &body body)
  (cond
    ((< (length body) 1) (error "IF-NOT need at least a not-true body"))
    ((> (length body) 2) (error "IF-NOT need at maximum 2 parts"))
    (:else `(if (not ,condition) ,(car body) ,(cadr body)))))

(defun replace-at-or-push-back (list old-element new-element
				&key (test #'equalp))
  "Creates a new list from LIST, replacing any OLD-ELEMENT by NEW-ELEMENT.
   The elements are compared with :TEST. If any OLD-ELEMENT is found,
   NEW-ELEMENT is inserted at the end."
  (labels
      ((replace-at (list pushback)
	 (with-lazy ((car (car list))
                     (cdr (cdr list)))
	   (cond
	     ((equalp list nil) (if pushback (cons new-element nil) nil))
	     ((funcall test car old-element)
	      (cons new-element (replace-at cdr nil)))
	     (t (cons (car list) (replace-at cdr pushback)))))))
    (replace-at list t)))

(defun %_/> (symbol body)
  (let ((result (car body)))
    (loop :for i :in (cdr body)
       :for list-item := (if (listp i) i (list i))
       :do (setf result
                 (replace-at-or-push-back list-item symbol result)))
    result))

(defmacro /> (&body body)
  "Forward pipelining operator. (/> (a) (b) (c)) is translated to 
   (c (b (a). A practical example: (macroexpand '(/> 1 (* 3) (+ 2) (* 99))) 
   => (* 99 (+ 2 (* 3 1))). The position of the substitution can be changed
   using the symbol :/>, for example (/> (a) (b c :/> d) (j) is translated to
   (j (b c (a) d))."
  (%_/> :/> body))

(defmacro </ (&body body)
  "The same as /> but uses the :</ symbol and the body is reversed."
  (%_/> :</ (reverse body)))

(defmacro //> (&body body)
  "Like /> but also creates a lambda function with one argument that is the first
element of />"
  (let ((arg (gensym "ARG")))
    `(lambda (,arg) ,(%_/> ://> (cons arg body)))))

(defmacro <// (&body body)
  "Backward function composition. (<< (f) (g) (h)) is translated to
   (lambda (x) (f (g (h x))))"
  (let ((arg (gensym "ARG")))
    `(lambda (,arg) ,(%_/> :<// (cons arg (reverse body))))))

(defun |#_-reader| (stream subchar arg)
  "Is like the comment ; reader macro but that is aplied to an expresion
for example #_1 is #|1|# or #_(+ 1 2 3) is #|(+ 1 2 3)|#"
  (declare (ignore subchar arg))
  (read stream t nil t)
  (values))

(set-dispatch-macro-character #\# #\_ #'|#_-reader|)

(defun memoize (function)
  "Returns a morization function vection of the argument FUNCTION."
  (let ((hash-table (make-hash-table :test 'equalp)))
    (lambda (&rest args)
      (multiple-value-bind (value foundp)
	  (gethash args hash-table)
	(if foundp
	    value
	    (setf (gethash args hash-table) (apply function args)))))))

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

(defmacro loop-partition ((&rest args) in-list &body do-body)
  "The same as (loop for (args) on in-list by the-size-args-cdrs do do-body)"
  (let* ((iterator (gensym "ITERATOR_"))
         (aux      (gensym "AUX_"))
         (result   (gensym "RESULT_"))
         (iteration-part
          (loop for arg in args collecting
               `(setf ,arg (let ((,aux (and ,iterator (car ,iterator))))
                             (setf ,iterator (and ,iterator (cdr ,iterator)))
                             ,aux)))))
    `(let (,@args (,iterator ,in-list) ,result)
       (do ()((not ,iterator) ,result)
         ,@iteration-part
         (setf ,result (progn ,@do-body))))))

(defmacro let-when ((&rest bindings) &body in-body)
  "Creates let bindings, but if one of the bindings is nil, the body is not
executed and returns nil"
  (loop for let-binding in (reverse bindings)
     for let-name = (if (listp let-binding) (car let-binding) let-binding)
     for result = `(let (,let-binding) (when ,let-name ,@in-body)) then
       `(let (,let-binding) (when ,let-name ,result))
     finally (return result)))

(defun bool (x)
  (if x t nil))

(defun cartesian-product (list-1 list-2)
  (loop for i in list-1 nconcing
       (loop for j in list-2 collecting
            (list i j))))
