(defpackage :cl-functional.tco
  (:documentation
   "Is part of CL-FUNCTIONAL main library. TCO, means Tail-Call-Optimization,
    and was created to allow TCO in any implementation. You can use it via,
    DEFUN-TAIL or LAMBDA-TAIL and where is supposed to ocurr the tail recursive
    call to the function you simplily call the function itself with tail-call.
    tail-call is a symbol, nor a macro or function. That symbol is searched
    to detect where to do a TCO.

    (defun my+ (&rest args)
      (let ((fcall (lambda-tail (x remainding)
                                (if (not remainding) x
                                    (tail-call (+ x (first remainding))
                                               (rest remainding))))))
        (funcall fcall 0 args)))

    ATM it only works with normal arguments, in other words, it not handles
    correctly the &keyword o &rest arguments.")
  (:use :common-lisp)
  (:export :lambda-tail :defun-tail))

(in-package :cl-functional.tco)

(defmacro defun-at-compile-time (&body body)
  "This packages needs a lot of functions that will be only used at the
   macro-expansion time, and therefore whe need to evaluate some defun's at
   compile time"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,@body)))

(defun-at-compile-time @applicable-args (arg-list)
  "This function gets the arguments that a function can use from a argument
   list definition. For example in list of args like
   (a b &key c d &rest r) this function retuns '(a b c d r)"
  (loop for i in arg-list
     with rest = nil
     with r = (list)
     do
       (cond ((equalp i '&rest) (setf rest t)))
       (when (and (not (equalp i '&rest))
		  (not (equalp i '&body))
		  (not (equalp i '&optional))
		  (not (equalp i '&keyword)))
	 (setf r (nconc r
			(list (if (listp i) (first i) i)))))
     finally (return r)))


(defun-at-compile-time @tail-callp (code)
  "Returns if the code have a tail-call indicating to do such optimization"
  (when (listp code)
    (let ((first (first code)))
      (or (equalp first :tail-call)
	  (equalp first 'tail-call)))))

(defun-at-compile-time @alter-code (code modify-fun &optional
                                         (match-fun #'@tail-callp))
  "Modifies a function when the MATCH-FUN returns t. That part of the function
   gets modified by MODIFY-FUN. Please note that this code and the function
   arguments should not make any side effects."
  (flet ((recall-on (code) (@alter-code code modify-fun match-fun)))
    (let ((code (if (funcall match-fun code) (funcall modify-fun code) code)))
      (loop for element in code
	 collecting
	   (if (listp element) (recall-on element)
	       element)))))

(defun-at-compile-time @apply-tail-call-op (code next-call-args call-againp
                                                 ret-block)
  "This code modifies the call to the TCO optimization, to work with the macro
   LAMBDA-TAIL."
  (let ((rest (cdr code)))
    `(progn 
       (setf ,next-call-args (list ,@rest))
       (setf ,call-againp t)
       (return-from ,ret-block nil))))

(defmacro lambda-tail ((&rest input-args) &body input-body)
  "The macro that searches for the tail-call symbol, like it was a function
   call and applies the TCO optimization."
  (let* ((call-againp (gensym "call-againp_"))
	 (next-call-args (gensym "next-call-args_"))
	 (function (gensym "function_"))
	 (result (gensym "result_"))
	 (lambda-block (gensym "lambda-block_"))
	 (applicable-input-args (@applicable-args input-args))
	 (lambda-tail-call-op 
	  (lambda (code) (@apply-tail-call-op code next-call-args call-againp
					 lambda-block)))
	 (input-body (@alter-code input-body lambda-tail-call-op)))
    `(lambda ,input-args
       (let* ((,call-againp nil)
	      (,next-call-args nil)
	      (,function (lambda ,input-args (block ,lambda-block ,@input-body)))
	      (,result nil))
	 (apply ,function (list ,@applicable-input-args))
	 (loop while ,call-againp
	    do
	      (setf ,call-againp nil)
	      (setf ,result (apply ,function ,next-call-args)))
	 ,result))))

(defmacro defun-tail (fun-name (&rest input-args) &body input-body)
  "Defines tail call recursive named funcion, using LAMBDA-TAIL."
  (unless (symbolp fun-name)
    (error "The first argument have should be a function name"))
  (let ((lambda-name (gensym "lambda-name_"))
	(applicable-args (@applicable-args input-args)))
    `(defun ,fun-name ,input-args
       (let ((,lambda-name (lambda-tail ,input-args ,@input-body)))
	 (apply ,lambda-name (list ,@applicable-args))))))



