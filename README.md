# CL-FUNCTIONAL

CL-FUNCTIONAL is a Common Lisp library under the *Simplified BSD License*,
dedicated to give some functiona like macros, and functions.

You can find the documentation in the souce code.

## utils.lisp 

* >> : foward function composition
* << : backward function composition
* /> : foward pipelining
* </ : backward pipelining
* memoize : generate a morization function from an existing one
* lazy : creates a delayed computation
* with-lazy : allows work with lazy computations as it were normal variables

## tco.lisp

Provides tail call optimization, via transformation a lambda function with a
recusive tail call to a iterative loop.

```lisp
(defun my+ (&rest args)
      (let ((fcall (lambda-tail (x remainding)
                                (if (not remainding) x
                                    (tail-call (+ x (first remainding))
                                               (rest remainding))))))
        (funcall fcall 0 args)))
```
