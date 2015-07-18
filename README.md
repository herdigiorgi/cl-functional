# CL-FUNCTIONAL

CL-FUNCTIONAL is a Common Lisp library under the *Simplified BSD License*,
dedicated to give some functiona like macros, and functions, but without
any purity.

You can find the documentation in the souce code. And practica examples in the
testing ``t`` directory.

## What gives to you?
### utils.lisp 

* ``ifte``: A more conventional if then else, found in conventional languages.
* ``if-not``
* ``lazy``,``with-lazy``,``lazy-force``, ``lazy-make``, ``lazyp`` and
  ``lazy-force``: Bringin to you some lazy functions
* ``/>``, ``//>``: foward pipelining
* ``</``, ``<//``: backward pipelining
* ``memoize``: generate a morization function from an existing one
* ``comp-case``: ``case`` with a custom function
* ``let-when``

### data-structures
It aims to simplify the data structures manipulation

* ``mk#`` and ``mk#!``, for hash-table creation and modification, that allows
  simple creation of hash tables.

### tco.lisp

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
