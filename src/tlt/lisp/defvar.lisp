(in-package "GLI")

;;;; Module DEFVAR

;;; Copyright (c) 1996 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Variable Definitions




;;; This module implements the macros `gl:defvar', `gl:defparameter', and
;;; `gl:defconstant'.  Defvar and defparameter define global variables that are
;;; translated into C variables.  Defconstant defines pointers to variable
;;; values that are always inlined into the referencing locations.

(defmacro gl:defvar (name &optional (initial-value no-initial-value)
			  (documentation nil))
  (declare (ignore documentation))
  `(gl:progn
     (gl:declaim (special ,name))
     (def-named-variable ,name :variable ,initial-value)))
			  
(defmacro gl:defparameter (name initial-value)
  `(gl:progn
     (gl:declaim (special ,name))
     (def-named-variable ,name :parameter ,initial-value)))

(defmacro gl:defconstant (name initial-value)
  `(gl:progn
     (gl:declaim (constant ,name))
     (def-named-variable ,name :constant ,initial-value)))




;;; The macro `def-translatable-lisp-var' is used to define a variable for GL on
;;; a symbol that is an underlying Lisp implementation variable.  An example is
;;; *features*.  We must use the Lisp package version so that the reader can
;;; interpret #+ reader macros appropriately, but we want to distinguish a use
;;; of this variable from a use of some other Lisp variable that we do not have
;;; a definition or translation for.  This macro does the trick, standing in for
;;; a defvar, as far as the translator is concerned, but while still using the
;;; previously defined defvar while in development.

(defmacro gl:def-translatable-lisp-var
    (name &optional (initial-value no-initial-value))
  `(gl:progn
     (gl:declaim (special ,name))
     (def-named-variable ,name :underlying-lisp-variable ,initial-value)))

(defmacro def-translatable-lisp-constant
    (name initial-value)
  `(gl:progn
     (gl:declaim (constant ,name))
     (def-named-variable ,name :underlying-lisp-constant ,initial-value)))
