(in-package "GLI")

;;;; Module TYPES

;;; Copyright (c) 1999 The ThinLisp Group
;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: Jim Allard






;;;; Lisp Type Manipulation




;;; The macro `gl:deftype' defines an expander from one type specification to
;;; another, where it is presumed that the type expanded into is a "simpler"
;;; type closer to the what is natively supported within the GL language.  Types
;;; declared with gl:deftype can be used anywhere that natively supported types
;;; can be used.

(defmacro declared-type-expansion (name)
  `(get ,name :declared-type-expander))

(defmacro gl:deftype (name arglist &rest body)
  (let ((type-expander (intern (format nil "~a-TYPE-EXPANDER" name))))
    (unless (eval-feature :translator)
      (multiple-value-bind (decls forms)
	  (split-declarations-and-body body)
	`(progn
	   (deftype ,name ,arglist ,@body)
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defun ,type-expander ,arglist
	       ,@decls
	       (block ,name
		 ,@forms))
	     (setf (declared-type-expansion ',name) ',type-expander)))))))

(defun expand-type (type)
  (let* ((cons-type? (consp type))
	 (type-name (if cons-type? (cons-car type) type))
	 (expander? (declared-type-expansion type-name)))
    (setq type 
	  (if expander?
	      (expand-type
		(if cons-type?
		    (apply expander? (cons-cdr type))
		    (funcall expander?)))
	      type))
    (cond
      ((and (consp type)
	    (memqp (cons-car type) '(array simple-array)))
       (if (cons-cdr type)
	   `(,(cons-car type) ,(expand-type (cons-cadr type)) ,@(cddr type))
	 type))
      ((and (consp type)
	    (memqp (cons-car type) '(and or not unsigned-byte)))
       (cons (cons-car type)
	     (loop for subtype in (cons-cdr type)
		 collect (if (integerp subtype)
			     subtype
			   (expand-type subtype)))))
      ((and (consp type)
	    (eq (cons-car type) 'function))
       (list 'function
	     (loop for arg-type in (cons-second type)
		   collect (expand-type arg-type))
	     (expand-type (cons-third type))))
      (t type))))




;;; The macro `explicit-lisp-to-c-type-p' returns whether or not a Lisp type is
;;; of the form (c-type x), which is an explicit interface within the Lisp type
;;; system to the C types.

(defmacro explicit-lisp-to-c-type-p (lisp-type)
  (let ((type (gensym)))
    `(let ((,type ,lisp-type))
       (and (consp ,type)
	    (eq (cons-car ,type) 'c-type)))))




;;; This module contains operations for manipulating types within GLT.  In many
;;; cases the operations used here will be based on the underlying Lisp's type
;;; system.  Note that these operations are not the GL supported operations for
;;; use in runtime systems.  Those will be supplied in the GLRTL system under
;;; the GL package, not the GLI package.

;;; The function `gl-subtypep' should be used instead of subtypep so we can fix
;;; problems in underlying Lisp implementations as needed.  Similarly, we should
;;; use `gl-typep' instead of typep and `gl-type-of' instead of type-of.

(defun gl-subtypep (subtype superior-type)
  (setq subtype (expand-type subtype))
  (setq superior-type (expand-type superior-type))
  (cond ((eq superior-type 'void)
	 (values t t))
	((eq superior-type '*)
	 (values
	   (type-includes-values-p subtype)
	   t))
	;; Types of the form (c-type "long") or (c-type (pointer "char")) are
	;; subtypes if the C type specifiers are equal.
	((or (explicit-lisp-to-c-type-p superior-type)
	     (explicit-lisp-to-c-type-p subtype))
	 (values
	   (and (consp superior-type)
		(consp subtype)
		(eq (cons-car superior-type) (cons-car subtype))
		(equal (cons-cadr superior-type) (cons-cadr subtype)))
	   t))
	((and (consp superior-type)
	      (eq (cons-car superior-type) 'values))
	 (values
	   (or (and (eq subtype '*)
		    (loop for value-type in (cons-cdr superior-type)
			  always (gl-subtypep 't value-type)))
	       (and (consp subtype)
		    (eq (cons-car subtype) 'values)
		    (loop for superior-value-type in (cons-cdr superior-type)
			  for subtype-value-type-cons?
			      = (cons-cdr subtype)
			      then (cdr subtype-value-type-cons?)
			  always (gl-subtypep (or (car subtype-value-type-cons?)
						  'null)
					      superior-value-type))))
	   t))
	((or (memqp superior-type
		    '(unbound managed-float gl-string-stream file-stream))
	     (memqp subtype '(unbound)))
	 (values (eq subtype superior-type) t))
	;; Since all of our arrays are one-dimensional, all array types are
	;; sequences.  In CL, only vectors and simple-arrays are subtypes of
	;; sequence.
	((eq superior-type 'sequence)
	 (multiple-value-bind (result certainp)
	     (gl-subtypep subtype 'array)
	   (if result
	       (values result certainp)
	     (gl-subtypep subtype 'list))))
	((eq subtype 'void)
	 (values nil t))
	((eq subtype '*)
	 (gl-subtypep 't superior-type))
	((and (consp subtype)
	      (eq (cons-car subtype) 'values))
	 (gl-subtypep (or (second subtype) 'null) superior-type))
	((eq subtype 'managed-float)
	 (values (eq superior-type 't) t))
	((memqp subtype '(gl-string-stream file-stream))
	 (values (memqp superior-type '(stream t)) t))
	(t
	 (subtypep subtype superior-type))))

(defconstant most-positive-gl-fixnum 536870911)

(defconstant most-negative-gl-fixnum -536870912)

(defstruct (gl-string-stream)
  (strings nil)
  (input-string)
  (input-index)
  (input-index-bounds))

(defun gl-typep (object type)
  (setq type (expand-type type))
  (cond ((memqp type '(void * t))
	 t)
	((eq type 'unbound)
	 nil)
	;; Lucid doesn't implement the types string-stream or file-stream.  -jra
	;; 3/4/96 However, CMU Lisp and other ANSI CL implementations do, and on
	;; those Lisps the interpretation of the type string-stream was messing
	;; us up.  Instead, switch over to gl-string-stream, which is the
	;; structure type that will actually implement this functionality in GL.
	;; -jallard 5/27/99
	((eq type 'gl-string-stream)
	 (typep object 'gl-string-stream))
	((eq type 'managed-float)
	 (and (typep object '(array double-float))
	      (= (length object) 1)))
	#+lucid
	((eq type 'file-stream)
	 (and (typep object 'stream)
	      (not (typep object 'lcl:string-output-stream))))
	;; If we run on a non-30-bit fixnum implementation, comment this in.
	;; -jra 3/4/96
;	((eq type 'fixnum)
;	 (and (typep object 'integer)
;	       (<= object most-positive-gl-fixnum)
;	       (>= object most-negative-gl-fixnum)))
	((and (consp type)
	      (eq (cons-car type) 'values)
	      (= (length type) 2))
	 (gl-typep object (cons-second type)))
	((explicit-lisp-to-c-type-p type)
	 nil)
	(t
	 (typep object type))))




;;; The macro `fixnump' checks if the given value is a fixnum in GL translated
;;; systems.

(defmacro fixnump (x)
  `(gl-typep ,x 'fixnum))

(defmacro gl-type-of (object)
  `(type-of ,object))




;;; The parameter `gl-optimizable-types' holds an alist of Lisp types to C type
;;; implementations to optimize the given type when that value is stored in a
;;; lexcial variable or appears as a result of an expression evaluation.

(defparameter gl-optimizable-types
  '(((unsigned-byte 8) . uint8)
    ((unsigned-byte 16) . uint16)
    (fixnum . sint32)
    (double-float . double)
    (character . unsigned-char)
    ((array (unsigned-byte 8)) . (array uint8))
    ((array (unsigned-byte 16)) . (array uint16))
    ((array double-float) . (array double))
    (simple-string . (array unsigned-char))
    (string . (array unsigned-char))
    (simple-vector . (array Obj))))




;;; The parameter `gl-significant-types' is a list of Lisp types that are
;;; significant to the optimizations possible in GL.  This list can be added to
;;; arbitrarily, but it is included here so that the translator can collapse
;;; pathological combinations of ands, ors, and nots into a simple enough type
;;; spec that it can be handled easily.  Effectively this list is used to limit
;;; the type inference insanity possible within Common Lisp.

;;; Note that all types in this list must either have a single type tag or must
;;; have a special case within translate-type-check-predicate.

(defparameter gl-significant-types
  (nconc
    (loop for (type) in gl-optimizable-types
	  collect type)
    '(null
      cons
      list
      managed-float
      symbol
      compiled-function
      package
      gl-string-stream
      file-stream)))
      
      



;;; The function `most-specific-common-supertype' takes two types and returns a
;;; type that the most specific type that is superior to both given types.  It
;;; first attempts to use one or the other of the given types, and then searches
;;; through a set of types that GL knows how to optimize in runtime systems.  If
;;; all of this fails, this function returns T, the supertype of all types.
;;; This function is useful when combining the types from multiple expressions
;;; to determine the most specific type that could result from the evaluation of
;;; one or the other expression.

;;; When this function is used during the type combinations during a
;;; translation, there is no way to find a common supertype for types that
;;; differ in whether or not they set the values count.  In those cases this
;;; function will signal an error.  However, this fucntion is also used before
;;; actual translations to try to find the best guess at Lisp function return
;;; types.  When used for this purpose, you can allow this function to upgrade
;;; from types that don't set the values count up to values count setting types
;;; by passing the optional ignore-values-mismatch argument.

(defun most-specific-common-supertype
    (type1 type2 &optional ignore-values-mismatch?)
  (cond ((equal type1 type2)
	 type1)
	((or (type-includes-values-p type1)
	     (type-includes-values-p type2))
	 (cond
	   ((or (not (type-includes-values-p type1))
		(not (type-includes-values-p type2)))
	    (cond
	      (ignore-values-mismatch? '*)
	      (t (error "Values setting mismatch attempting to combine ~s and ~s."
			type1 type2))))
	   ((or (eq type1 '*)
		(eq type2 '*)
		(/= (length type1) (length type2)))
	    '*)
	   (t
	    (cons 'values
		  (loop for subtype1 in (cons-cdr type1)
			for subtype2 in (cons-cdr type2)
			collect (most-specific-common-supertype
				  subtype1 subtype2))))))
	((gl-subtypep type1 type2)
	 type2)
	((gl-subtypep type2 type1)
	 type1)
	(t
	 (loop for type in gl-significant-types
	       when (and (gl-subtypep type1 type) (gl-subtypep type2 type))
	         return type
	       finally (return t)))))




;;; The function `duplicate-type-declaration' takes two types that have both
;;; been declared on the same variable, and returns a type sufficient for use in
;;; declaring this variable as the intersection of of the two types.  The
;;; trivial implementation of this operation would return the two types within
;;; an and, but that is not sufficient for optimization purposes.

(defun duplicate-type-declaration (type1 type2)
  (setq type1 (expand-type type1))
  (setq type2 (expand-type type2))
  (cond ((eq type1 '*)
	 type2)
	((eq type2 '*)
	 type1)
	((or (eq type2 'void) (gl-subtypep type1 type2))
	 type1)
	((or (eq type1 'void) (gl-subtypep type2 type1))
	 type2)
	(t
	 (loop for type in gl-significant-types
	       when (or (gl-subtypep type1 type) (gl-subtypep type2 type))
	         return type
	       finally (return t)))))




;;; The function `optimizable-type-of' takes a Lisp object and returns the most
;;; specific optimizable type that includes the given object.

(defun optimizable-type-of (object)
  (loop for type in gl-significant-types
	when (gl-typep object type)
	  return type
	finally (return t)))




;;; The function `upgraded-optimizable-type' takes a type and returns the most
;;; specific super type that is optimizable in GL.

(defun upgraded-optimizable-type (base-type)
  (loop with expanded-base-type = (expand-type base-type)
	initially (when (explicit-lisp-to-c-type-p expanded-base-type)
		    (return expanded-base-type))
	for type in gl-significant-types
	when (gl-subtypep expanded-base-type type)
	  return type
	finally (return t)))




;;; The parameter `upgraded-array-element-type-alist' holds the alist of Lisp
;;; types array element types to their equivalent GL package Lisp types and
;;; optimized C array element types, representing all array type optimizations
;;; supported within GL.

(defparameter upgraded-array-element-type-alist
  `((character          gl:character          unsigned-char)
    ((unsigned-byte 8)  (gl:unsigned-byte 8)  uint8)
    ((unsigned-byte 16) (gl:unsigned-byte 16) uint16)
    (double-float       gl:double-float       double)
    (t                  gl:t                  obj)))




;;; The function `upgraded-gl-array-element-type' takes an element type for an
;;; array and returns the specific superior type that has a specialized array
;;; implementation.  Note that the macro gl:upgraded-array-element-type uses
;;; this function.

(defun upgraded-gl-array-element-type (element-type)
  (second (assoc (expand-type element-type) upgraded-array-element-type-alist
		 :test #'gl-subtypep)))




;;; The function `c-type-for-lisp-type' takes a Lisp type and returns the C type
;;; that should be used in translations to represent it.  (See C-TYPES for a
;;; description of C types.)

(defun c-type-for-lisp-type (lisp-type)
  ;; The cond clauses optimize the most commonly used cases.
  (setq lisp-type (expand-type lisp-type))
  (cond
    ((eq lisp-type 't) 'obj)
    ((eq lisp-type 'fixnum) 'sint32)
    ((eq lisp-type 'simple-vector) '(array obj))
    ((explicit-lisp-to-c-type-p lisp-type)
     ;; The C type for Lisp types of the form (c-type *) are EQ to that Lisp
     ;; type.  (Got you confused yet?  -jallard 7/7/97)
     (cond ((gl-subtypep lisp-type '(c-type "long"))
	    'long)
	   ((gl-subtypep lisp-type '(c-type (pointer "char")))
	    '(pointer char))
	   ((gl-subtypep lisp-type '(c-type (pointer "void")))
	    '(pointer void))
	   ((gl-subtypep lisp-type '(c-type (pointer "uint32")))
	    '(pointer uint32))
	   (t
	    lisp-type)))
    (t
     (loop for (optimizable-type . c-type) in gl-optimizable-types
	   do
       (when (gl-subtypep lisp-type optimizable-type)
	 (return c-type))
	   finally (return 'obj)))))



    
;;; The function `c-func-type-for-lisp-func-return-type-spec' takes a Lisp
;;; function return type specification as would be found in the ftype or
;;; computed-ftype specification, and returns the appropriate C function return
;;; type for a translation of that Lisp function.  Note that the returned C type
;;; is the type of only the first value returned by the C function, since all
;;; secondary values are returned as type Obj within the values-buffer global
;;; array.

(defun c-func-type-for-lisp-func-return-type-spec (lisp-type-spec)
  (cond
    ;; First deal with the non-Lisp types we use to describe return values not
    ;; expressible within the Lisp type system.

    ;; The return spec * is used for functions that return an unpredictable
    ;; number of values.
    ((eq lisp-type-spec '*)
     'obj)
    ;; The return spec void is used for functions that return no useful value.
    ((eq lisp-type-spec 'void)
     'void)
    ;; The return spec (values <type> ...) describes a specific number of
    ;; multiple values of given Lisp types, always implemented as C Obj types.
    ((and (consp lisp-type-spec) (eq (cons-car lisp-type-spec) 'values))
     'obj)
    ;; Otherwise it is a specific, potentially optimizable type.
    (t
     (c-type-for-lisp-type lisp-type-spec))))




;;; The function `satisfies-lisp-required-type-p' takes a Lisp return type and a
;;; Lisp required type.  This function returns whether or not the return type
;;; satisfies the required type or would need further type checking or coercion
;;; to provide the required type.  Note that this function is strict, that the
;;; return type must fully satisfy the type requirements of the required type.
;;; Also note that the requirement of multiple values means that the return type
;;; must either be * or a (values ...) type.  This function is used to determine
;;; whether or not further operations need to be wrapped around a value
;;; producing form to set the values count.

(defun satisfies-lisp-required-type-p (result-type required-type)
  (or (eq required-type 'void)
      (and (eq required-type 't) (not (eq result-type 'void)))
      (equal required-type result-type)
      (and (eq required-type '*)
	   (type-includes-values-p result-type))
      (and (consp required-type)
	   (eq (cons-car required-type) 'values)
	   (if (and (consp result-type)
		    (eq (cons-car result-type) 'values))
	       (loop for subtype in (cons-cdr required-type)
		     for result-type-cons = (cons-cdr result-type)
					  then (cdr result-type-cons)
		     for result-subtype = (or (car result-type-cons) 'null)
		     always (gl-subtypep subtype result-subtype))
	       (and (eq result-type '*)
		    (loop for subtype in (cons-cdr required-type)
			  always (gl-subtypep 't subtype)))))
      (and (not (values-of-t-type-p required-type))
	   (gl-subtypep (type-of-first-value result-type) required-type))))




;;; The function `values-of-t-type-p' takes a type specification and returns
;;; true if it is a (values t ...) type.  The function `type-of-first-value'
;;; takes a type and returns either the type, or if the given type returns
;;; multiple values, it returns the type of the first returned value.

(defun values-of-t-type-p (type)
  (and (consp type)
       (eq (cons-car type) 'values)
       (loop for subtype in (cons-cdr type)
	     always (eq subtype 't))))

(defun type-of-first-value (type)
  (cond ((atom type)
	 (if (eq type '*)
	     t
	     type))
	((eq (cons-car type) 'values)
	 (or (second type) 'null))
	(t type)))




;;; The function `type-includes-values-p' takes a Lisp type and returns whether
;;; or not the type specifies its multiple values requirements.

(defun type-includes-values-p (type)
  (or (eq type '*)
      (and (consp type)
	   (eq (cons-car type) 'values))))





;;; The function `default-init-for-lisp-type' takes a Lisp type and returns the
;;; default initial value for variables of the given type, where they have been
;;; given no explicit initial value.

(defun default-init-for-lisp-type (lisp-type)
  (cond ((gl-subtypep lisp-type 'fixnum) 0)
	((gl-subtypep lisp-type 'double-float) 0.0)
	((gl-subtypep lisp-type 'character) #\null)
	(t nil)))
