(in-package "GL")

;;;; Module GL-TYPES

;;; Copyright (c) 1999 The ThinLisp Group
;;; Copyright (c) 1996 Gensym Corporation.
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






;;;; Type Operations for GL




;;; This module implements type predicates and declarations.  There are also
;;; two variables needed by the memory allocation primitives.

(defvar gli::current-region 0)

(defvar gli::temporary-area-top nil)

(deftype void () 'gli::void)

(deftype fixnum () 'gli::fixnum)

(defconstant most-positive-fixnum 536870911)

(defconstant most-negative-fixnum -536870912)

(defmacro fixnump (object)
  `(typep ,object 'gli::fixnum))

(deftype null () 'gli::null)

(defmacro null (object)
  `(not ,object))

(deftype symbol () 'gli::symbol)

(defmacro symbolp (object)
  `(typep ,object 'gli::symbol))

(deftype atom () 'gli::atom)

(defmacro atom (object)
  `(not (consp ,object)))

(deftype cons () 'gli::cons)

(defmacro consp (object)
  `(typep ,object 'gli::cons))

(deftype list () 'gli::list)

(defmacro listp (object)
  `(typep ,object 'list))

(deftype number () 'gli::number)

(defmacro numberp (object)
  `(typep ,object 'gli::number))




;;; Note that we are immoral scum and are ignoring the ranges handed in.  All
;;; values declared as integers in this form are presumed to be just fixnums.
;;; Note that this would be OK, but we also do in fact attempt to pay attention
;;; to the unsigned-byte types by NOT expanding them.  When we have spare time
;;; (HA!) we should do the moral thing, expand all other integer type
;;; specifications into an integer range spec, and then fix up gl-typep and
;;; gl-subtypep to explicitly deal with the integer types.  -jra 1/8/97

(deftype integer (&optional low-bound high-bound)
  (declare (ignore low-bound high-bound))
  'gli::fixnum)

(defmacro integerp (object)
  `(typep ,object 'gli::fixnum))

(deftype unsigned-byte (bit-length)
  `(gli::unsigned-byte ,bit-length))

(deftype float () 'gli::double-float)

; (deftype managed-float () 'gli::managed-float)

(deftype double-float () 'gli::double-float)

(deftype gensym-float () 'double-float)

(defmacro gensym-float-type ()
  ''double-float)

(defmacro floatp (object)
  `(typep ,object 'gli::double-float))

(deftype character () 'gli::character)

(deftype string-char () 'gli::character)

(defmacro characterp (object)
  `(typep ,object 'gli::character))

(deftype simple-vector () 'gli::simple-vector)

(defmacro simple-vector-p (object)
  `(typep ,object 'gli::simple-vector))

(deftype string () 'gli::string)

(deftype simple-string () 'gli::string)

(defmacro stringp (object)
  `(typep ,object 'gli::string))

(defmacro simple-string-p (object)
  `(typep ,object 'gli::string))

(deftype package () 'gli::package)

(defmacro packagep (object)
  `(typep ,object 'gli::package))

(deftype compiled-function () 'gli::compiled-function)

(defmacro compiled-function-p (object)
  `(typep ,object 'gli::compiled-function))

(deftype stream () 'gli::stream)

(defmacro streamp (object)
  `(typep ,object 'gli::stream))

(deftype string-stream () 'gli::gl-string-stream)

(deftype file-stream () 'gli::file-stream)

(deftype and (&rest types)
  `(gli::and ,@types))

(deftype or (&rest types)
  `(gli::or ,@types))

(deftype not (type)
  `(gli::not ,type))

(deftype values (&rest types)
  `(gli::values ,@types))

;;; All GL arrays are vectors and are considered simple-arrays.

(defun-for-macro expand-array-type (original-array-type array-specs)
  (if array-specs
      (let ((elt-type (car array-specs))
	    (dimensions (car (cdr array-specs))))
	(when (cdr (cdr array-specs))
	  (ab-lisp::error
	    "Malformed array type spec ~s"
	    (cons original-array-type array-specs)))
	(cond ((or (null dimensions)
		   (lisp:eql dimensions 1)
		   (lisp:equal dimensions '(*))
		   (lisp:equal dimensions '(gli::*)))
	       `(gli::array ,elt-type))
	      ((and (consp dimensions)
		    (null (cdr dimensions))
		    (fixnump (car dimensions)))
	       `(gli::array ,elt-type ,dimensions))
	      (t
	       (ab-lisp::error
		 "GL doesn't support multi-dimensional arrays in type ~s."
		 (cons original-array-type array-specs)))))
      'gli::array))

(deftype vector (&optional (elt-type t) (length '*))
  (expand-array-type 'vector `(,elt-type (,length))))

(deftype simple-array (&optional (elt-type t) (dimensions '(*)))
  (expand-array-type 'simple-array `(,elt-type ,dimensions)))

(deftype array (&optional (elt-type t) (dimensions '(*)))
  (expand-array-type 'array `(,elt-type ,dimensions)))

(deftype c-type (held-c-type)
  `(gli::c-type 
     ,(cond ((stringp held-c-type)
	     held-c-type)
	    ((and (consp held-c-type)
		  (consp (cdr held-c-type))
		  (stringp (car (cdr held-c-type))))
	     (let ((car (car (the cons held-c-type))))
	       (if (or (eq car 'pointer)
		       (eq car 'gli::pointer))
		   `(gli::pointer ,(car (cdr held-c-type)))
		   (ab-lisp::error
		     "Badly formed C-TYPE, ~s"
		     `(c-type ,held-c-type)))))
	    (t
	     (ab-lisp::error
	       "Badly formed C-TYPE, ~s"
	       `(c-type ,held-c-type))))))






;;;; Macro-type Subtyping Operations




;;; The following operations will only be available during macro-expand and
;;; compile time, and are not translatable runtime functions.

(defun-for-macro upgraded-array-element-type (type)
  (gli::upgraded-gl-array-element-type type))

(defun-for-macro subtypep (subtype superior-type &optional environment)
  (declare (ignore environment))
  (gli::gl-subtypep subtype superior-type))
