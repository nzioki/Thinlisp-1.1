(in-package "GLI")

;;;; Module GL-PRIM

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






;;;; Primitive Operations for GL




;;; This module implements facilities in GL that have direct translations into C
;;; code or are present at compile time only.






;;;; Arrays

(gl:declaim (gl:functional length-trans gl:fill-pointer gl:aref gl:elt
			   gl:svref gl:schar))

(gl:define-compiler-macro gl:length (sequence)
  `(length-trans ,sequence))

(def-c-translation length-trans (sequence)
  ((lisp-specs :ftype ((t) fixnum))
   `(length ,sequence))
  ((trans-specs :lisp-type ((simple-vector) fixnum)
		:c-type (((pointer sv)) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr sequence "length")))
  ((trans-specs :lisp-type ((string) fixnum)
		:c-type (((pointer str)) sint32))
   (make-c-cast-expr
    'sint32 (make-c-indirect-selection-expr sequence "fill_length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 8))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint8) sequence)
	       "fill_length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 16))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint16) sequence)
	       "fill_length")))
  ((trans-specs :lisp-type (((array double-float)) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-double) sequence)
	       "length")))
  ((trans-specs :lisp-type ((t) fixnum)
		:c-type ((obj) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "length" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "length") (list sequence))))

(def-gl-macro gl:array-dimension (vector axis)
  (unless (eql axis 0)
    (error "Arrays in GL are all vectors, so the axis must be 0."))
  `(array-dimension-1 ,vector))

(def-gl-macro gl:array-total-size (vector)
  `(array-dimension-1 ,vector))

(def-c-translation array-dimension-1 (vector)
  ((lisp-specs :ftype ((t) fixnum))
   `(array-dimension ,vector 0))
  ((trans-specs :lisp-type ((simple-vector) fixnum)
		:c-type (((pointer sv)) sint32))
   (make-c-cast-expr 'sint32 (make-c-indirect-selection-expr vector "length")))
  ((trans-specs :lisp-type ((string) fixnum)
		:c-type (((pointer str)) sint32))
   (make-c-cast-expr 'sint32 (make-c-indirect-selection-expr vector "length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 8))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint8) vector)
	       "length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 16))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint16) vector)
	       "length")))
  ((trans-specs :lisp-type (((array double-float)) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-double) vector)
	       "length")))
  ((trans-specs :lisp-type ((t) fixnum)
		:c-type ((obj) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32
     "generic_array_dimension" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_array_dimension") (list vector))))

(def-c-translation gl:fill-pointer (vector)
  ((lisp-specs :ftype ((array) fixnum))
   `(fill-pointer ,vector))
  ((trans-specs :lisp-type ((string) fixnum)
		:c-type (((pointer str)) sint32))
   (make-c-cast-expr
    'sint32 (make-c-indirect-selection-expr vector "fill_length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 8))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint8) vector)
	       "fill_length")))
  ((trans-specs :lisp-type (((array (unsigned-byte 16))) fixnum)
		:c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint16) vector)
	       "fill_length")))
  ((trans-specs :lisp-type ((t) fixnum)
		:c-type ((obj) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "generic_fill_pointer" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_fill_pointer") (list vector))))

(gl:defsetf gl:fill-pointer set-fill-pointer)

(def-c-translation set-fill-pointer (vector new-fill-pointer)
  ((lisp-specs :ftype ((t fixnum) fixnum))
   (let ((vector-var (gensym))
	 (new-fill-pointer-var (gensym)))
     `(let ((,vector-var ,vector)
	    (,new-fill-pointer-var ,new-fill-pointer))
	(when (and (stringp ,vector-var)
		   (< ,new-fill-pointer-var
		      (array-dimension ,vector-var 0)))
	  (setf (char ,vector-var ,new-fill-pointer-var)
		#\null))
	(setf (fill-pointer ,vector-var) ,new-fill-pointer-var))))
  ((trans-specs :lisp-type ((simple-vector fixnum) fixnum)
		:c-type ((obj sint32) sint32))
   (translation-error
     "Cannot set the fill-pointer of a simple-vector, it doesn't have one."))
  ((trans-specs :lisp-type ((string fixnum) fixnum)
		:c-type (((pointer str) sint32) sint32))
   (let* ((env (l-expr-env function-call-l-expr))
	  (string-ref
	    (reusable-c-variable-identifier 'string c-func '(pointer str) env))
	  (fill-ref?
	    (unless (c-name-expr-p new-fill-pointer)
	      (reusable-c-variable-identifier 'fill c-func 'sint32 env))))
     (emit-expr-to-compound-statement
       (make-c-infix-expr (make-c-name-expr string-ref) "=" vector)
       c-compound-statement)
     (when fill-ref?
       (emit-expr-to-compound-statement
	 (make-c-infix-expr (make-c-name-expr fill-ref?) "=" new-fill-pointer)
	 c-compound-statement))
     (emit-expr-to-compound-statement
       (make-c-infix-expr
	 (make-c-subscript-expr
	   (make-c-indirect-selection-expr (make-c-name-expr string-ref) "body")
	   (if fill-ref?
	       (make-c-name-expr fill-ref?)
	       new-fill-pointer))
	 "=" (make-c-literal-expr (code-char 0)))
       c-compound-statement)
     (make-c-infix-expr
       (make-c-indirect-selection-expr
	 (make-c-name-expr string-ref) "fill_length")
       "=" (if fill-ref?
	       (make-c-name-expr fill-ref?)
	       new-fill-pointer))))
  ((trans-specs :lisp-type (((array (unsigned-byte 8)) fixnum) fixnum)
		:c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sa-uint8) vector)
       "fill_length")
     "=" new-fill-pointer))
  ((trans-specs :lisp-type (((array (unsigned-byte 16)) fixnum) fixnum)
		:c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sa-uint16) vector)
       "fill_length")
     "=" new-fill-pointer))
  ((trans-specs :lisp-type (((array double-float) fixnum) fixnum)
		:c-type ((obj sint32) sint32))
   (translation-error
     "The type (array double-float) doesn't have a fill-pointer."))
  ((trans-specs :lisp-type ((t fixnum) fixnum)
		:c-type ((obj sint32) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "generic_set_fill_pointer"
     '(obj sint32))
   (make-c-function-call-expr
     (make-c-name-expr "generic_set_fill_pointer")
     (list vector new-fill-pointer))))

(def-c-translation gl:elt (sequence index)
  ((lisp-specs :ftype ((sequence fixnum) t))
   `(elt ,sequence ,index))
  ((trans-specs :lisp-type ((list fixnum) t)
		:c-type ((obj sint32) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "nth" '(sint32 obj))
   (make-c-function-call-expr (make-c-name-expr "nth")
			      (list index sequence)))
  ((trans-specs :lisp-type ((simple-vector fixnum) t)
		:c-type (((array obj) sint32) obj))
   (make-c-subscript-expr sequence index))
  ((trans-specs :lisp-type ((string fixnum) character)
		:c-type (((array unsigned-char) sint32) unsigned-char))
   (make-c-subscript-expr sequence index))
  ((trans-specs :lisp-type (((array (unsigned-byte 8)) fixnum) fixnum)
		:c-type (((array uint8) sint32) uint8))
   (make-c-subscript-expr sequence index))
  ((trans-specs :lisp-type (((array (unsigned-byte 16)) fixnum) fixnum)
		:c-type (((array uint16) sint32) uint16))
   (make-c-subscript-expr sequence index))
  ((trans-specs :lisp-type (((array double-float) fixnum) double-float)
		:c-type (((array double) sint32) double))
   (make-c-subscript-expr sequence index))
  ((trans-specs :lisp-type ((sequence fixnum) t)
		:c-type ((obj sint32) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'gl:elt
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_elt" '(obj sint32))
   (make-c-function-call-expr
     (make-c-name-expr "generic_elt") (list sequence index))))

(def-c-translation set-elt (sequence index value)
  ((lisp-specs :ftype ((sequence fixnum t) t))
   `(setf (elt ,sequence ,index) ,value))
  ((trans-specs :lisp-type ((list fixnum t) t)
		:c-type ((obj sint32 obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "nthcdr" '(sint32 obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (coerce-c-expr-result-to-type
	 (make-c-function-call-expr (make-c-name-expr "nthcdr")
				    (list index sequence))
	 'obj '(pointer cons) (l-expr-env function-call-l-expr))
       "car")
     "=" value))
  ((trans-specs :lisp-type ((simple-vector fixnum t) t)
		:c-type (((array obj) sint32 obj) obj))
   (make-c-infix-expr (make-c-subscript-expr sequence index) "=" value))
  ((trans-specs :lisp-type ((string fixnum character) character)
		:c-type (((array unsigned-char) sint32 unsigned-char)
			 unsigned-char))
   (make-c-infix-expr (make-c-subscript-expr sequence index) "=" value))
  ((trans-specs :lisp-type (((array (unsigned-byte 8)) fixnum fixnum)
			    fixnum)
		:c-type (((array uint8) sint32 uint8) uint8))
   (make-c-infix-expr (make-c-subscript-expr sequence index) "=" value))
  ((trans-specs :lisp-type (((array (unsigned-byte 16)) fixnum fixnum)
			    fixnum)
		:c-type (((array uint16) sint32 uint16) uint16))
   (make-c-infix-expr (make-c-subscript-expr sequence index) "=" value))
  ((trans-specs :lisp-type (((array double-float) fixnum double-float)
			    double-float)
		:c-type (((array double) sint32 double) double))
   (make-c-infix-expr (make-c-subscript-expr sequence index) "=" value))
  ((trans-specs :lisp-type ((sequence fixnum t) t)
		:c-type ((obj sint32 obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) "SET-ELT"
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_set_elt" '(obj sint32 obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_set_elt") (list sequence index value))))

(gl:defsetf gl:elt set-elt)

(def-c-translation gl:aref (array index)
  ((lisp-specs :ftype ((array fixnum) t))
   `(aref ,array ,index))
  ((trans-specs :lisp-type ((simple-vector fixnum) t)
		:c-type (((array obj) sint32) obj))
   (make-c-subscript-expr array index))
  ((trans-specs :lisp-type ((string fixnum) character)
		:c-type (((array unsigned-char) sint32) unsigned-char))
   (make-c-subscript-expr array index))
  ((trans-specs :lisp-type (((array (unsigned-byte 8)) fixnum) fixnum)
		:c-type (((array uint8) sint32) uint8))
   (make-c-subscript-expr array index))
  ((trans-specs :lisp-type (((array (unsigned-byte 16)) fixnum) fixnum)
		:c-type (((array uint16) sint32) uint16))
   (make-c-subscript-expr array index))
  ((trans-specs :lisp-type (((array double-float) fixnum) double-float)
		:c-type (((array double) sint32) double))
   (make-c-subscript-expr array index))
  ((trans-specs :lisp-type ((array fixnum) t)
		:c-type ((obj sint32) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'gl:aref
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_aref" '(obj sint32))
   (make-c-function-call-expr
     (make-c-name-expr "generic_aref") (list array index))))

(defparameter primitive-array-types
  ;; Format is:  ( array-type  array-element-type  type-name )
  ;; The type-name is to be used to create type-specific function names.
  '((simple-vector                    T                 simple-vector)
    (string                           character         string)
    ((array (unsigned-byte 8))        (unsigned-byte 8) array-unsigned-byte-8)
    ((array (unsigned-byte 16))       (unsigned-byte 16)array-unsigned-byte-16)
    ((array double-float)             double-float      array-double-float)))


(def-c-translation set-aref (array index value)
  ((lisp-specs :ftype ((array fixnum t) t))
   `(setf (aref ,array ,index) ,value))
  ((trans-specs :lisp-type ((simple-vector fixnum t) t)
		:c-type (((array obj) sint32 obj) obj))
   (make-c-infix-expr (make-c-subscript-expr array index) "=" value))
  ((trans-specs :lisp-type ((string fixnum character) character)
		:c-type (((array unsigned-char) sint32 unsigned-char)
			 unsigned-char))
   (make-c-infix-expr (make-c-subscript-expr array index) "=" value))
  ((trans-specs :lisp-type (((array (unsigned-byte 8)) fixnum fixnum)
			    fixnum)
		:c-type (((array uint8) sint32 uint8) uint8))
   (make-c-infix-expr (make-c-subscript-expr array index) "=" value))
  ((trans-specs :lisp-type (((array (unsigned-byte 16)) fixnum fixnum)
			    fixnum)
		:c-type (((array uint16) sint32 uint16) uint16))
   (make-c-infix-expr (make-c-subscript-expr array index) "=" value))
  ((trans-specs :lisp-type (((array double-float) fixnum double-float)
			    double-float)
		:c-type (((array double) sint32 double) double))
   (make-c-infix-expr (make-c-subscript-expr array index) "=" value))
  ((trans-specs :lisp-type ((array fixnum t) t)
		:c-type ((obj sint32 obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) "SET-AREF"
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_set_aref" '(obj sint32 obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_set_aref") (list array index value))))

(gl:defsetf gl:aref set-aref)

(def-c-translation gl:svref (simple-vector index)
  ((lisp-specs :ftype ((simple-vector fixnum) t))
   `(svref ,simple-vector ,index))
  ((trans-specs :c-type (((array obj) sint32) obj))
   (make-c-subscript-expr simple-vector index)))

(def-c-translation set-svref (simple-vector index value)
  ((lisp-specs :ftype ((simple-vector fixnum t) t))
   `(setf (svref ,simple-vector ,index) ,value))
  ((trans-specs :c-type (((array obj) sint32 obj) obj))
   (make-c-infix-expr (make-c-subscript-expr simple-vector index) "=" value)))

(gl:defsetf gl:svref set-svref)

(def-c-translation gl:schar (string index)
  ;; Note that within GL, schar can be applied to strings with fill-pointers.
  ;; This means that in development Lisp images, we must always use char to
  ;; implement this operation.
  ((lisp-specs :ftype ((string fixnum) character))
   `(char ,string ,index))
  ((trans-specs :c-type (((array unsigned-char) sint32) unsigned-char))
   (make-c-subscript-expr string index)))

(def-gl-macro gl:char (string index)
  `(gl:schar ,string ,index))

(def-c-translation set-schar (string index value)
  ;; Note that within GL, schar can be applied to strings with fill-pointers.
  ;; This means that in development Lisp images, we must always use char to
  ;; implement this operation.
  ((lisp-specs :ftype ((string fixnum character) character))
   `(setf (char ,string ,index) ,value))
  ((trans-specs :c-type (((array unsigned-char) sint32 unsigned-char)
			 unsigned-char))
   (make-c-infix-expr (make-c-subscript-expr string index) "=" value)))

(gl:defsetf gl:schar set-schar)




;;; The following provides direct access to the C strcmp facility, returning the
;;; integer values it does.  For reference, a negative value means string1 was
;;; less than string2, a zero means they are equal strings, and positive if
;;; string1 is greater than string2.

(gl:declaim (gl:functional string-compare))

(def-c-translation string-compare (string1 string2)
  ((lisp-specs :ftype ((string string) fixnum))
   (let ((str1 (gensym))
	 (str2 (gensym)))
     `(let ((,str1 ,string1)
	    (,str2 ,string2))
	(cond ((string< ,str1 ,str2) -1)
	      ((string= ,str1 ,str2) 0)
	      (t 1)))))
  ((trans-specs :c-type (((pointer unsigned-char) (pointer unsigned-char))
			 sint32))
   (make-c-function-call-expr
     (make-c-name-expr "strcmp")
     (list (make-c-cast-expr '(pointer char) string1)
	   (make-c-cast-expr '(pointer char) string2)))))




;;; The macro `gl:replace-strings' is a version of replace optimized for string
;;; copying.  Note that there is purposefully no :END1 argument.  There must be
;;; enough room in the first string to hold the values being copied, or else
;;; this operation will overwrite whatever object arbitrarily follows it in
;;; memory.

(def-gl-macro gl:replace-strings
    (to-string from-string &key (start1 0) (start2 0) end2)
  (let ((to (gensym))
	(from (gensym))
	(s1 (if (constantp start1) start1 (gensym)))
	(s2 (if (constantp start2) start2 (gensym)))
	(e2 (gensym)))
    `(gl:let* ((,to ,to-string)
	       (,from ,from-string)
	       ,@(if (symbolp s1) `((,s1 ,start1)))
	       ,@(if (symbolp s2) `((,s2 ,start2)))
	       (,e2 ,(or end2 `(length-trans ,from))))
       (gl:declare (string ,to ,from)
		   (fixnum ,@(if (symbolp s1) `(,s1))
			   ,@(if (symbolp s2) `(,s2))
			   ,e2))
       (replace-strings-1
	 ,to ,from ,s1 ,s2 ,(if (eql s2 0) e2 `(gl:- ,e2 ,s2)))
       ,@(unless (eql s1 0)
	   `(,to)))))

(def-c-translation replace-strings-1 (to from start1 start2 count)
  ((lisp-specs :ftype ((string string fixnum fixnum fixnum) string))
   `(replace ,to ,from :start1 ,start1 :start2 ,start2
	     :end2 (the fixnum (+ ,start2 ,count))))
  ((trans-specs :c-type (((pointer unsigned-char) (pointer unsigned-char)
			  sint32 sint32 sint32)
			 (pointer unsigned-char)))
   (make-c-function-call-expr
     (make-c-name-expr "memcpy")
     (list
       (make-c-cast-expr '(pointer void) (make-c-add-expr to "+" start1))
       (make-c-cast-expr '(pointer void) (make-c-add-expr from "+" start2))
       count))))

(def-gl-macro gl:replace-simple-vectors
    (to-simple-vector from-simple-vector &key (start1 0) (start2 0) end2)
  (let ((to (gensym))
	(from (gensym))
	(s1 (if (constantp start1) start1 (gensym)))
	(s2 (if (constantp start2) start2 (gensym)))
	(e2 (gensym)))
    `(gl:let* ((,to ,to-simple-vector)
	       (,from ,from-simple-vector)
	       ,@(if (symbolp s1) `((,s1 ,start1)))
	       ,@(if (symbolp s2) `((,s2 ,start2)))
	       (,e2 ,(or end2 `(length-trans ,from))))
       (gl:declare (simple-vector ,to ,from)
		   (fixnum ,@(if (symbolp s1) `(,s1))
			   ,@(if (symbolp s2) `(,s2))
			   ,e2))
       (replace-simple-vectors-1
	 ,to ,from ,s1 ,s2 ,(if (eql s2 0) e2 `(- ,e2 ,s2)))
       ,@(unless (eql s1 0)
	   `(,to)))))

(def-c-translation replace-simple-vectors-1 (to from start1 start2 count)
  ((lisp-specs :ftype ((simple-vector simple-vector fixnum fixnum fixnum)
		       simple-vector))
   `(replace ,to ,from :start1 ,start1 :start2 ,start2
	     :end2 (the fixnum (+ ,start2 ,count))))
  ((trans-specs :c-type (((pointer obj) (pointer obj) sint32 sint32 sint32)
			 (pointer obj)))
   (make-c-function-call-expr
     (make-c-name-expr "memcpy")
     (list
       ;; Let the C compiler multiply the start args by sizeof(Obj) as a
       ;; side-effect of adding them to the Obj* arrays.
       (make-c-cast-expr '(pointer void) (make-c-add-expr to "+" start1))
       (make-c-cast-expr '(pointer void) (make-c-add-expr from "+" start2))
       (make-c-infix-expr (make-c-sizeof-expr (c-type-string 'obj))
			  "*" count)))))




;;; The macro `gl:replace-uint16-arrays' is a version of replace optimized for
;;; arrays of (unsigned-byte 16).  Note that there is purposefully no :END1
;;; argument.  There must be enough room in the first array to hold the values
;;; being copied, or else this operation will overwrite whatever object
;;; arbitrarily follows it in memory.

(def-gl-macro gl:replace-uint16-arrays
    (to-array from-array &key (start1 0) (start2 0) end2)
  (let ((to (gensym))
	(from (gensym))
	(s1 (if (constantp start1) start1 (gensym)))
	(s2 (if (constantp start2) start2 (gensym)))
	(e2 (gensym)))
    `(gl:let ((,to ,to-array)
	      (,from ,from-array)
	      ,@(if (symbolp s1) `((,s1 ,start1)))
	      ,@(if (symbolp s2) `((,s2 ,start2)))
	      (,e2 ,(or end2 `(length ,from))))
       (gl:declare (type (array (unsigned-byte 16)) ,to ,from)
		   (fixnum ,@(if (symbolp s1) `(,s1))
			   ,@(if (symbolp s2) `(,s2))
			   ,e2))
       (replace-uint16-arrays-1
	 ,to ,from ,s1 ,s2 ,(if (eql s2 0) e2 `(gl:- ,e2 ,s2)))
       ,@(unless (eql s1 0)
	   `(,to)))))

(def-c-translation replace-uint16-arrays-1 (to from start1 start2 count)
  ((lisp-specs :ftype (((array (unsigned-byte 16)) (array (unsigned-byte 16))
			fixnum fixnum fixnum) (array (unsigned-byte 16))))
   `(replace ,to ,from :start1 ,start1 :start2 ,start2
	     :end2 (the fixnum (+ ,start2 ,count))))
  ((trans-specs :c-type (((pointer uint16) (pointer uint16)
			  sint32 sint32 sint32)
			 (pointer uint16)))
   (make-c-function-call-expr
     (make-c-name-expr "memcpy")
     (list
       (make-c-cast-expr
	 '(pointer void)
	 (make-c-add-expr to "+" start1))
       (make-c-cast-expr
	 '(pointer void)
	 (make-c-add-expr from "+" start2))
       (make-c-infix-expr count "*" 2)))))

(def-gl-macro gl:fill-string (string character &key (start nil) (end nil))
  (if (and (null start) (null end))
      (if (symbolp string)
	  `(gl:progn
	     (fill-string-1 ,string ,character 0
			    (length-trans (gl:the string ,string)))
	     ,string)
	  (let ((string-var (gensym)))
	    `(gl:let ((,string-var ,string))
	       (gl:declare (string ,string-var))
	       (fill-string-1
		 ,string-var ,character 0 (length-trans ,string-var))
	       ,string-var)))
      (let ((string-var (gensym))
	    (char-var (gensym))
	    (start-var (gensym))
	    (end-var (gensym)))
	`(gl:let* ((,string-var ,string)
		   (,char-var ,character)
		   (,start-var ,(or start 0))
		   (,end-var
		      ,(or end `(length-trans (gl:the string ,string-var)))))
	   (gl:declare (string ,string-var)
		       (character ,char-var)
		       (fixnum ,start-var ,end-var))
	   (fill-string-1 ,string-var ,char-var
			  ,start-var (- ,end-var ,start-var))
	   ,string-var))))

(def-c-translation fill-string-1 (string char start count)
  ((lisp-specs :ftype ((string character fixnum fixnum) void))
   ;; Note that gl:fill-string (the only caller for fill-string-1) guarantees
   ;; that I can eval the start argument twice.
   `(fill ,string ,char :start ,start :end (+ ,start ,count)))
  ((trans-specs :c-type (((pointer unsigned-char) unsigned-char sint32 sint32)
			 void))
   (make-c-function-call-expr
     (make-c-name-expr "memset")
     (list (make-c-cast-expr
	     '(pointer void) (make-c-infix-expr string "+" start))
	   char count))))

(gl:declaim (gl:functional search-string-1 position-in-string-1))

(def-c-translation search-string-1 (pattern searched start1 start2)
  ((lisp-specs :ftype ((string string fixnum fixnum) t))
   `(search ,pattern ,searched :start1 ,start1 :start2 ,start2))
  ((trans-specs :c-type (((pointer unsigned-char) (pointer unsigned-char)
			  sint32 sint32)
			 obj))
   (let ((result-var (reusable-c-variable-identifier
		       'strstr-result c-func '(pointer char)
		       (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr
	 (make-c-infix-expr
	   (make-c-name-expr result-var) "="
	   (make-c-function-call-expr
	     (make-c-name-expr "strstr")
	     (list (make-c-infix-expr
		     (make-c-cast-expr '(pointer char) searched)
		     "+" start1)
		   (make-c-infix-expr
		     (make-c-cast-expr '(pointer char) pattern)
		     "+" start2))))
	 "==" "NULL")
       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
       (coerce-c-expr-result-to-type
	 (make-c-infix-expr
	   (make-c-cast-expr 'uint32 (make-c-name-expr result-var))
	   "-"
	   (make-c-cast-expr 'uint32 searched))
	 'uint32 'obj (l-expr-env function-call-l-expr))))))

(def-c-translation position-in-string-1 (target-char searched start)
  ((lisp-specs :ftype ((character string fixnum) t))
   `(position ,target-char ,searched :start ,start))
  ((trans-specs :c-type ((unsigned-char (pointer unsigned-char) sint32) obj))
   (let ((result-var (reusable-c-variable-identifier
		       'strchr-result c-func '(pointer char)
		       (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr
	 (make-c-infix-expr
	   (make-c-name-expr result-var) "="
	   (make-c-function-call-expr
	     (make-c-name-expr "strchr")
	     (list (make-c-cast-expr '(pointer char)
				     (make-c-add-expr searched "+" start))
		   (make-c-cast-expr 'int target-char))))
	 "==" "NULL")
       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
       (coerce-c-expr-result-to-type
	 (make-c-infix-expr
	   (make-c-cast-expr 'uint32 (make-c-name-expr result-var))
	   "-"
	   (make-c-cast-expr 'uint32 searched))
	 'uint32 'obj (l-expr-env function-call-l-expr))))))

(defun l-expr-wants-unsigned-char-type-p (l-expr)
  (and (not (l-expr-constant-p l-expr))
       (satisfies-c-required-type-p
	 (uncoerced-l-expr-c-return-type l-expr) 'unsigned-char)))

(gl:declaim (gl:functional gl:char-code gl:code-char))

(def-c-translation gl:char-code (char)
  ((lisp-specs :ftype ((character) fixnum))
   `(char-code ,char))
  ;; Note that the existing type coercions will do exactly the right thing with
  ;; this.
  ((trans-specs :c-type ((unsigned-char) sint32))
   (make-c-cast-expr 'sint32 char)))

(def-c-translation gl:code-char (integer)
  ((lisp-specs :ftype ((fixnum) character))
   `(code-char ,integer))
  ((trans-specs :c-type ((sint32) unsigned-char))
   (make-c-cast-expr 'unsigned-char integer)))

(defmacro def-char-comparitors (lisp-and-c-op-pairs)
  (cons
    'progn
    (loop for (lisp-op c-op) in lisp-and-c-op-pairs
	  for lisp-sym = (intern (symbol-name lisp-op) *lisp-package*)
	  for gl-sym = (intern (format nil "TWO-ARG-~a" (symbol-name lisp-sym)))
	  append
	  `((gl:declaim (gl:functional ,gl-sym))
	    (def-c-translation ,gl-sym (char1 char2)
	     ((lisp-specs :ftype ((character character) t))
	      `(,',lisp-sym ,char1 ,char2))
	     ((trans-specs
		:test (or (l-expr-wants-unsigned-char-type-p char1-l-expr)
			  (l-expr-wants-unsigned-char-type-p char2-l-expr))
		:c-type ((unsigned-char unsigned-char) boolean))
	      (make-c-infix-expr char1 ,c-op char2))
	     ((trans-specs :c-type ((obj obj) boolean))
	      (make-c-infix-expr char1 ,c-op char2)))))))

(def-char-comparitors ((char=  "==")
		       (char/= "!=")
		       (char<  "<")
		       (char<= "<=")
		       (char>  ">")
		       (char>= ">=")))

(def-c-translation make-simple-vector (length)
  ((lisp-specs :ftype ((fixnum) simple-vector))
   `(make-array ,length))
  ((trans-specs :c-type ((sint32) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_simple_vector")
     (list length 
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'simple-vector
	       (declared-area-name
		 (l-expr-env function-call-l-expr) 'simple-vector)))
	   (make-c-literal-expr (c-type-tag 'sv))))))

(def-c-translation make-string-1 (length)
  ((lisp-specs :ftype ((fixnum) string))
   `(make-string-array ,length :fill-pointer t))
  ((trans-specs :c-type ((sint32) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_string")
     (list length 
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'string
	       (declared-area-name
		 (l-expr-env function-call-l-expr) 'string)))
	   (make-c-literal-expr (c-type-tag 'str))))))

(def-gl-macro gl:make-string (length &key (initial-element #\null)
				     (dont-initialize nil))
  (if dont-initialize
      `(make-string-1 ,length)
      (let ((new-string (gensym)))
	`(gl:let ((,new-string (make-string-1 ,length)))
	   (gl:declare (string ,new-string))
	   (gl:fill-string ,new-string ,initial-element)
	   ,new-string))))

(def-c-translation make-uint8-array (length)
  ((lisp-specs :ftype ((fixnum) (array (unsigned-byte 8))))
   `(make-array ,length :element-type '(unsigned-byte 8) :fill-pointer t))
  ((trans-specs :c-type ((sint32) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_uint8_array")
     (list length 
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       '(array (unsigned-byte 8))
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 '(array (unsigned-byte 8)))))
	   (make-c-literal-expr (c-type-tag 'sa-uint8))))))

(def-c-translation make-uint16-array (length)
  ((lisp-specs :ftype ((fixnum) (array (unsigned-byte 16))))
   `(make-array ,length :element-type '(unsigned-byte 16) :fill-pointer t))
  ((trans-specs :c-type ((sint32) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_uint16_array")
     (list length 
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       '(array (unsigned-byte 16))
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 '(array (unsigned-byte 16)))))
	   (make-c-literal-expr (c-type-tag 'sa-uint16))))))

(def-c-translation make-double-array (length)
  ((lisp-specs :ftype ((fixnum) (array double-float)))
   `(make-array ,length :element-type 'double-float))
  ((trans-specs :c-type ((sint32) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_double_array")
     (list length 
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       '(array double-float)
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 '(array double-float))))
	   (make-c-literal-expr (c-type-tag 'sa-double))))))

(def-gl-macro gl:make-array
    (&environment env dimensions &key (element-type t)
		  (initial-element nil element-supplied?)
		  initial-contents fill-pointer)
  (let* ((expanded-element-type (gl:macroexpand-all element-type env))
	 (upgraded-type
	   (if (constantp expanded-element-type)
	       (upgraded-gl-array-element-type (eval expanded-element-type))
	       (error "GL:make-array :element-type arguments must be constants, was ~s"
		      element-type)))
	 (array-var (gensym))
	 (index-var (gensym))
	 (iteration-length-var (gensym))
	 (initial-var (gensym))
	 (constant-length?
	   (and (constantp dimensions)
		(let ((dim (eval dimensions)))
		  (or (and (fixnump dim) dim)
		      (and (consp dim) (null (cdr dim)) (fixnump (car dim))
			   (car dim))
		      (error "Bad make-array dimensions: ~s" dimensions)))))
	 (fixnum-dims?
	   (gl-subtypep (expression-result-type dimensions env) 'fixnum)))
	       
    ;; We have no fast implementation for initial-contents, so we should always
    ;; complain when it is used, unless the user has already admitted that this
    ;; is fat-and-slow.
    (when initial-contents
      (fat-and-slow-warning
	env "INITIAL-CONTENTS argument to MAKE-ARRAY"
	initial-contents))
    ;; Some varieties of our arrays have fill-pointers always, and some don't.
    ;; If they ask for a fill-pointer on an array type that doesn't support it,
    ;; complain here, else just do the default thing.
    (when (and (memqp upgraded-type '(t gl:double-float))
	       fill-pointer)
      (error "Make-array with upgraded-array-element-type ~S can't have a fill-pointer."
	     upgraded-type))
    (multiple-value-bind (maker-function array-type)
	(array-maker-function-and-type-for-element-type upgraded-type)
      (let ((length-form
	      (cond (constant-length? constant-length?)
		    (fixnum-dims? dimensions)
		    (t `(gl::check-make-array-dimensions ,dimensions)))))
	(cond (element-supplied?
	       (if (eq array-type 'gl:string)
		   `(gl:make-string ,length-form :initial-element ,initial-element)
		 `(gl:let* ((,iteration-length-var ,length-form)
			    (,array-var (,maker-function ,iteration-length-var))
			    (,initial-var ,initial-element))
		   (gl:declare (gl:fixnum ,iteration-length-var)
			       (gl:type ,array-type ,array-var)
			       (gl:type ,upgraded-type ,initial-var))
		   (gl:dotimes (,index-var ,iteration-length-var)
		     (gl:declare (gl:fixnum ,index-var))
		     (gl:setf (gl:aref ,array-var ,index-var) ,initial-var))
		   ,array-var)))
	      (initial-contents
	       `(gl:let* ((,iteration-length-var ,length-form)
			  (,array-var (,maker-function ,iteration-length-var))
			  (,initial-var ,initial-contents))
		  (gl:declare (gl:fixnum ,iteration-length-var)
			      (gl:type ,array-type ,array-var))
		  (gl:dotimes (,index-var ,iteration-length-var)
		    (gl:declare (gl:fixnum ,index-var))
		    (gl:setf (gl:aref ,array-var ,index-var)
			     (gl:the ,upgraded-type
				     (gl:car (gl:the gl:cons ,initial-var))))
		    (gl:setq ,initial-var (gl:cdr (gl:the gl:cons ,initial-var))))
		  ,array-var))
	      (t
	       `(,maker-function ,length-form)))))))

(defun array-maker-function-and-type-for-element-type (upgraded-type)
  (cond ((eq upgraded-type 'gl:character)
	 (values 'make-string-1 'gl:string))
	((equal upgraded-type '(gl:unsigned-byte 8))
	 (values 'make-uint8-array '(gl:array (gl:unsigned-byte 8))))
	((equal upgraded-type '(gl:unsigned-byte 16))
	 (values 'make-uint16-array '(gl:array (gl:unsigned-byte 16))))
	((eq upgraded-type 'gl:double-float)
	 (values 'make-double-array '(gl:array gl:double-float)))
	((eq upgraded-type t)
	 (values 'make-simple-vector 'gl:simple-vector))
	(t
	 (error "Unrecognized upgraded-array-element-type ~s"
		upgraded-type))))	  







;;;; Managed-floats




;;; The type `gl:managed-float' has been added to GL to provide low level
;;; support for a type that can hold double-floats or a pointer to an object.
;;; The memory for these locations may be allowed to overlap if that aids in
;;; making these objects as small as possible.  The goal of this object is to
;;; store a floating point value as efficiently as possible while they are
;;; allocated and to store a pointer to the next managed-float in a resource
;;; pool while they are reclaimed.  The operations available on managed floats
;;; are `gl:make-managed-float', `gl:managed-float-value' (which is setfable),
;;; `gl:managed-float-next-object' (which is setfable), and
;;; `gl:managed-float-p'.

#+c-managed-floats
(def-c-translation gl:make-managed-float (new-value)
  ((lisp-specs :ftype ((double-float) managed-float))
   (let ((new-array (gensym))
	 (new-obj (gensym)))
     `(let* ((,new-array (make-array 1 :element-type 'double-float))
	     (,new-obj (cons ,new-array 'managed-float)))
	(setf (aref (the (array double-float) ,new-array) 0)
	      ,new-value)
	,new-obj)))
  ((trans-specs :c-type ((double) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_mdouble")
     (list new-value
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'managed-float
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 'managed-float)))
	   (make-c-literal-expr (c-type-tag 'mdouble))))))

#+c-managed-floats
(gl:declaim (gl:side-effect-free gl:managed-float-value))

#+c-managed-floats
(def-c-translation gl:managed-float-value (managed-float)
  ((lisp-specs :ftype ((managed-float) double-float))
   `(aref (the (array double-float) (car ,managed-float)) 0))
  ((trans-specs :c-type ((obj) double))
   (make-c-direct-selection-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer mdouble) managed-float)
       "body")
     "value")))

#+c-managed-floats
(gl:defsetf gl:managed-float-value set-managed-float-value)

#+c-managed-floats
(def-c-translation set-managed-float-value (managed-float new-value)
  ((lisp-specs :ftype ((managed-float double-float) double-float))
   (let ((mfloat (gensym)))
     `(let ((,mfloat ,managed-float))
	(setf (cdr ,mfloat) 'managed-float)
	(setf (aref (the (array double-float) (car ,mfloat)) 0)
	      ,new-value))))
  ((trans-specs :c-type ((obj double) double))
   (make-c-infix-expr
     (make-c-direct-selection-expr
       (make-c-indirect-selection-expr
	 (make-c-cast-expr '(pointer mdouble) managed-float)
	 "body")
       "value")
     "=" new-value)))

#-c-managed-floats
(def-gl-macro set-managed-float-value (managed-float new-value)
  `(gl:setf (gl:aref (gl:the (gl:array gl:double-float)
			     (gl:car (gl:the gl:cons ,managed-float)))
		     0)
	    (gl:the gl:double-float ,new-value)))

#+c-managed-floats
(gl:declaim (gl:side-effect-free gl:managed-float-next-object))

#+c-managed-floats
(def-c-translation gl:managed-float-next-object (managed-float)
  ((lisp-specs :ftype ((managed-float) t))
   `(cdr ,managed-float))
  ((trans-specs :c-type ((obj) obj))
   (make-c-direct-selection-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer mdouble) managed-float)
       "body")
     "next_object")))

#+c-managed-floats
(gl:defsetf gl:managed-float-next-object set-managed-float-next-object)

#+c-managed-floats
(def-c-translation set-managed-float-next-object (managed-float new-value)
  ((lisp-specs :ftype ((managed-float t) t))
   `(setf (cdr ,managed-float) ,new-value))
  ((trans-specs :c-type ((obj double) double))
   (make-c-infix-expr
     (make-c-direct-selection-expr
       (make-c-indirect-selection-expr
	 (make-c-cast-expr '(pointer mdouble) managed-float)
	 "body")
       "next_object")
     "=" new-value)))

#+c-managed-floats
(gl:declaim (gl:functional gl:managed-float-p))

#+c-managed-floats
(def-c-translation gl:managed-float-p (object)
  ((lisp-specs :ftype ((t) t))
   (let ((thing (gensym)))
     `(let ((,thing ,object))
	(and (consp ,thing)
	     (typep (car ,thing) '(array double-float))
	     (eq (cdr ,thing) 'managed-float)))))
  ((trans-specs :c-type ((obj) boolean))
   (if (c-name-expr-p object)
       (translate-type-check-predicate object 'managed-float 't)
       (let ((temp (reusable-c-variable-identifier
		     'temp c-func 'obj
		     (l-expr-env function-call-l-expr))))
	 (emit-expr-to-compound-statement
	   (make-c-infix-expr (make-c-name-expr temp) "=" object)
	   c-compound-statement)
	 (translate-type-check-predicate
	   (make-c-name-expr temp) 'managed-float t)))))






;;;; Streams




;;; Within GL only two kinds of Common Lisp streams are implemented.  They are
;;; string-streams and file-streams.  File-streams are currently only used to
;;; implement the *terminal-io* stream to "stdout" and "stdin", though in the
;;; future we may decide to make these the basis of the gensym-streams
;;; implementation.

(def-c-translation gl:make-string-output-stream ()
  ((lisp-specs :ftype (() gl-string-stream))
   `(make-gl-string-stream))
  ((trans-specs :c-type (() obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_string_strm")
     (list (make-c-literal-expr
	     (region-number-for-type-and-area
	       'gl-string-stream
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 'gl-string-stream)))
	   (make-c-literal-expr (c-type-tag 'string-strm))))))

(def-gl-macro gl:make-string-input-stream (string)
  `(gl:let ((in-stream (gl:make-string-output-stream))
	    (in-string ,string))
    (gl:setf (string-stream-input-string in-stream) in-string)
    (gl:setf (string-stream-input-index in-stream) 0)
    (gl:setf (string-stream-input-index-bounds in-stream)
	     (length-trans in-string))
    in-stream))

(gl:declaim (gl:side-effect-free string-stream-strings))

(def-c-translation string-stream-strings (string-stream)
  ((lisp-trans :ftype ((gl-string-stream) list))
   `(gl-string-stream-strings ,string-stream))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer string-strm) string-stream)
     "strings")))

(def-c-translation set-string-stream-strings (string-stream list)
  ((lisp-trans :ftype ((gl-string-stream list) list))
   `(setf (gl-string-stream-strings ,string-stream) ,list))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer string-strm) string-stream)
       "strings")
     "=" list)))

(gl:defsetf string-stream-strings set-string-stream-strings)

(gl:declaim (gl:side-effect-free string-stream-input-string))

(def-c-translation string-stream-input-string (string-stream)
  ((lisp-trans :ftype ((gl-string-stream) string))
   `(gl-string-stream-input-string ,string-stream))
  ((trans-specs :c-type ((obj) (array unsigned-char)))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer string-strm) string-stream)
     "input_string")))

(def-c-translation set-string-stream-input-string (string-stream string)
  ((lisp-trans :ftype ((gl-string-stream string) string))
   `(setf (gl-string-stream-input-string ,string-stream)
	  ,string))
  ((trans-specs :c-type ((obj (array unsigned-char)) (array unsigned-char)))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer string-strm) string-stream)
       "input_string")
     "=" string)))

(gl:defsetf string-stream-input-string set-string-stream-input-string)

(gl:declaim (gl:side-effect-free string-stream-input-index))

(def-c-translation string-stream-input-index (string-stream)
  ((lisp-trans :ftype ((gl-string-stream) fixnum))
   `(gl-string-stream-input-index ,string-stream))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer string-strm) string-stream)
     "input_index")))

(def-c-translation set-string-stream-input-index (string-stream fixnum)
  ((lisp-trans :ftype ((gl-string-stream fixnum) fixnum))
   `(setf (gl-string-stream-input-index ,string-stream)
	  ,fixnum))
  ((trans-specs :c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer string-strm) string-stream)
       "input_index")
     "=" fixnum)))

(gl:defsetf string-stream-input-index set-string-stream-input-index)

(gl:declaim (gl:side-effect-free string-stream-input-index-bounds))

(def-c-translation string-stream-input-index-bounds (string-stream)
  ((lisp-trans :ftype ((gl-string-stream) fixnum))
   `(gl-string-stream-input-index-bounds ,string-stream))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer string-strm) string-stream)
     "input_index_bounds")))

(def-c-translation set-string-stream-input-index-bounds (string-stream fixnum)
  ((lisp-trans :ftype ((gl-string-stream fixnum) fixnum))
   `(setf (gl-string-stream-input-index-bounds ,string-stream)
	  ,fixnum))
  ((trans-specs :c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer string-strm) string-stream)
       "input_index_bounds")
     "=" fixnum)))

(gl:defsetf string-stream-input-index-bounds set-string-stream-input-index-bounds)




;;; The following function is used to compute the initial value for
;;; *terminal-io* in gl/lisp/format.lisp.

(def-c-translation make-terminal-io-file-stream ()
  ((lisp-specs :ftype (() file-stream))
   '*terminal-io*)
  ((trans-specs :c-type (() obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_file_strm")
     (list (make-c-name-expr "stdin")
	   (make-c-name-expr "stdout")
	   (make-c-name-expr "NULL")
	   (make-c-name-expr "NULL")
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'file-stream
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 'file-stream)))
	   (make-c-literal-expr (c-type-tag 'file-strm))))))







;;;; Conses



;;; Note that GL:CONS is declared side-effect free since allocators modify no
;;; existing structures.  This works fine as long as the memory meter functions
;;; (which are rarely called) are not declared side-effect free.

(gl:declaim (gl:side-effect-free gl:cons))

(def-c-translation gl:cons (car cdr)
  ((lisp-specs :ftype ((t t) cons))
   `(cons ,car ,cdr))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_cons")
     (list car cdr
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'cons (declared-area-name
		       (l-expr-env function-call-l-expr) 'cons)))))))

(gl:declaim (gl:side-effect-free make-list-1))

(def-c-translation make-list-1 (length init-elements-p initial-elt)
  ;; Note that the result type is T since a zero elt list returns NIL.
  ((lisp-specs :ftype ((fixnum fixnum t) t))
   `(make-list ,length :initial-element (and (not (zerop ,init-elements-p))
					     ,initial-elt)))
  ((trans-specs :c-type ((sint32 sint32 obj) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_list")
     (list length init-elements-p initial-elt
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'cons (declared-area-name
		       (l-expr-env function-call-l-expr) 'cons)))))))





;;; The macro `set-list-contents' and `set-list-contents*' will modify a list
;;; such that it contains as elements the new contents passed as the rest
;;; argument.  It is more efficient than calling setf on the first, then second,
;;; etc. in that it will not traverse the conses of a list more than once.  It
;;; returns the passed list.  This macro is used by the implementations of
;;; GL:LIST and friends in glt/lisp/gl-types.lisp

(def-gl-macro set-list-contents (list &rest new-contents)
  (cond ((null new-contents)
	 list)
	((null (cdr new-contents))
	 (if (symbolp list)
	     `(gl:progn
		(gl:setf (gl:car ,list) ,(car new-contents))
		,list)
	     (let ((list-evaled (gensym)))
	       `(gl:let ((,list-evaled ,list))
		  (gl:setf (gl:car ,list-evaled) ,(car new-contents))
		  ,list-evaled))))
	(t
	 (let* ((eval-needed? (not (symbolp list)))
		(new-list (if eval-needed? (gensym) list))
		(current-cons (gensym)))
	   `(gl:let* (,@(if eval-needed? `((,new-list ,list)))
			(,current-cons ,new-list))
	      (gl:setf (gl:car ,current-cons) ,(car new-contents))
	      ,@(loop with lines = nil
		      for element in (cdr new-contents)
		      do
		  (push `(gl:setq ,current-cons (gl:cdr-of-cons ,current-cons))
			lines)
		  (push `(gl:setf (gl:car ,current-cons) ,element) lines)
		      finally
			(return (nreverse lines)))
	      ,new-list)))))

(def-gl-macro set-list-contents* (list &rest new-contents)
  (cond ((null (cdr new-contents))
	 (error "SET-LIST-CONTENTS* must be called with at least 2 new-value ~
                 arguments."))
	(t
	 (let* ((eval-needed? (not (symbolp list)))
		(new-list (if eval-needed? (gensym) list))
		(current-cons (gensym)))
	   `(gl:let* (,@(if eval-needed? `((,new-list ,list)))
			(,current-cons ,new-list))
	      (gl:setf (gl:car ,current-cons) ,(car new-contents))
	      ,@(loop with lines = nil
		      for element-cons on (cdr new-contents)
		      for element = (car element-cons)
		      do
		  (cond
		    ((cons-cdr element-cons)
		     (push `(gl:setq ,current-cons (gl:cdr-of-cons ,current-cons))
			   lines)
		     (push `(gl:setf (gl:car ,current-cons) ,element) lines))
		    (t
		     (push `(gl:setf (gl:cdr ,current-cons) ,element) lines)))
		      finally
			(return (nreverse lines)))
	      ,new-list)))))

	       
	       
(gl:declaim (gl:functional car-trans cdr-trans))

(def-c-translation car-trans (list)
  ((lisp-specs :ftype ((list) t))
   `(car ,list))
  ((trans-specs :lisp-type ((cons) t)
		:c-type ((obj) obj))
   (make-c-function-call-expr
     (make-c-name-expr "CAR")
     (list list)))
  ((trans-specs :lisp-type ((list) t)
		:c-type ((obj) obj))
   (let ((var? nil)
	 (expr list))
     (unless (c-name-expr-p expr)
       (setq var? (reusable-c-variable-identifier
		    'temp c-func 'obj (l-expr-env function-call-l-expr)))
       (setq expr (make-c-name-expr var?))
       (emit-expr-to-compound-statement
	 (make-c-infix-expr expr "=" list)
	 c-compound-statement))
     (make-c-conditional-expr
       (make-c-infix-expr expr "!=" "NULL")
       (make-c-function-call-expr (make-c-name-expr "CAR") (list expr))
       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))))

(def-c-translation set-car (cons value)
  ((lisp-specs :ftype ((cons t) t))
   `(setf (car ,cons) ,value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-function-call-expr (make-c-name-expr "CAR") (list cons))
     "=" value)))

(gl:defsetf gl:car set-car)

(def-gl-macro gl:rplaca (cons new-car)
  (if (symbolp cons)
      `(gl:progn
	 (gl:setf (gl:car ,cons) ,new-car)
	 ,cons)
      (let ((cons-var (gensym)))
	`(gl:let ((,cons-var ,cons))
	   (gl:setf (gl:car ,cons-var) ,new-car)
	   ,cons-var))))

(def-c-translation cdr-trans (list)
  ((lisp-specs :ftype ((list) t))
   `(cdr ,list))
  ((trans-specs :lisp-type ((cons) t)
		:c-type ((obj) obj))
   (make-c-function-call-expr (make-c-name-expr "CDR") (list list)))
  ((trans-specs :lisp-type ((list) t)
		:c-type ((obj) obj))
   (let ((var? nil)
	 (expr list))
     (unless (c-name-expr-p expr)
       (setq var? (reusable-c-variable-identifier
		    'temp c-func 'obj (l-expr-env function-call-l-expr)))
       (setq expr (make-c-name-expr var?))
       (emit-expr-to-compound-statement
	 (make-c-infix-expr expr "=" list)
	 c-compound-statement))
     (make-c-conditional-expr
       (make-c-infix-expr expr "!=" "NULL")
       (make-c-function-call-expr (make-c-name-expr "CDR") (list expr))
       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))))

(def-c-translation set-cdr (cons value)
  ((lisp-specs :ftype ((cons t) t))
   `(setf (cdr ,cons) ,value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-function-call-expr (make-c-name-expr "CDR") (list cons))
     "=" value)))

(gl:defsetf gl:cdr set-cdr)

(def-gl-macro gl:rplacd (cons new-car)
  (if (symbolp cons)
      `(gl:progn
	 (gl:setf (gl:cdr ,cons) ,new-car)
	 ,cons)
      (let ((cons-var (gensym)))
	`(gl:let ((,cons-var ,cons))
	   (gl:setf (gl:cdr ,cons-var) ,new-car)
	   ,cons-var))))

(def-gl-macro gl:car-of-cons (gl:cons)
  `(gl:car (gl:the gl:cons ,gl:cons)))

(def-gl-macro gl:cdr-of-cons (gl:cons)
  `(gl:cdr (gl:the gl:cons ,gl:cons)))





;;; The macro `def-car-cdr-suite' will be called from within GL, where the
;;; functions being defined here can be translated.  This macro is defined here
;;; so that we can use Lisp, since the CxR suite must be defined before gl:loop.

(defmacro def-car-cdr-suite (from-level to-level)
  (cons
    'gl:progn
    (loop for levels from from-level to to-level
	  append
	  (loop for op-index from 0 below (expt 2 levels)
		for selector-list
		    = (loop for char-index from 0 below levels
			    collect (if (logbitp char-index op-index) #\D #\A))
		for car-cdr-list
		    = (loop for char-index from 0 below levels
			    collect (if (logbitp char-index op-index)
					'gl:cdr
					'gl:car))
		for op-name = (intern (concatenate
					'string '(#\C) selector-list '(#\R))
				      *gl-package*)
		for op-of-conses
		    = (intern (format nil "~a-OF-CONSES" op-name) *gl-package*)
		for setter-name
		    = (intern (format nil "SET-~a" op-name) *gli-package*)
		for outer-op
		    = (intern (format nil "C~aR" (car selector-list))
			      *gl-package*)
		for inner-op
		    = (intern (concatenate
				'string '(#\C) (cdr selector-list) '(#\R))
			      *gl-package*)
		for inner-op-of-conses
		    = (intern (format nil "~a-OF-CONS~a"
				      inner-op
				      (if (cddr selector-list) "ES" ""))
			      *gl-package*)
	      append
		`((gl:declaim (gl:functional ,op-name)
			      ,@(if (<= levels 3)
				    `((gl:inline ,op-name))
				    nil))
		  (gl:defun ,op-name (gl:list)
		    (gl:declare (gl:type gl:list gl:list)
				(gl:return-type t))
		    ,@(loop for op-cons on (reverse car-cdr-list)
			    for op = (car op-cons)
			    collect
			    (if (null (cons-cdr op-cons))
				`(gl:if gl:list
					(,op (gl:the gl:cons gl:list))
					nil)
				`(gl:if gl:list
					(gl:setq
					  gl:list
					  (,op (gl:the gl:cons gl:list)))
					(gl:return-from ,op-name nil)))))
		  (gl:defmacro ,op-of-conses (gl:list)
		    `(,',outer-op
			  (gl:the gl:cons (,',inner-op-of-conses ,gl:list))))
		  (gl:defsetf ,op-name ,setter-name)
		  (gl:defmacro ,setter-name (list value)
		    `(gl:setf (,',outer-op (,',inner-op-of-conses ,list))
			      ,value)))))))

(def-gl-macro gl:push (&environment env item list-place)
  (if (and (symbolp list-place)
	   (not (eq (gl:variable-information list-place env) :symbol-macro)))
      `(gl:setf ,list-place (gl:cons ,item ,list-place))
      (multiple-value-bind (temps vals stores store-form access-form)
	  (gl:get-setf-expansion list-place env)
	(let ((item-var (gensym)))
	  `(gl:let*
	       ,(cons (list item-var item)
		      (loop for var in (append temps stores)
			    for val in (append vals `((gl:cons ,item-var
							       ,access-form)))
			    collect (list var val)))
	     ,store-form)))))

(def-gl-macro gl:pop (&environment env list-place)
  (if (and (symbolp list-place)
	   (not (eq (gl:variable-information list-place env) :symbol-macro)))
      `(gl:prog1 (gl:car ,list-place)
	 (gl:setq ,list-place (gl:cdr ,list-place)))
      (multiple-value-bind (temps vals stores store-form access-form)
	  (gl:get-setf-expansion list-place env)
	`(gl:let* ,(loop for var in (append temps stores)
			 for val in (append vals `((gl:cdr ,access-form)))
			 collect (list var val))
	   (gl:prog1 (gl:car ,access-form)
	     ,store-form)))))






;;;; Symbols




;;; This section implements the lowest level primitives for symbols.  Many other
;;; features, such as interning and gensyming, are implemented in
;;; gl/lisp/packages.lisp.

;;; The `make-empty-symbol' mallocs and type tags the memory for a symbol, but
;;; does not yet install a print-name or any other characteristics.  They all
;;; happen in the GL libraries for packages.

(def-c-translation make-empty-symbol ()
  ((lisp-specs :ftype (() symbol))
   `(derror "There is no development time expansion for make-empty-symbol."))
  ((trans-specs :c-type (() obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_symbol")
     (list (make-c-literal-expr
	     (region-number-for-type-and-area
	       'symbol
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 'symbol)))
	   (make-c-literal-expr (c-type-tag 'sym))))))




;;; The translated form `set-symbol-type-tag' takes a symbol and installs the
;;; appropriate type tag onto it.  This is needed to initialize C symbol
;;; structures that were allocated in arrays, i.e. the constant symbols of a
;;; translated file.

(def-c-translation set-symbol-type-tag (symbol)
  ((lisp-specs :ftype ((symbol) fixnum))
   `(derror "No development translation for set-symbol-type-tag of ~s."
	   ,symbol))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "type")
     "=" (make-c-literal-expr (c-type-tag 'sym)))))
   
   


(gl:declaim (gl:side-effect-free symbol-local-value))

(def-c-translation symbol-local-value (symbol)
  ((lisp-specs :ftype ((symbol) t))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) boolean))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "local_value")))

(gl:defsetf symbol-local-value set-symbol-local-value)

(def-c-translation set-symbol-local-value (symbol flag)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "Cant set-symbol-local-value of ~s to ~s in development."
	   ,symbol ,flag))
  ((trans-specs :c-type ((obj boolean) boolean))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "local_value")
     "=" flag)))

(gl:declaim (gl:side-effect-free symbol-external))

(def-c-translation symbol-external (symbol)
  ((lisp-specs :ftype ((symbol) t))
   (let ((sym (gensym)))
     `(let ((,sym ,symbol))
	(multiple-value-bind (new-sym internal)
	    (find-symbol (symbol-name ,sym) (symbol-package ,sym))
	  (declare (ignore new-sym))
	  (eq internal :external)))))
  ((trans-specs :c-type ((obj) boolean))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol) "external")))

(gl:defsetf symbol-external set-symbol-external)

(def-c-translation set-symbol-external (symbol flag)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "Can't set-symbol-external of ~s to ~s in development."
	   ,symbol ,flag))
  ((trans-specs :c-type ((obj boolean) boolean))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "external")
     "=" flag)))

(gl:declaim (gl:side-effect-free symbol-balance))

(def-c-translation symbol-balance (symbol)
  ((lisp-specs :ftype ((symbol) fixnum))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sym) symbol) "balance"))))

(gl:defsetf symbol-balance set-symbol-balance)

(def-c-translation set-symbol-balance (symbol fixnum)
  ((lisp-specs :ftype ((symbol fixnum) fixnum))
   (declare (ignore symbol))
   fixnum)
  ((trans-specs :c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "balance")
     "=" fixnum)))

(gl:declaim (gl:side-effect-free symbol-imported))

(def-c-translation symbol-imported (symbol)
  ((lisp-specs :ftype ((symbol) t))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) boolean))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol) "imported")))

(gl:defsetf symbol-imported set-symbol-imported)

(def-c-translation set-symbol-imported (symbol flag)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "Can't set-symbol-imported of ~s to ~s." ,symbol ,flag))
  ((trans-specs :c-type ((obj boolean) boolean))
   (make-c-infix-expr 
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "imported")
     "=" flag)))

(gl:declaim (gl:side-effect-free symbol-name-hash))

(def-c-translation symbol-name-hash (symbol)
  ((lisp-specs :ftype ((symbol) fixnum))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sym) symbol) "name_hash"))))

(gl:defsetf symbol-name-hash set-symbol-name-hash)

(def-c-translation set-symbol-name-hash (symbol fixnum)
  ((lisp-specs :ftype ((symbol fixnum) fixnum))
   (declare (ignore symbol))
   fixnum)
  ((trans-specs :c-type ((obj sint32) sint32))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "name_hash")
     "=" fixnum)))

(def-gl-macro gl:symbol-name (symbol)
  `(gl:the gl:string
	   ,(if (or (symbolp symbol) (constantp symbol))
		`(gl:if ,symbol
			(non-null-symbol-name ,symbol)
			"NIL")
		(let ((sym (gensym)))
		  `(gl:let ((,sym ,symbol))
		     (gl:symbol-name ,sym))))))

(gl:declaim (gl:functional non-null-symbol-name))

(def-c-translation non-null-symbol-name (symbol)
  ((lisp-specs :ftype ((symbol) string))
   `(symbol-name ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol) "symbol_name")))

(def-c-translation set-symbol-name (symbol string)
  ((lisp-specs :ftype ((t t) t))
   `(derror "Can't actually set the symbol name in development: ~s ~s"
	   ,symbol ,string))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "symbol_name")
     "=" string)))

(def-gl-macro gl:symbol-value (symbol)
  (if (or (symbolp symbol) (constantp symbol))
      `(gl:if ,symbol
	      (gl:if (symbol-local-value ,symbol)
		     (symbol-value-pointer ,symbol)
		     (symbol-non-local-value ,symbol))
	      nil)
      (let ((var (gensym)))
	`(gl:let ((,var ,symbol))
	   (gl:symbol-value ,var)))))

(gl:defsetf gl:symbol-value gl:set)

(def-gl-macro gl:set (symbol new-value)
  (let ((sym (gensym))
	(val (gensym)))
    `(gl:let ((,sym ,symbol)
	      (,val ,new-value))
       (gl:if ,sym
	      (gl:if (symbol-local-value ,sym)
		     (set-symbol-value-pointer ,sym ,val)
		     (set-symbol-non-local-value ,sym ,val))
	      (gl:error "Can't set the symbol-value of NIL."))
       ,val)))

(gl:declaim (gl:side-effect-free symbol-value-pointer))

(def-c-translation symbol-value-pointer (symbol)
  ((lisp-specs :ftype ((symbol) t))
   `(symbol-value ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol) "symbol_value")))

(gl:defsetf symbol-value-pointer set-symbol-value-pointer)

(def-c-translation set-symbol-value-pointer (symbol new-value)
  ((lisp-specs :ftype ((symbol t) t))
   `(setf (symbol-value ,symbol) ,new-value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol) "symbol_value")
     "=" new-value)))

(gl:declaim (gl:side-effect-free symbol-non-local-value))

(def-c-translation symbol-non-local-value (symbol)
  ((lisp-specs :ftype ((symbol) t))
   `(symbol-value ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-unary-expr
     #\* (make-c-cast-expr
	   '(pointer obj)
	   (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer sym) symbol)
	     "symbol_value")))))

(def-c-translation set-symbol-non-local-value (symbol new-value)
  ((lisp-specs :ftype ((symbol t) t))
   `(setf (symbol-value ,symbol) ,new-value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-unary-expr
       #\* (make-c-cast-expr
	     '(pointer obj)
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sym) symbol)
	       "symbol_value")))
     "=" new-value)))

(def-gl-macro gl:symbol-plist (symbol)
  (if (or (symbolp symbol) (constantp symbol))
      `(gl:if ,symbol
	      (non-null-symbol-plist ,symbol)
	      symbol-plist-of-nil)
      (let ((var (gensym)))
	`(gl:let ((,var ,symbol))
	   (gl:symbol-plist ,var)))))

(gl:declaim (gl:side-effect-free non-null-symbol-plist))

(def-c-translation non-null-symbol-plist (symbol)
  ((lisp-specs :ftype ((symbol) t))
   `(symbol-plist ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "symbol_plist")))

(gl:defsetf gl:symbol-plist set-symbol-plist)

(def-gl-macro set-symbol-plist (symbol new-plist)
  (let ((sym (gensym))
	(new (gensym)))
    `(gl:let ((,sym ,symbol)
	      (,new ,new-plist))
       (gl:if ,sym
	      (set-non-null-symbol-plist ,sym ,new)
	      (gl:setq symbol-plist-of-nil ,new)))))

(def-c-translation set-non-null-symbol-plist (symbol new-plist)
  ((lisp-specs :ftype ((symbol t) t))
   `(setf (symbol-plist ,symbol) ,new-plist))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "symbol_plist")
     "=" new-plist)))

(gl:declaim (gl:side-effect-free gl:symbol-package))

(def-c-translation gl:symbol-package (symbol)
  ((lisp-specs :ftype ((symbol) t))
   `(symbol-package ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "symbol_package")))

(def-c-translation set-symbol-package (symbol package-or-nil)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "There is no development implementation of set-symbol-package: ~
            args = ~s, ~s"
	   ,symbol ,package-or-nil))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "symbol_package")
     "=" package-or-nil)))

(gl:declaim (gl:side-effect-free gl:symbol-function))

(def-c-translation gl:symbol-function (symbol)
  ((lisp-specs :ftype ((symbol) t))
   `(symbol-function ,symbol))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "symbol_function")))

(def-c-translation set-symbol-function (symbol function)
  ((lisp-specs :ftype ((symbol t) t))
   `(setf (symbol-function ,symbol) ,function))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "symbol_function")
     "=" function)))

(gl:defsetf gl:symbol-function set-symbol-function)

(gl:declaim (gl:side-effect-free symbol-left-branch))




;;; The macro `fboundp' takes a symbol and returns whether or not the
;;; symbol-function cell of the symbol is to a compiled-function.  Note that GL
;;; does not allow function names of the (setf <symbol>) style, and so this
;;; function takes only symbols and not generalized function names.

(def-gl-macro gl:fboundp (symbol)
  (if (eval-feature :translator)
      `(gl:not (gl:eq (gl:symbol-function ,symbol) (the-unbound-value)))
      `(ab-lisp::fboundp ,symbol)))

(def-c-translation symbol-left-branch (symbol)
  ((lisp-specs :ftype ((symbol) t))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "left_branch")))

(gl:defsetf symbol-left-branch set-symbol-left-branch)

(def-c-translation set-symbol-left-branch (symbol new-value)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "Can't set-symbol-left-branch of ~s to ~s in development."
	   ,symbol ,new-value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "left_branch")
     "=" new-value)))

(gl:declaim (gl:side-effect-free symbol-right-branch))

(def-c-translation symbol-right-branch (symbol)
  ((lisp-specs :ftype ((symbol) t))
   (declare (ignore symbol))
   'nonnil-variable-of-unknowable-value-and-type)
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sym) symbol)
     "right_branch")))

(gl:defsetf symbol-right-branch set-symbol-right-branch)

(def-c-translation set-symbol-right-branch (symbol new-value)
  ((lisp-specs :ftype ((symbol t) t))
   `(derror "Can't set-symbol-right-branch of ~s to ~s in development."
	   ,symbol ,new-value))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer sym) symbol)
       "right_branch")
     "=" new-value)))

(gl:declaim (gl:side-effect-free not-unbound-value-p))

(def-c-translation not-unbound-value-p (value)
  ((lisp-specs :ftype ((t) t))
   `(derror "Not-unbound-value-p has no development implementation: ~s"
	   ,value))
  ((trans-specs :c-type ((obj) boolean))
   (make-c-infix-expr
     value "!="
     (make-c-cast-expr 'obj (make-c-unary-expr
			      #\& (make-c-name-expr "Unbound"))))))

(gl:declaim (gl:side-effect-free the-unbound-value))

(def-c-translation the-unbound-value ()
  ((lisp-specs :ftype (() t))
   `(derror "The-unbound-value cannot be returned in development Lisp."))
  ((trans-specs :c-type (() obj))
   (c-unbound-value-expr)))




;;; In CMU Lisp, if you give a fill-pointered string to make-symbol, the
;;; compiler can croak on that later on.  Since GL always allocates
;;; fill-pointered strings, this is especially a problem.  So, within the base
;;; Lisp environment, make sure that all strings given to make-symbol are in
;;; fact simple-strings.

(defun make-symbol-safely (string)
  (when (not (simple-string-p string))
    (let ((new-string (make-string (length string))))
      (replace new-string string)
      (setq string new-string)))
  (make-symbol string))






;;;; Compiled Functions




(gl:declaim (gl:side-effect-free compiled-function-arg-count
			   compiled-function-optional-arguments
			   compiled-function-default-arguments
			   compiled-function-name))

(def-c-translation compiled-function-arg-count (compiled-function)
  ((lisp-specs :ftype ((compiled-function) fixnum))
   `(derror "No Lisp env implementation of (compiled-function-arg-count ~s)"
	   ,compiled-function))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer func) compiled-function)
	       "arg_count"))))

(def-c-translation compiled-function-optional-arguments (compiled-function)
  ((lisp-specs :ftype ((compiled-function) fixnum))
   `(progn
      (derror "No Lisp env implementation of (compiled-function-optional-arguments ~s)"
	     ,compiled-function)
      0))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer func) compiled-function)
	       "optional_arguments"))))

(def-c-translation compiled-function-sets-values-count (compiled-function)
  ((lisp-specs :ftype ((compiled-function) fixnum))
   `(progn
      (derror "No Lisp env implementation of (compiled-function-optional-arguments ~s)"
	     ,compiled-function)))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32 (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer func) compiled-function)
	       "sets_values_count"))))

(defvar variable-of-unknown-value nil)

(def-c-translation compiled-function-default-arguments (compiled-function)
  ((lisp-specs :ftype ((compiled-function) t))
   `(progn
      (derror "No Lisp env implementation of (compiled-function-default-arguments ~s)"
	     ,compiled-function)))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer func) compiled-function)
     "default_arguments")))

(def-c-translation compiled-function-name (compiled-function)
  ((lisp-specs :ftype ((compiled-function) t))
   #+lucid
   `(lucid::function-name ,compiled-function)
   #+cmu
   `(kernel:%function-name ,compiled-function)
   #-(or lucid cmu)
   `(progn
      (derror "No Lisp env implementation of (compiled-function-name ~s)"
	     ,compiled-function)
      :dummy-name))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer func) compiled-function)
     "name")))






;;;; Packages







;;; The macro `gl:make-package' takes a name and a :use keyword argument, and
;;; returns a new package with that name.  If a package with that name already
;;; exists, this function signals an error.

(def-gl-macro gl:make-package (name &key (use ''("GL")))
  (if (eval-feature :translator)
      `(gl::make-package-1 ,name ,use)
      `(lisp:make-package ,name :use ,use)))

(def-gl-macro gl:find-package (string-or-symbol-or-package)
  (if (eval-feature :translator)
      `(gl::find-package-1 ,string-or-symbol-or-package)
      ;; Note that the Lucid implementation of find-package has an error, in
      ;; that it signals an error when given a package object instead of just
      ;; returning it.  This macroexpansion will work around that bug.
      ;; -jallard, 5/1/97
    `(find-package-safely ,string-or-symbol-or-package)))

(defun find-package-safely (arg)
  (if (typep arg 'package)
      arg
    (lisp:find-package arg)))


(def-c-translation make-new-package (name use-list)
  ((lisp-specs :ftype ((string t) package))
   `(make-package ,name :use ,use-list))
  ((trans-specs :c-type ((obj obj) obj))
   (make-c-function-call-expr
     (make-c-name-expr "alloc_package")
     (list name use-list
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'package
	       (declared-area-name
		 (l-expr-env function-call-l-expr)
		 'package)))
	   (make-c-literal-expr (c-type-tag 'pkg))))))

(gl:declaim (gl:functional gl:package-name))

(def-c-translation gl:package-name (package)
  ((lisp-specs :ftype ((package) string))
   `(package-name ,package))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer pkg) package) "name")))

(gl:declaim (gl:side-effect-free package-use-list-internal))

(def-c-translation package-use-list-internal (package)
  ((lisp-specs :ftype ((package) t))
   `(package-use-list ,package))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer pkg) package)
     "used_packages")))

(gl:declaim (gl:side-effect-free package-root-symbol))

(def-c-translation package-root-symbol (package)
  ((lisp-specs :ftype ((package) t))
   `(derror "Package-root-symbol has no development implementation: ~s" ,package))
  ((trans-specs :c-type ((obj) obj))
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer pkg) package)
     "root_symbol")))

(gl:defsetf package-root-symbol set-package-root-symbol)

(def-c-translation set-package-root-symbol (package symbol)
  ((lisp-specs :ftype ((package t) t))
   `(derror "Set-package-root-symbol has no development implementation: ~s to ~s"
	   ,package ,symbol))
  ((c-type :c-type ((obj obj) obj))
   (make-c-infix-expr
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer pkg) package)
       "root_symbol")
     "=" symbol)))
     






;;;; Typep




(def-gl-macro gl:typep (&environment env object type)
  (let ((expanded-type (gl:macroexpand type env)))
    (unless (gl:constantp expanded-type)
      (error "GL:typep can only handle constant types, not ~s" expanded-type))
    (setq type (expand-type (eval expanded-type)))
    (if (and (symbolp object)
	     (not (eq (gl:variable-information object env) :symbol-macro)))
	(cond ((consp type)
	       (let ((first (cons-car type)))
		 (case first
		   ((and or not)
		    `(,(cdr (assq first '((and . gl:and) (or . gl:or)
					  (not . gl:not))))
		       ,@(loop for subtype in (cons-cdr type)
			       collect `(gl:typep ,object (gl:quote ,subtype)))))
		 ((satisfies gl:satisfies)
		  `(,(cons-second type) ,object))
		 ((unsigned-byte)
		  `(and (inlined-typep ,object 'fixnum)
			(>= (the fixnum ,object) 0)
			(< (the fixnum ,object) ,(expt 2 (second type)))))
		 (t
		  `(inlined-typep ,object (gl:quote ,type))))))
	      (t
	       `(inlined-typep ,object (gl:quote ,type))))
	(let ((object-var (gensym)))
	  `(gl:let ((,object-var ,object))
	     (gl:typep ,object-var (gl:quote ,type)))))))

(def-c-translation type-tag (object)
  ((lisp-specs :ftype ((t) fixnum))
   `(car (type-tags-for-lisp-type (type-of ,object))))
  ((trans-specs :c-type ((obj) sint32))
   (let ((tag-var (reusable-c-variable-identifier
		    'temp c-func 'sint32 (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr object "==" "NULL")
       (make-c-literal-expr 0)
       (make-c-conditional-expr
	 (make-c-infix-expr
	   (make-c-name-expr tag-var)
	   "=" (make-c-infix-expr (make-c-cast-expr 'uint32 object) "&" 3))
	 (make-c-name-expr tag-var)
	 (make-c-cast-expr
	   'sint32 (make-c-indirect-selection-expr
		     (make-c-cast-expr '(pointer hdr) object)
		     "type")))))))

(def-gl-macro gl:typecase (keyform &rest clauses)
  ;; Note that gli::type-tag is not a proper macro, and can evaluate its
  ;; argument multiple times.
  (if (eval-feature :translator)
      (if (symbolp keyform)
	  `(fixnum-case (type-tag ,keyform)
	     ,@(loop with tags-so-far = nil
		     for (type . forms) in clauses
		     for type-tags? = (unless (eq type t)
					(type-tags-for-lisp-type type))
		     for unused-tags
			 = (loop for tag in type-tags?
				 unless (member tag tags-so-far)
				   collect (progn (push tag tags-so-far)
						  tag))
		     when (eq type t)
		       collect `(t ,@forms)
		     when (and (not (eq type t)) unused-tags)
		       collect `(,unused-tags ,@forms)))
	  (let ((key-var (gensym)))
	    `(gl:let ((,key-var ,keyform))
	       (gl:typecase ,key-var
		 ,@clauses))))
      (let ((key-var (gensym)))
	`(gl:let ((,key-var ,keyform))
	   (gl:cond
	     ,@(loop for (type . forms) in clauses
		     collect
		     (if (memqp type '(gl:t gl:otherwise))
			 `(t ,@forms)
			 `((gl:typep ,key-var ',type) ,@forms))))))))

(gl:declaim (gl:functional gl:not))

(def-c-translation gl:not (object)
  ((lisp-specs :ftype ((t) t))
   `(not ,object))
  ((trans-specs :c-type ((boolean) boolean))
   ;; Perform some optimizations when negating an argument.  If this is a not of
   ;; a not, just return the argument to the inner not.  If this is a not of a
   ;; c-equality-expr, replace the argument with a new c-equality-expr using the
   ;; opposite equality string.  Otherwise, just negate the argument.
   (cond ((and (c-unary-expr-p object)
	       (char= (c-unary-expr-op-char object) #\!))
	  (c-unary-expr-arg-expr object))
	 ((c-equality-expr-p object)
	  (let ((op-string (c-equality-expr-op-string object)))
	    (make-c-equality-expr
	      (c-equality-expr-left-arg object)
	      (cond ((string= op-string "==")
		     "!=")
		    ((string= op-string "!=")
		     "==")
		    (t
		     (translation-error
		       "Can't translate NOT of ~s, bad string ~s"
		       object op-string)))
	      (c-equality-expr-right-arg object))))
	 (t
	  (make-c-unary-expr #\! object)))))

(def-c-translation eq-trans (object1 object2)
  ((lisp-specs :ftype ((t t) t))
   `(eq ,object1 ,object2))
  ((trans-specs :c-type ((obj obj) boolean))
   (make-c-equality-expr object1 "==" object2)))

(gl:define-compiler-macro gl:eql (object1 object2)
  `(eql-trans ,object1 ,object2))

(gl:declaim (gl:functional eql-trans))

(def-c-translation eql-trans (object1 object2)
  ((lisp-specs :ftype ((t t) t))
   `(eql ,object1 ,object2))
  ((trans-specs :lisp-type (((or symbol fixnum character)
			     t)
			    t)
		:c-type ((obj obj) boolean))
   (make-c-equality-expr object1 "==" object2))
  ((trans-specs :lisp-type ((t
			     (or symbol fixnum character))
			    t)
		:c-type ((obj obj) boolean))
   (make-c-equality-expr object1 "==" object2))
  ((trans-specs :lisp-type ((t t) t)
		:c-type ((obj obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "eql" '(obj obj))
   (make-c-function-call-expr (make-c-name-expr "eql")
			      (list object1 object2))))

(gl:define-compiler-macro gl:equal (object1 object2)
  `(equal-trans ,object1 ,object2))

(gl:declaim (gl:functional equal-trans))

(def-c-translation equal-trans (object1 object2)
  ((lisp-specs :ftype ((t t) t))
   `(equal ,object1 ,object2))
  ((trans-specs :lisp-type (((or symbol fixnum character)
			     t)
			    t)
		:c-type ((obj obj) boolean))
   (make-c-equality-expr object1 "==" object2))
  ((trans-specs :lisp-type ((t
			     (or symbol fixnum character))
			    t)
		:c-type ((obj obj) boolean))
   (make-c-equality-expr object1 "==" object2))
  ((trans-specs :lisp-type ((t t) t)
		:c-type ((obj obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "equal" '(obj obj))
   (make-c-function-call-expr (make-c-name-expr "equal")
			      (list object1 object2))))

(defun process-cond-clauses (clauses)
  (let ((clause (cons-car clauses))
	(rest-clauses (cons-cdr clauses)))
    (unless (consp clause)
      (error "Malformed cond clause ~s" clause))
    (let ((test (cons-car clause))
	  (forms (cons-cdr clause)))
      (cond ((eq test t)
	     (if forms
		 `(gl:progn ,@forms)
		 t))
	    (forms
	     `(gl:if ,test
		     (gl:progn ,@forms)
		     ,(when rest-clauses
			(process-cond-clauses rest-clauses))))
	    (t
	     (let ((test-value (gensym)))
	       `(gl:let ((,test-value ,test))
		  (gl:if ,test-value
			 ,test-value
			 ,(when rest-clauses
			    (process-cond-clauses rest-clauses))))))))))
	       
	

(def-gl-macro gl:cond (&rest clauses)
  (if clauses
      (process-cond-clauses clauses)
      nil))

(def-gl-macro gl:when (test &body body)
  `(gl:if ,test
	  (gl:progn ,@body)))

(def-gl-macro gl:unless (test &body body)
  `(gl:if (gl:not ,test)
	  (gl:progn ,@body)))







;;;; Case





(def-gl-macro gl:case (&environment env keyform &rest case-clauses)
  (cond
    ((and (gl-subtypep (expression-result-type keyform env) 'fixnum)
	  (loop for (keys) in case-clauses
		always (or (fixnump keys)
			   (memqp keys '(t gl:otherwise))
			   (and (consp keys)
				(loop for key in keys
				      always (fixnump key))))))
     `(fixnum-case ,keyform ,@case-clauses))
    ((and (gl-subtypep (expression-result-type keyform env) 'character)
	  (loop for (keys) in case-clauses
		always (or (characterp keys)
			   (memqp keys '(t gl:otherwise))
			   (and (consp keys)
				(loop for key in keys
				      always (characterp key))))))
     `(fixnum-case (gl:char-code ,keyform)
	,@(loop for (keys . forms) in case-clauses
		collect
		`(,(cond ((characterp keys)
			  (list (char-code keys)))
			 ((memqp keys '(t gl:otherwise))
			  keys)
			 (t
			  (loop for key in keys
				collect (char-code key))))
		   ,@forms))))
    (t
     (let ((key-val (gensym)))
       `(gl:let ((,key-val ,keyform))
	  (gl:cond
	    ,@(loop for (keylist . forms) in case-clauses
		    when keylist
		      collect
		      (cond
			((memqp keylist '(gl:t gl:otherwise))
			 `(gl:t ,@forms))
			((atom keylist)
			 `((gl:eql ,key-val ',keylist) ,@forms))
			((null (cdr keylist))
			 `((gl:eql ,key-val ',(car keylist)) ,@forms))
			(t
			 `((gl:or ,@(loop for key in keylist
					  collect `(gl:eql ,key-val ',key)))
			   ,@forms))))))))))

(def-gl-macro gl:ecase (&rest args)
  `(gl:case ,@args (t (gl:error "Fell off end of ECASE - no matching clause"))))

(def-gl-macro gl:psetq (&rest vars-and-values)
  (cond
    ((null vars-and-values)
     nil)
    ((null (cons-cddr vars-and-values))
     `(gl:progn
	(gl:setq ,(cons-car vars-and-values) ,(cons-second vars-and-values))
	nil))
    (t
     (let ((settings nil))
       `(gl:let ,(loop for (var value) on vars-and-values by #'cddr
		       for temp-var = (gensym)
		       collect (list temp-var value)
		       do
		   (push `(gl:setq ,var ,temp-var) settings))
	  ,@(nreverse settings)
	  nil)))))

(def-gl-macro gl:multiple-value-setq (variables form)
  (let ((bind-vars (loop repeat (length variables) collect (gensym))))
    `(gl:multiple-value-bind ,bind-vars ,form
       ,@(when (memq nil (cdr variables))
	   `((gl:declare
	       (gl:ignore ,@(loop for first = t then nil
				  for set in variables
				  for bind in bind-vars
				  when (and (null set) (not first))
				    collect bind)))))
       ,@(loop for set in variables
	       for bind in bind-vars
	       when set collect `(gl:setq ,set ,bind))
       ,(car bind-vars))))






;;;; Pointers




;;; The following forms implement operations for fetching the integer values of
;;; object pointers.  These are used for printing and for computing hash numbers
;;; of objects.

;;; The translatable form `pointer-as-fixnum' returns the value of the pointer
;;; as a fixnum.  Note that if the uppemost bits of this value are on, then the
;;; result will be a negative value.  Also note that informatoin will be lost of
;;; the result of this form is allowed to be tagged as a Lisp fixnum.  If the
;;; value is carefully used, it can be passed through fixnum-declared forms
;;; which are implemented as sint32 values, in which case it will retain all its
;;; bits.  This is primarily used for the random object Lisp printer forms.

(def-c-translation pointer-as-fixnum (object)
  ((lisp-specs :ftype ((t) fixnum))
   #+lucid
   `(sys:%pointer ,object)
   #-lucid
   `(progn ,object -1))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr 'sint32 object)))




;;; The translatable form `pointer-as-positive-fixnum' returns a positive,
;;; 29-bit fixnum that can be used as a good hashing number for Lisp objects.
;;; The returned value is likely to be unique for the given Lisp object.  The
;;; value is the pointer shifted right by 3 bits.  Since we allocate items on 4
;;; byte boundaries, not 8, this could lead to two objects whose addresses are
;;; within 4 bytes of each to return the same value from this operation.
;;; However, since all of our Lisp objects are at least 8 bytes wide, I believe
;;; that this cannot happen.  In any case, the returned value is more than
;;; adequate as a hashing number for the given Lisp object.  If we had a garbage
;;; collector that could relocate items, this could cause problems, but we don't
;;; so it can't.

(def-c-translation pointer-as-positive-fixnum (object)
  ((lisp-specs :ftype ((t) fixnum))
   #+lucid
   `(sys:%pointer ,object)
   #-lucid
   `(sxhash ,object))
  ((trans-specs :c-type ((obj) sint32))
   (make-c-cast-expr
     'sint32
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 object)
       ">>" (make-c-literal-expr 3)))))






;;;; Machine Type




;;; The function `get-platform-code' returns an integer which identifies the
;;; currently running system.  See code in gl/lisp/gl-util.lisp for how to add
;;; further machines to this set.

(def-c-translation get-platform-code ()
  ((lisp-specs :ftype (() fixnum))
   '(if nonnil-variable-of-unknowable-value-and-type
	15 					; the code for Sun4
       nonnil-variable-of-unknowable-value-and-type))
  ((trans-specs :c-type (() sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "get_platform_code" nil)
   (make-c-function-call-expr
     (make-c-name-expr "get_platform_code") nil)))






;;;; Memory Management




;;; The following functions can allocate more memory into a running GL image and
;;; query about the amount of memory used.

(def-c-translation malloc-block-into-region (region-number byte-count silent)
  ((lisp-specs :ftype ((fixnum fixnum fixnum) void))
   `(derror "No Lisp type expansion for (malloc_block_into_region ~a ~a)"
	   ,region-number ,byte-count ,silent))
  ((trans-specs :c-type ((sint32 sint32 sint32) void))
   (make-c-function-call-expr
     (make-c-name-expr "malloc_block_into_region")
     (list region-number byte-count silent))))

(def-c-translation internal-region-bytes-size (region-number)
  ((lisp-specs :ftype ((fixnum) fixnum))
   `(derror "No Lisp type expansion for (region-bytes-size ~a)"
	   ,region-number))
  ((trans-specs :c-type ((sint32) sint32))
   (make-c-function-call-expr
     (make-c-name-expr "region_number_bytes_size") (list region-number))))

(def-c-translation internal-region-bytes-used (region-number)
  ((lisp-specs :ftype ((fixnum) fixnum))
   `(derror "No Lisp type expansion for (region-bytes-used ~a)"
	   ,region-number))
  ((trans-specs :c-type ((sint32) sint32))
   (make-c-function-call-expr
     (make-c-name-expr "region_number_bytes_used") (list region-number))))

(def-c-translation internal-region-bytes-available (region-number)
  ((lisp-specs :ftype ((fixnum) fixnum))
   `(derror "No Lisp type expansion for (region-bytes-available ~a)"
	   ,region-number))
  ((trans-specs :c-type ((sint32) sint32))
   (make-c-function-call-expr
     (make-c-name-expr "region_number_bytes_available") (list region-number))))
