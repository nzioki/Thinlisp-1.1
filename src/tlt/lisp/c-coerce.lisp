(in-package "GLI")

;;;; Module C-COERCE

;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; C Type Coercion




;;; This module implements coercions between C types.  Most of the types used in
;;; GLT can be coerced into type Obj and back.  This module implements the
;;; functions and declarations needed for that.

;;; The macro `c-type-coercion-function' takes a C type name symbol and returns
;;; a compiled function that can coerce a C-expr into the given type.  It is
;;; setfable.  The macro `c-pointer-type-coersion-function' takes a C type name
;;; symbol and returns a compiled function that can coerce a C-expr into a
;;; pointer to the given type.  It is setfable.

(defmacro c-type-coercion-function (c-type)
  `(get ,c-type :c-type-coercion-function))

(defmacro c-type-coercion-predicate-function (c-type)
  `(get ,c-type :c-type-coercion-predicate-function))

(defmacro c-pointer-type-coercion-function (c-type)
  `(get ,c-type :c-type-coersion-function))

(defmacro c-pointer-type-coercion-predicate-function (c-type)
  `(get ,c-type :c-pointer-type-coercion-predicate-function))




;;; The macro `def-c-type-coercion' implements C translations for coercions of C
;;; expression results to this type.  It takes a C type name, an argument list
;;; of one element for a C expression structure, and contains a body which looks
;;; like the body of a case statement.  The case keys should be C types returned
;;; by the expression given to this function.  The case clauses should return a
;;; C expression that contains the given C expression argument and coerces that
;;; argument from the case clause type into the target type.

(defmacro def-c-type-coercion (c-type (c-expr-var &optional c-type-var)
				      &body case-clauses)
  (let ((coercion-function-name (intern (format nil "COERCE-~a" c-type)))
	(predicate-function-name (intern (format nil "COERCE-~a-P" c-type)))
	(starting-type (or c-type-var (make-symbol "STARTING-C-TYPE"))))
    `(progn
       (defun ,coercion-function-name (,c-expr-var ,starting-type env)
	 (identity env)
	 (cond
	   ,@(loop for case-clause in case-clauses
		   collect
		   `(,(if (cdr (cons-car case-clause))
			  `(or ,@(loop for c-type in (cons-car case-clause)
				       collect `(satisfies-c-required-type-p
						  ,starting-type
						  ',c-type)))
			  `(satisfies-c-required-type-p
			     ,starting-type
			     ',(cons-car (cons-car case-clause))))
		      ,@(cons-cdr case-clause)))
	   (t
	    (translation-error
	      ,(format nil "Can't coerce C type ~~s into ~a" c-type)
	      ,starting-type))))
       (setf (c-type-coercion-function ',c-type) #',coercion-function-name)
       (defun ,predicate-function-name (starting-type)
	 (loop for coercable-type in ',(loop for case-clause in case-clauses
					     append (cons-car case-clause))
	       thereis (satisfies-c-required-type-p
			 starting-type coercable-type)))
       (setf (c-type-coercion-predicate-function ',c-type)
	     #',predicate-function-name)
       ',c-type)))




;;; The macro `def-c-pointer-type-coercion' implements C translations for
;;; coercions of C expression results into pointers to this type.  It takes a C
;;; type name, an argument list of one element for a C expression, and contains
;;; a body which looks like the body of a case statement.  The case keys should
;;; be C types returned by the expression given to this function.  The case
;;; clauses should return a C expression that contains the given C expression
;;; argument and coerces that argument from the case clause type into a pointer
;;; to the target type.

(defmacro def-c-pointer-type-coercion (c-type (c-expr-var &optional c-type-var)
					      &body case-clauses)
  (let ((coercion-function-name (intern (format nil "COERCE-~a-POINTER" c-type)))
	(predicate-function-name (intern (format nil "COERCE-~a-PTR-P" c-type)))
	(starting-type (or c-type-var (make-symbol "STARTING-C-TYPE"))))
    `(progn
       (defun ,coercion-function-name (,c-expr-var ,starting-type env)
	 (identity env)
	 (cond
	   ,@(loop for case-clause in case-clauses
		   collect
		   `(,(if (cdr (cons-car case-clause))
			  `(or ,@(loop for c-type in (cons-car case-clause)
				       collect `(satisfies-c-required-type-p
						  ,starting-type
						  ',c-type)))
			  `(satisfies-c-required-type-p
			     ,starting-type
			     ',(cons-car (cons-car case-clause))))
		      ,@(cons-cdr case-clause)))
	   (t
	    (translation-error
	      ,(format nil "Can't coerce C type ~~s into a ~a pointer." c-type)
	      ,starting-type))))
       (setf (c-pointer-type-coercion-function ',c-type)
	     #',coercion-function-name)
       (defun ,predicate-function-name (starting-type)
	 (loop for coercable-type in ',(loop for case-clause in case-clauses
					     append (cons-car case-clause))
	       thereis (satisfies-c-required-type-p
			 starting-type coercable-type)))
       (setf (c-pointer-type-coercion-predicate-function ',c-type)
	     #',predicate-function-name)
       '(pointer ,c-type))))




;;; The function `coerce-c-expr-result-to-type' takes a c-expr, the C-type of
;;; its result, and a target C-type.  This function will surround the given
;;; c-expr with whatever coercion operations are needed to coerce from the
;;; current result type into the target result type.  If this cannot be done, a
;;; warning is issued and a call to error is returned.

(defun coerce-c-expr-result-to-type (c-expr c-type target-c-type env)
  (cond
    ((satisfies-c-required-type-p c-type target-c-type) c-expr)
    ((and (consp target-c-type)
	  (memqp (cons-car target-c-type) '(pointer array)))
     (let ((coerce-function
	     (c-pointer-type-coercion-function (cons-second target-c-type))))
       (cond (coerce-function
	      (funcall coerce-function c-expr c-type env))
	     (t
	      (translation-error
		"Can't coerce C type ~s into a ~s pointer."
		c-type target-c-type)
	      c-expr))))
    ((symbolp target-c-type)
     (let ((coerce-function (c-type-coercion-function target-c-type)))
       (cond (coerce-function
	      (funcall coerce-function c-expr c-type env))
	     (t
	      (translation-error
		"Can't coerce C type ~s into a ~s." c-type target-c-type)
	      c-expr))))
    (t
     (translation-error "Invalid target-c-type ~s." target-c-type))))

(defun c-type-can-be-coerced-p (current-type target-type)
  (or (satisfies-c-required-type-p current-type target-type)
      (and (consp target-type)
	   (memqp (cons-car target-type) '(pointer array))
	   (let ((predicate-function
		   (c-pointer-type-coercion-predicate-function
		     (cons-second target-type))))
	     (and predicate-function
		 (funcall predicate-function current-type))))
      (and (symbolp target-type)
	   (let ((predicate-function
		   (c-type-coercion-predicate-function target-type)))
	     (and predicate-function
		  (funcall predicate-function current-type))))))




;;; The function `most-specific-common-c-supertype' takes two C types and
;;; returns a C type that both argument types can be coerced to that is the most
;;; specific type in the intersection between them.  Of course the degenerate
;;; case is that they are equal and so one of them is returned.  If they are not
;;; equal or one of them is void, then a coercion operation to find a common
;;; type, therefore the return type at this point would be void.

(defun most-specific-common-c-supertype (c-type-1 c-type-2)
  (cond ((and (null c-type-1) (null c-type-2))
	 'void)
	((null c-type-1)
	 c-type-2)
	((null c-type-2)
	 c-type-1)
	((satisfies-c-required-type-p c-type-1 c-type-2)
	 c-type-2)
	(t
	 'void)))

(defmacro c-integer-types ()
  `'(sint32 uint8 uint16 unsigned-char))




;;; The function `default-value-for-c-type' takes a C type and returns a C expr
;;; that returns a default value for the given type.  This is generally used in
;;; situations where a stand-in expression is needed for an expression that
;;; could not supply the desired type.

(defun default-value-for-c-type (c-type)
  (cond ((loop for type in (c-integer-types)
	       thereis (satisfies-c-required-type-p c-type type))
	 (make-c-literal-expr 0))
	((satisfies-c-required-type-p c-type 'double)
	 (make-c-literal-expr 0.0))
	(t
	 (make-c-cast-expr c-type (make-c-name-expr "NULL")))))






;;;; C Type Definitions




;;; The following are the declarations and coercion functions for all the C
;;; types.

;; When passing "pointers" straight through to C, the argument type will be
;; pointer to void.

(def-c-type void "void" "void *" nil nil)

(def-c-pointer-type-coercion void (c-expr starting-c-type)
  ((obj)
   (make-c-cast-expr '(pointer void) c-expr))
  ((void)
   (make-c-cast-expr
     '(pointer void)
     (coerce-c-expr-result-to-type
       c-expr starting-c-type 'obj env))))

(def-c-type obj "Obj" "Obj *" nil nil)

(def-c-type-coercion obj (c-expr original-type)
  (((pointer cons))
   (make-c-cast-expr
     'obj
     (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "+" 2)))
  ((sint32 uint32)
   (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-infix-expr c-expr "<<" (make-c-literal-expr 2))
       "+" (make-c-literal-expr 1))))
  ((uint16 uint8 int long size-t)
   (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-infix-expr
	 (make-c-cast-expr 'sint32 c-expr) "<<" (make-c-literal-expr 2))
       "+" (make-c-literal-expr 1))))
  ((unsigned-char)
   (make-c-cast-expr
     'obj (make-c-infix-expr
	    (make-c-infix-expr
	      (make-c-cast-expr 'uint32 c-expr) "<<" (make-c-literal-expr 2))
	    "+" (make-c-literal-expr 3))))
  (((pointer obj))
   (make-c-cast-expr
     'obj (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "-"
			     (make-c-sizeof-expr (c-type-string 'hdr)))))
  (((pointer unsigned-char))
   (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer str) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer uint8))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint8) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer uint16))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint16) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer double))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-double) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
  ((boolean)
   (make-c-conditional-expr
     c-expr
     (make-c-cast-expr 'obj (make-c-unary-expr #\& (make-c-name-expr "T")))
     (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))
  ((double)
   (make-c-function-call-expr
     (make-c-name-expr "alloc_ldouble")
     (list c-expr
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'double-float (declared-area-name env 'double-float)))
	   (make-c-literal-expr (c-type-tag 'ldouble)))))
  (((pointer uint32))
    ;; Objects masquerading as something else coming in from C sometimes are
    ;; passed as pointer to uint32.  Just case them to Obj.
    (make-c-cast-expr 'obj c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'obj (default-value-for-c-type 'obj)))
  )

(defun make-void-cast-stand-in-expr (c-expr original-type target-type default)
  (let ((exprs (list default)))
    (unless (eq original-type 'void)
      (push (make-c-function-call-expr
	      (make-c-name-expr "type_cast_error")
	      (list (make-c-literal-expr (c-type-string 'void))
		    (make-c-literal-expr (c-type-string target-type))))
	exprs))
    (unless (side-effect-free-c-expr-p c-expr)
      (push c-expr exprs))
    (if (null (cdr exprs))
	(car exprs)
	(make-c-comma-expr exprs))))

(def-c-pointer-type-coercion obj (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sv) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer obj)
     (default-value-for-c-type '(pointer obj)))))

(def-c-type cons "Cons" "Cons *" nil nil)

(def-c-pointer-type-coercion cons (c-expr original-expr)
  ((obj)
   (make-c-cast-expr
     '(pointer cons)
     (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "-" 2)))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer cons)
     (default-value-for-c-type '(pointer cons)))))
		     

(def-c-type hdr "Hdr" "Hdr *" nil nil)

(def-c-type sint32 "sint32" "sint32 *" nil nil)

(def-c-type-coercion sint32 (c-expr original-type)
  ((obj)
   (make-c-infix-expr
     (make-c-cast-expr 'sint32 c-expr) ">>" (make-c-literal-expr 2)))
  ((uint32 uint8 uint16 unsigned-char int long size-t)
   (make-c-cast-expr 'sint32 c-expr))
  ((double)
   ;; This is used by rounding and truncation code.
   (make-c-cast-expr 'sint32 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'sint32 (make-c-literal-expr 0))))

(def-c-type uint8 "uint8" "uint8 *" nil nil)

(def-c-type-coercion uint8 (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'uint8
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((uint16 sint32 unsigned-char)
   (make-c-cast-expr 'uint8 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'uint8 (make-c-literal-expr 0))))

(def-c-pointer-type-coercion uint8 (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-uint8) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer uint8)
     (default-value-for-c-type '(pointer uint8)))))

(def-c-type uint16 "uint16" "uint16 *" nil nil)

(def-c-type-coercion uint16 (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'uint16 (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((uint8 sint32 unsigned-char)
   (make-c-cast-expr 'uint16 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'uint16 (make-c-literal-expr 0))))

(def-c-pointer-type-coercion uint16 (c-expr original-expr)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-uint16) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer uint16)
     (default-value-for-c-type '(pointer uint16)))))

(def-c-type uint32 "uint32" "uint32 *" nil nil)

(def-c-pointer-type-coercion uint32 (c-expr original-expr)
  ;; In the OLE code, there is a cheater function that needs this type
  ;; transform.  -jallard 11/3/97
  ((obj)
   (make-c-cast-expr '(pointer uint32) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer uint32)
     (default-value-for-c-type '(pointer uint32)))))

(def-c-type char "char" "char *" nil nil)

(def-c-type unsigned-char "unsigned char" "unsigned char *" nil nil)




;;; The types long and int are only used when casting values for passing to
;;; built-in routines, or casting back the type of returned values.

(def-c-type long "long" "long *" nil nil)

(def-c-type size-t "size_t" "size_t *" nil nil)

(def-c-type int "int" "int *" nil nil)

(def-c-type-coercion int (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'int
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 uint16 char unsigned-char long size-t)
   (make-c-cast-expr 'int c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'int (make-c-literal-expr 0))))

(def-c-type-coercion long (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'long
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 uint16 char unsigned-char int size-t)
   (make-c-cast-expr 'long c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'long (make-c-literal-expr 0))))

(def-c-type-coercion size-t (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'size-t
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 uint16 char unsigned-char int)
   (make-c-cast-expr 'size-t c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'size-t (make-c-literal-expr 0))))

(def-c-type-coercion unsigned-char (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'unsigned-char (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 uint16)
   (make-c-cast-expr 'unsigned-char c-expr))
  ((char)
   (make-c-cast-expr 'unsigned-char c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'unsigned-char (make-c-literal-expr 0))))

(def-c-pointer-type-coercion unsigned-char (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer str) c-expr)
     "body"))
  (((pointer char))
   (make-c-cast-expr '(pointer unsigned-char) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer unsigned-char)
     (default-value-for-c-type '(pointer unsigned-char)))))

(def-c-type-coercion char (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'char (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 uint16)
   (make-c-cast-expr 'char c-expr))
  ((unsigned-char)
   (make-c-cast-expr 'char c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'char (make-c-literal-expr 0))))

(def-c-pointer-type-coercion char (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     '(pointer char)
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer str) c-expr)
       "body")))
  (((pointer unsigned-char))
   (make-c-cast-expr '(pointer char) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer char)
     (default-value-for-c-type '(pointer char)))))

(def-c-type boolean "int" "int *" nil nil)

(def-c-type-coercion boolean (c-expr original-c-type)
  ((obj)
   (make-c-equality-expr c-expr "!=" (make-c-name-expr "NULL")))
  ;; I'm not really sure that the next line is well founded.  Lisp FALSE is
  ;; explicitly only NIL, and integers with zero values are actually equivalent
  ;; to TRUE.  So, for the types that are native C types (as opposed to types
  ;; that optimize Lisp types), provide the convenience of comparing them
  ;; against zero.  -jallard 7/23/97
  ((int long char short size-t unsigned-short)
   (make-c-equality-expr c-expr "!=" (make-c-literal-expr 0)))
  ((void)
   ;; Since all types satisfy the void type, this is the effective else clause
   ;; of this form.  Attempt to cast the value back to object and then compare
   ;; it against NULL.
   (make-c-equality-expr
     (coerce-c-expr-result-to-type
       c-expr original-c-type 'obj env)
     "!=" (make-c-name-expr "NULL"))))

(def-c-type double "double" "double *" nil nil)

(def-c-type-coercion double (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer ldouble) c-expr)
     "body"))
  ((sint32 uint32 uint16 uint8 long int size-t)
   (make-c-cast-expr 'double c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'double (make-c-literal-expr 0.0))))

(def-c-pointer-type-coercion double (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-double) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer double)
     (default-value-for-c-type '(pointer double)))))

(def-c-type mdouble "Mdouble" "Mdouble *" managed-float 4)

(def-c-type ldouble "Ldouble" "Ldouble *" double-float 5)

(def-c-type sv "Sv" "Sv *" simple-vector 6)

(def-c-type str "Str" "Str *" string 7)

(def-c-type sa-uint8 "Sa_uint8" "Sa_uint8 *" (array (unsigned-byte 8)) 8)

(def-c-type sa-uint16 "Sa_uint16" "Sa_uint16 *" (array (unsigned-byte 16)) 9)

(def-c-type sa-double "Sa_double" "Sa_double *" (array double-float) 10)

(def-c-type sym "Sym" "Sym *" symbol 11)

(def-c-type func "Func" "Func *" compiled-function 12)

(defmacro def-c-function-types-for-funcall ()
  (cons 'progn
	(loop for args from 0 to gl:lambda-parameters-limit
	      for c-type-symbol
		  = (intern (format nil "FUNC-~a" args) *gli-package*)
	      for c-type-string = (format nil "Func_~a" args)
	      collect
	      `(def-c-type ,c-type-symbol
		   ,c-type-string
		 ,(format nil "~a *" c-type-string)
		 nil nil))))

(def-c-function-types-for-funcall)

(def-c-type pkg "Pkg" "Pkg *" package 13)

(def-c-type unbound "Hdr" "Hdr *" unbound 14)

(def-c-type string-strm "String_strm" "String_strm *" string-stream 15)

(def-c-type file-strm "File_strm" "File_strm *" file-stream 16)

(def-c-type jmp-buf "jmp_buf" "jmp_buf *" nil nil)

(def-c-type c-function "ERROR" "Obj (*)(Obj)" nil nil)

(def-c-type char-pointer "char *" "char **" nil nil)



;;; Note that you never manipulate files in C, only pointers to them.

(def-c-type file "FILE" "FILE *" nil nil)

