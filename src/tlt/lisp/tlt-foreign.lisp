(in-package "GLI")

;;;; Module GLT-FOREIGN

;;; Copyright (c) 1997 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Primitive Operations for GL Foreign Function Call Out and Call In




;;; This module implements the macros for defining interfaces to hand-written C
;;; functions.  There are three facilities here, first one for defining call-out
;;; interfaces from Lisp to C foreign functions.  The second for defining
;;; call-out interfaces from Lisp to C macros (providing a Lisp macro to use
;;; instead during Lisp development).  The third is a function for linking
;;; libraries against the set of foreign functions that have been defined, but
;;; not yet bound to C functions within the Lisp environment.

;;; The macro `def-gl-foreign-function' defines a Lisp function that can be
;;; called in order to call a C function.  When expanding during translation
;;; this macro has the side-effect of registering the given C name as the
;;; identifier for the Lisp symbol.

(defmacro gl:def-gl-foreign-function (lisp-name-and-key-forms
				      &rest arg-names-and-types)
  (let* ((lisp-name (first lisp-name-and-key-forms))
	 (key-forms (rest lisp-name-and-key-forms))
	 (language-form (assq :language key-forms))
	 (name-form (assq :name key-forms))
	 (return-type-form (assq :return-type key-forms)))
    `(def-gl-foreign-function-required-args
	 (,lisp-name
	    (:language ,(if language-form
			    (second language-form)
			    :c))
	    (:name ,(if name-form
			(second name-form)
			nil))
	    (:return-type ,(if return-type-form
			       (second return-type-form)
			       nil)))
	 ,@arg-names-and-types)))

(defmacro def-gl-foreign-function-required-args ((lisp-name
						   (&key (language :c))
						   (&key name)
						   (&key return-type))
						 &rest arg-names-and-types)
  (unless (eq language :c)
    (error "GL can only translate foreign calls to :C, not ~s" language))
  (let ((expansion
	  `(gl:progn
	     (gl:declaim (gl:ftype
			   (gl:function
			     ,(loop for (nil type) in arg-names-and-types
				    collect (rewrite-foreign-keyword-type-names
					      type))
			     ,(rewrite-foreign-keyword-type-names return-type))
			   ,lisp-name))
	     (gl:declaim (foreign-c-identifier ,lisp-name ,name)))))
;    (format t "~%~%Made ~s~%" expansion)
    expansion))

(defparameter keyword-to-lisp-type-alist
  '((:fixnum                 . (c-type "long"))
    (:fixnum-long            . (c-type "long"))
    (:fixnum-int             . (c-type "long"))
    (:long                   . (c-type "long"))
    (:signed-32bit           . fixnum)
    (:unsigned-32bit-pointer . (c-type (pointer "uint32")))
    (:char-pointer           . (c-type (pointer "char")))
    (:string                 . (c-type (pointer "char")))
    (:simple-string          . (c-type (pointer "char")))
    (:double-float           . double-float)
    (:8-bit-unsigned-array   . (array (unsigned-byte 8)))
    (:16-bit-unsigned-array  . (array (unsigned-byte 16)))
    (:object                 . t)
    (:void                   . void)))

(defun rewrite-foreign-keyword-type-names (type)
  (or (and (keywordp type)
	   (cdr (assq type keyword-to-lisp-type-alist)))
      type))




;;; The variable `underlying-def-foreign-callable' holds the symbol naming this
;;; port's implementation of def-foreign-callable.  If this variable
;;; returnscontains NIL, then no attempt will be made to enable this feature in
;;; Lisp development.

(defvar underlying-def-foreign-callable
  #+lucid
  'lcl:def-foreign-callable
  #+(and cmu verify)
  'alien:def-alien-routine
  #-(or lucid (and cmu verify))
  nil)

(defun underlying-lisp-callable-type (type)
  #+lucid
  (or (cdr (assq type '((:fixnum-long            . :fixnum)
			(:fixnum-int             . :fixnum)
			(:object                 . :pointer)
			(:string                 . :simple-string)
			(:unsigned-32bit-pointer . (:pointer :unsigned-32bit))
			(:char-pointer           . (:pointer :char))
			)))
      type)
  #+(and cmu verify)
  (or (cdr (assq type '((:fixnum-long            . '(alien (signed 64)))
			(:fixnum-int             . '(alian (signed 32)))
			(:object                 . '(alian '(* void)))
			(:string                 . '(alien '(* (unsigned 16))))
			(:unisgned-32bit-pointer . '(alien '(* (unsigned 32))))
			(:char-pointer           . '(alien '(* char))))))
      type)
  #-lucid
  type)




;;; The macro `gli::def-foreign-callable' is written to be compatible with the
;;; Lucid and Chestnut implementations.  It defines the function in question,
;;; using the same type transforms as are used for foreign-functions.  Note that
;;; this is NOT being made into an exported symbol of GL until the Lisp code has
;;; been rewritten to be able to handle it.

(defmacro gli::def-foreign-callable
    ((lisp-name &rest key-value-pairs)
     args
     &body decls-and-forms)
  (let* ((return-type
	   (or (second (assq :return-type key-value-pairs))
	       (error ":return-type unspecified in def-foreign-callable")))
	 (name (second (assq :name key-value-pairs)))
	 (transformed-return-type
	   (rewrite-foreign-keyword-type-names return-type))
	 (transformed-args
	   (loop for (arg-name arg-type) in args
		 collect (list arg-name
			       (rewrite-foreign-keyword-type-names arg-type))))
	 (translating? (eval-feature :translator))
	 (lisp-implementation? underlying-def-foreign-callable))
    `(gl:progn
       ,@(unless translating?
	   `((gl:declaim
	       (gl:ftype
		 (gl:function ,(loop for arg-spec in transformed-args
				     collect (second arg-spec))
			      ,transformed-return-type)
		 ,lisp-name))))
       ,@(unless (or translating? (null name))
	   `((gl:declaim
	       (function-c-identifier ,lisp-name ,name))))
       ,(cond
	   ((and (not translating?) lisp-implementation?)
	    `(,lisp-implementation?
		(,lisp-name
		   (:return-type ,(underlying-lisp-callable-type return-type))
		   ,@(if name `((:name ,name))))
		,(loop for (arg-name arg-type) in args
		       collect `(,arg-name ,(underlying-lisp-callable-type arg-type)))
		,@decls-and-forms))
	   (t
	    `(gl:defun ,lisp-name ,(loop for (arg-name) in args collect arg-name)
	       (gl:declare
		 (gl:return-type ,transformed-return-type)
		 ,@(loop for (arg-name arg-type) in transformed-args
			 collect `(gl:type ,arg-type ,arg-name)))
	       ,@decls-and-forms))))))






;;;; Inlineable Pseudo Functions


(def-gl-macro gl:def-inlined-pseudo-function-with-side-effects
    (lisp-name args &body lisp-body)
  (unless (eval-feature :translator)
    (let* ((specs? (and (consp lisp-name) lisp-name))
	   (given-return-type (if specs? (second specs?) :object))
	   (lisp-return-type
	     (rewrite-foreign-keyword-type-names given-return-type))
	   (lisp-name (if specs? (car specs?) lisp-name))
	   (c-name
	     (or (and specs? (third specs?))
		 (string-downcase (substitute #\_ #\- (symbol-name lisp-name)))))
	   (given-arg-types
	     (loop for arg in args collect (if (atom arg) :object (second arg))))
	   (lisp-arg-types
	     (loop for type in given-arg-types
		   collect (rewrite-foreign-keyword-type-names type)))
	   (arg-names
	     (loop for arg in args collect (if (atom arg) arg (car arg))))
	   )
      `(def-c-translation ,lisp-name ,arg-names
	 ((lisp-specs :ftype (,lisp-arg-types ,lisp-return-type))
	  (list* 'let
		 (list ,@(loop for name in arg-names
			       collect `(list ',name ,name)))
		 ',lisp-body))
	 ((trans-specs :c-type (,(loop for type in lisp-arg-types
				       collect (c-type-for-lisp-type type))
				 ,(c-type-for-lisp-type lisp-return-type)))
	  (make-c-function-call-expr
	    (make-c-name-expr ,c-name)
	    (list ,@arg-names)))))))

(def-gl-macro gl:def-inlined-pseudo-function (lisp-name args &body lisp-body)
  `(gl:progn
     (gl:declaim (gl:side-effect-free
		   ,(if (consp lisp-name) (car lisp-name) lisp-name)))
     (gl:def-inlined-pseudo-function-with-side-effects ,lisp-name ,args
       ,@lisp-body)))

