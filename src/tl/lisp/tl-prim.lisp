(in-package "GL")

;;;; Module GL-PRIM

;;; Copyright (c) 1996 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Miscellaneous Primitive GL Operations




;;; This module implements some primitive operations for GL.  Note that this is
;;; loaded after the types defintions but before LOOP or DO, so only tagbody and
;;; the GLI built-in looping special forms are available.






;;;; Features




;;; The variable `*features*' is implemented as a shared, translatable Lisp
;;; variable.  What this means is that within the Lisp environment this symbol
;;; is EQ to the Lisp package symbol, but in translation the following form
;;; provides a C global variable.

(def-translatable-lisp-var *features* '(:gl))

(def-translatable-lisp-var * nil)





;;; The function `eval-feature' takes a keyword symbol or a form using AND, OR,
;;; or NOT tokens, and returns whether or not the described feature set is
;;; included in the current binding of *features*.

(defun eval-feature (feature-form)
  (let ((loop-var nil))
    (cond ((symbolp feature-form)
	   (gli::for-loop
	     ((setq loop-var *features*)
	      loop-var
	      (setq loop-var (cdr (the cons loop-var))))
	     (if (eq (car (the cons loop-var)) feature-form)
		 (return-from eval-feature feature-form)))
	   nil)
	  ((atom feature-form)
	   ;; This is really an error case, but we don't have error yet.  Return
	   ;; NIL.
	   nil)
	  (t
	   (case (car (the cons feature-form))
	     ((and)
	      (gli::for-loop
		((setq loop-var (cdr (the cons feature-form)))
		 loop-var
		 (setq loop-var (cdr (the cons loop-var))))
		(if (not (eval-feature (car (the cons loop-var))))
		    (return-from eval-feature nil)))
	      feature-form)
	     ((or)
	      (gli::for-loop
		((setq loop-var (cdr (the cons feature-form)))
		 loop-var
		 (setq loop-var (cdr (the cons loop-var))))
		(if (eval-feature (car (the cons loop-var)))
		    (return-from eval-feature feature-form)))
	      nil)
	     ((not)
	      (not (eval-feature (car (the cons (cdr (the cons feature-form)))))))
	     (t nil))))))








;;;; Symbols




;;; The underlying Lisp constant `gl:lambda-list-keywords' is needed at runtime
;;; for some compiler work in NEW-MENUS.

(gli::def-translatable-lisp-constant gl:lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment))




;;; The macro `intern' implements the Common Lisp intern operation.  Since it is
;;; often called with a constant string, we attempt to compute the hash of the
;;; string at compile time by open-coding the call to sxhash-string.  Since
;;; sxhash-string is functional, the translator will constant fold it into the
;;; computed hash number at compile time.  Note that the functions sxhash-string
;;; and intern-string-in-package are defined later, in PACKAGES.  During
;;; development and the first pass of translations this is OK, since we only
;;; expand into calls to those operations during the actual translation pass.

(defmacro intern (string &optional package)
  (if (eval-feature :translator)
      (if (or (constantp string) (symbolp string))
	  (let ((package-arg (if package
				 `(find-package-or-error ,package)
				 '*package*)))
	    `(intern-string-in-package
	       ,string (sxhash-string ,string) ,package-arg))
	  (let ((string-var (gensym)))
	    `(let ((,string-var ,string))
	       (declare (string ,string-var))
	       (intern ,string-var ,@(if package `(,package) nil)))))
      `(lisp:intern ,string ,@(if package `(,package) nil))))




;;; The macro `make-symbol' takes a string and returns an uninterned symbol with
;;; the given print-name.

(defmacro make-symbol (string)
  (if (eval-feature :translator)
      (if (or (constantp string) (symbolp string))
	  `(gli::init-symbol
	     (gli::make-empty-symbol)
	     ,string (sxhash-string ,string))
	  (let ((var (gensym)))
	    `(let ((,var ,string))
	       (gli::init-symbol
		 (gli::make-empty-symbol)
		 ,var (sxhash-string ,var)))))
      `(gli::make-symbol-safely ,string)))

(defmacro boundp (symbol)
  (if (eval-feature :translator)
      `(gli::not-unbound-value-p (symbol-value ,symbol))
      `(lisp:boundp ,symbol)))

(defmacro getf-macro (property-list property &optional default)
  (let ((plist (gensym))
	(prop (gensym)))
    `(let ((,plist ,property-list)
	   (,prop ,property))
       (block nil
	 (gli::for-loop
	   (nil ,plist (setq ,plist (cdr (the cons (cdr (the cons ,plist))))))
	   ;; "When" isn't defined until further on in this file, use "if"
	   ;; instead.
	   (if (eq (car (the cons ,plist)) ,prop)
	       (return-from nil (car (the cons (cdr (the cons ,plist)))))))
	 ,default))))

(defun getf (property-list property &optional default)
  (declare (return-type t))
  (getf-macro property-list property default))

(defun get (symbol property &optional default)
  (declare (return-type t))
  (getf-macro (symbol-plist symbol) property default))

(defun set-get (symbol property new-value)
  (declare (return-type t))
  (if symbol
      (let* ((original-plist (gli::non-null-symbol-plist symbol))
	     (plist original-plist))
	(block nil
	  (gli::for-loop
	    (nil plist (setq plist (cdr (the cons (cdr (the cons plist))))))
	    (cond ((eq (car (the cons plist)) property)
		   (setf (car (the cons (cdr (the cons plist)))) new-value)
		   (return-from nil nil))))
	  (gli::set-non-null-symbol-plist
	    symbol (cons property (cons new-value original-plist)))))
      (let ((plist gli::symbol-plist-of-nil))
	(block nil
	  (gli::for-loop
	    (nil plist (setq plist (cdr (the cons (cdr (the cons plist))))))
	    (cond ((eq (car (the cons plist)) property)
		   (setf (car (the cons (cdr (the cons plist)))) new-value)
		   (return-from nil nil))))
	  (setq gli::symbol-plist-of-nil
		(cons property (cons new-value gli::symbol-plist-of-nil))))))
  new-value)

(defmacro set-get-with-default-value
    (symbol property new-or-default-value &optional (new-value nil supplied?))
  `(set-get ,symbol ,property
	    ,(if supplied? new-value new-or-default-value)))

(defsetf get set-get-with-default-value)






;;;; Funcalling




;;; The macros `funcall-simple-compiled-function' and
;;; `funcall-simple-multi-valued-compiled-function' are used to implement fast
;;; funcall dispatching to functions that are known to take a fixed number of
;;; required arguments, with no special argument list operations.  The first
;;; expects that the called function returns one and only one value, and the
;;; second allows multiple returned values.

(defmacro funcall-simple-compiled-function (compiled-function &rest args)
  `(gli::funcall-internal nil ,compiled-function ,@args))

(defmacro funcall-simple-multi-valued-compiled-function
    (compiled-function &rest args)
  `(gli::funcall-internal t ,compiled-function ,@args))




;;; The macro `funcall-simple-function-symbol' is a special version of funcall
;;; used only for symbols which name functions with a fixed set of arguments,
;;; returning one value, and which have had their types declared with
;;; declare-function-type.

(defmacro funcall-simple-function-symbol (symbol &rest arguments)
  `(funcall-simple-compiled-function (symbol-function ,symbol) ,@arguments))






;;;; Lists





;;; The macro def-car-cdr-suite defines functions for accessors and macros for
;;; mutators.

(gli::def-car-cdr-suite 2 4)

(declaim (functional first rest second third fourth fifth sixth seventh eighth ninth tenth)
	 (inline first rest second third))

(defun first (list)
  (declare (return-type t))
  (car list))

(defmacro set-first (list new-value)
  `(setf (car ,list) ,new-value))

(defsetf first set-first)

(defmacro first-of-conses (list)
  `(car-of-cons ,list))

(defun rest (list)
  (declare (return-type t))
  (cdr list))

(defmacro set-rest (list new-value)
  `(setf (cdr ,list) ,new-value))

(defsetf rest set-rest)

(defmacro rest-of-conses (list)
  `(cdr-of-cons ,list))

(defun second (list)
  (declare (return-type t))
  (cadr list))

(defmacro set-second (list new-value)
  `(setf (cadr ,list) ,new-value))

(defsetf second set-second)

(defmacro second-of-conses (list)
  `(cadr-of-conses ,list))

(defun third (list)
  (declare (return-type t))
  (caddr list))

(defmacro set-third (list new-value)
  `(setf (caddr ,list) ,new-value))

(defsetf third set-third)

(defmacro third-of-conses (list)
  `(caddr-of-conses ,list))

;; Note that the three letter versions of these operations go inline, but the
;; four letter versions do not.

(defun fourth (list)
  (declare (return-type t))
  (car (cdddr list)))

(defmacro set-fourth (list new-value)
  `(setf (car (cdddr ,list)) ,new-value))

(defsetf fourth set-fourth)

(defmacro fourth-of-conses (list)
  `(cadddr-of-conses ,list))

(defun fifth (list)
  (declare (return-type t))
  (cadr (cdddr list)))

(defmacro set-fifth (list new-value)
  `(setf (cadr (cdddr ,list)) ,new-value))

(defsetf fifth set-fifth)

(defmacro fifth-of-conses (list)
  `(car-of-cons (cddddr-of-conses ,list)))

(defun sixth (list)
  (declare (return-type t))
  (caddr (cdddr list)))

(defmacro set-sixth (list new-value)
  `(setf (caddr (cdddr ,list)) ,new-value))

(defsetf sixth set-sixth)

(defmacro sixth-of-conses (list)
  `(cadr-of-conses (cddddr-of-conses ,list)))

(defun seventh (list)
  (declare (return-type t))
  (car (cdddr (cdddr list))))

(defmacro set-seventh (list new-value)
  `(setf (car (cdddr (cdddr ,list))) ,new-value))

(defsetf seventh set-seventh)

(defmacro seventh-of-conses (list)
  `(caddr-of-conses (cddddr-of-conses ,list)))

(defun eighth (list)
  (declare (return-type t))
  (cadr (cdddr (cdddr list))))

(defmacro set-eighth (list new-value)
  `(setf (cadr (cdddr (cdddr ,list))) ,new-value))

(defsetf eighth set-eighth)

(defmacro eighth-of-conses (list)
  `(cadddr-of-conses (cddddr-of-conses ,list)))

(defun ninth (list)
  (declare (return-type t))
  (caddr (cdddr (cdddr list))))

(defmacro set-ninth (list new-value)
  `(setf (caddr (cdddr (cdddr ,list))) ,new-value))

(defsetf ninth set-ninth)

(defmacro ninth-of-conses (list)
  `(car-of-cons (cddddr-of-conses (cddddr-of-conses ,list))))

(defun tenth (list)
  (declare (return-type t))
  (car (cdddr (cdddr (cdddr list)))))

(defmacro set-tenth (list new-value)
  `(setf (car (cdddr (cdddr (cdddr ,list)))) ,new-value))

(defsetf tenth set-tenth)

(defmacro tenth-of-conses (list)
  `(cadr-of-conses (cddddr-of-conses (cddddr-of-conses ,list))))

(defmacro make-list (length &key (initial-element nil))
  `(gli::make-list-1 ,length 1 ,initial-element))

(defmacro list (&rest elements)
  (cond
    ((null elements)
     nil)
    ((null (cdr elements))
     `(cons ,@elements nil))
    (t
     `(gli::set-list-contents
	(gli::make-list-1 ,(gli::length-trans elements) 0 nil)
	,@elements))))

(defmacro list* (element &rest elements)
  (cond ((null elements)
	 element)
	((null (cdr elements))
	 `(cons ,element ,(car elements)))
	(t
	 `(gli::set-list-contents*
	    (gli::make-list-1 ,(gli::length-trans elements) 0 nil)
	    ,element ,@elements))))

(declaim (functional last))

(defun last (list)
  (declare (list list)
	   (return-type list))
  ;; Note that gl:loop is not yet defined.
  (if list
      (let* ((current-cons list)
	     (next-cons? (cdr current-cons)))
	(declare (cons current-cons))
	(gli::while-loop
	  next-cons?
	  (setq current-cons next-cons?)
	  (setq next-cons? (cdr current-cons)))
	current-cons)
      nil))

(defun reverse (sequence)
  (declare (return-type t)
	   (consing-area permanent))
  (typecase sequence
    (list (reverse-list (the list sequence)))
    (string (reverse-string (the string sequence)))
    (t
     (gli::gli-simple-error "REVERSE argument was not a string or list."))))

(defun reverse-list (list)
  (declare (consing-area permanent)
	   (list list)
	   (return-type list))
  (if list
      (let* ((current-cons list)
	     (reversed-list nil))
	(declare (list current-cons reversed-list))
	(gli::while-loop
	  current-cons
	  (setf reversed-list
		(cons (car-of-cons current-cons) reversed-list))
	  (setf current-cons (cdr-of-cons current-cons)))
	reversed-list)
      nil))

(defun reverse-string (string)
  (declare (consing-area permanent)
	   (string string)
	   (return-type string))
  (let* ((length (gli::length-trans string))
	 (new-string (make-string length))
	 (index 0)
	 (reverse-index (1- length)))
    (declare (fixnum length index reverse-index)
	     (string new-string))
    (gli::while-loop
      (< index length)
      (setf (char new-string reverse-index)
	    (char string index))
      (setf index (1+ index))
      (setf reverse-index (1- reverse-index)))
    new-string))

(defun nreverse (list)
  (declare (list list)
	   (return-type list))
  (if list
      (let ((current-cdr (cdr-of-cons list)))
	(setf (cdr list) nil)
	(gli::while-loop
	  current-cdr
	  (let ((next-cdr (cdr-of-cons current-cdr)))
	    (setf (cdr current-cdr) list)
	    (setq list current-cdr)
	    (setq current-cdr next-cdr)))
	list)
      nil))






;;;; Primitive Program Control Operations




;;; This module implements the Common Lisp control operations.

(defmacro prog1 (value-form &body forms)
  (let ((var (gensym)))
    `(let ((,var ,value-form))
       ,@forms
       ,var)))

(defmacro prog2 (form value-form &body forms)
  `(progn ,form (prog1 ,value-form ,@forms)))

(defmacro return (&optional (form  nil))
  (if (null form)
      `(return-from nil)
      `(return-from nil ,form)))

(defmacro prog (values-forms &body decls-and-body)
  (multiple-value-bind (decls body)
      (split-declarations-and-body decls-and-body)
    `(block nil (let ,values-forms ,@decls (tagbody ,@body)))))

(defmacro prog* (values-forms &body decls-and-body)
  (multiple-value-bind (decls body)
      (split-declarations-and-body decls-and-body)
    `(block nil (let* ,values-forms ,@decls (tagbody ,@body)))))






;;;; Modify Macros




(define-modify-macro incf (&optional (increment 1)) +)

(define-modify-macro decf (&optional (decrement 1)) -)


