(in-package "GLI")

;;;; Module MACROS

;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Macroexpanding




;;; This module contains the code for performing macro-expansions in the GLT.

;;; The GL symbols are macro-function, defmacro, macroexpand, macroexpand-1,
;;; *macroexpand-hook*, destructuring-bind-strict, define-compiler-macro,
;;; compiler-macro-function, compiler-macroexpand, compiler-macroexpand-1, and
;;; parse-macro.  The only operation not defined from this chapter is enclose,
;;; since we do not intend to implement closures in GL at this time.  The
;;; environment operations variable-information, function-information,
;;; declaration-information, augment-environment, are define-declaration are
;;; defined in the DECLARE module.

;;; The GLT symbol walk is also defined in this module.  It takes a form
;;; and an optional environment, and fully macroexpands the form to all levels.
;;; This is the operation used by the translator to perform its
;;; macroexpansions, and it may be called within macros that which to
;;; pre-expand their full bodies so that analysis can be performed.  The
;;; resulting form will contain only GL special forms, functions, and variable
;;; references.

;;; Note that this macroexpander supports macrolet, symbol-macrolet, and
;;; compiler-macros, ala CLtL 2.  The macros defined are also passed through to
;;; lisp:macro-function.  The gl:macro-function version is defined since it
;;; needs to use the GL environment structures, not the ones native to the
;;; underlying Lisp.







;;;; Macros




;;; The function `macro-function' is a non-translatable GL function that returns
;;; the expansion function for a symbol, if any is defined.  With a null
;;; environment argument, it is setfable.

(defun gl:macro-function (symbol &optional env)
  (multiple-value-bind (type? local?)
      (gl:function-information symbol env)
    (cond ((and (eq type? :macro) local?)
	   local?)
	  ((eq type? :special-form)
	   ;; GL special forms hide macro functions.  This is done so we can
	   ;; have global Lisp macros that provide compatibility when using the
	   ;; underlying Lisp compiler.  However, our own macroexpand will not
	   ;; see the macros for our own special forms.
	   nil)
	  (t
	   (macro-function symbol)))))

(defun set-macro-function (symbol function)
  (setf (macro-function symbol) function))

(defsetf gl:macro-function set-macro-function)






;;;; Defmacro




;;; The function `gl:parse-macro' takes a name, a lambda-list, a body, and an
;;; optional environment and generates a two argument lambda expression (a list
;;; whose car is lambda, suitable for passing to function) which is used to
;;; implement the macro expander functions for defmacro and macrolet.  See CLtL
;;; 2, Sec. 8.5 p. 214 for details.  Note that GL does not support lexical
;;; closures, so the environment argument is ignored.

(defun gl:parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  (if lambda-list
      (multiple-value-bind (decls subbody) (split-declarations-and-body body)
	`(lambda (initial-form env)
	   ;; Make a call on env to avoid unused variable warnings.
	   (identity env)
	   (let ((cons-error-message
		   ,(format nil "The arguments to ~a didn't match up, ~
                                  ~~s found instead of a cons."
			    name)))
	     (gl:destructuring-bind-strict
		 ,(cons nil lambda-list) initial-form
		 ,@decls
		 (let ((cons-error-message default-cons-error-message))
		   (block ,name
		     ,@subbody))))))
      `(lambda (initial-form env)
	 (declare (ignore initial-form env))
	 (block ,name
	   ,@body))))
			    




;;; The macro `gl:defmacro' is used for defining new macros within GL.  It works
;;; just like the Lisp version, though this should be changed to expand to NIL
;;; when translating.  The macro `def-gl-macro' is in the GLI package, but it
;;; expands into gl:defmacro.  It is included so that macros to be defined in
;;; the GL package will still be found by etags and Meta-dot.

(defmacro gl:defmacro (name lambda-list &body body)
  (unless (eval-feature :translator)
    (when (eq (symbol-package name) *lisp-package*)
      (error "Redefining ~s, a symbol in the Lisp package."
	     name))
    ;; When possible, use the built-in defmacro so that arglist fetching in the
    ;; editor works.
    (if (not (memq '&env lambda-list))
	`(defmacro ,name ,lambda-list ,@body)
	(let ((macro-defn-name (intern (format nil "~a-MACRO-FUNCTION" name))))
	  `(progn
	     (eval-when (compile load eval)
	       (defun ,macro-defn-name
		   ,@(cons-cdr (gl:parse-macro name lambda-list body)))
	       (setf (macro-function ',name)
		     (function ,macro-defn-name)))
	     ',name)))))

(defmacro def-gl-macro (name lambda-list &body body)
  `(gl:defmacro ,name ,lambda-list ,@body))






;;;; Macro Expansion




;;; The variable `gl:*macroexpand-hook*' holds an object that can be coerced to
;;; a function and used as the dispatching function into macroexpansion
;;; functions.

(defvar gl:*macroexpand-hook* #'funcall)




;;; The function `gl:macroexpand-1' takes a form and an optional environment and
;;; macroexpands that form once, if it needs it.  It returns the expanded or
;;; original form and a second value that indicates if any macroexpansion was
;;; done.  Note that this form expands symbol-macros, so symbol argument forms
;;; should be passed here as well.  Note that this function does not expand
;;; compiler-macros.  X3J13 in its infinite wisdom decided that only
;;; compiler-macroexpand-1 could do that.

(defun gl:macroexpand-1 (form &optional env)
  (setq env (env-or-default env))
  (let* ((cons? (consp form))
	 (car (if cons? (cons-car form))))
    (cond ((and cons?
		(symbolp car)
		(not (eq (symbol-package car) *lisp-package*)))
	   (multiple-value-bind (type? local?)
	       (gl:function-information car env)
	     (cond
	       ((eq type? :macro)
		(values
		  (funcall gl:*macroexpand-hook*
			   (or local? (macro-function car))
			   form env)
		  t))
	       (t
		(values form nil)))))
	  ((and (symbolp form) form)
	   (multiple-value-bind (type? local?)
	       (gl:variable-information form env)
	     (if (eq type? :symbol-macro)
		 (values local? t)
		 (values form nil))))
	  (t (values form nil)))))




;;; The function `gl:macroexpand' takes a form and an environment and repeated
;;; calls gl:macroexpand-1 on the form and its expansions until it expands no
;;; more.

(defun gl:macroexpand (form &optional env)
  (setq env (env-or-default env))
  (multiple-value-bind (new-form expanded?)
      (gl:macroexpand-1 form env)
    (if expanded?
	(loop do
	  (multiple-value-setq (new-form expanded?)
	    (gl:macroexpand-1 new-form env))
	      while expanded?
	      finally
		(return (values new-form t)))
	(values new-form nil))))






;;;; Compiler Macros




;;; CLtL 2 defined a new facility called compiler-macros.  They are supported in
;;; GL.  See CLtL 2, Sec. 8.4, pp. 205-207 for details.  Note that compiler
;;; macros are not expanded by macroexpand-1, but instead are expanded by
;;; compiler-macroexpand-1.  Walk will expand compiler macros.

;;; The non-translatable function `gl:compiler-macro-function' takes a function
;;; name and an environment and returns the macro-expansion function if there is
;;; a compiler macro for the given name and if that name has not been overridden
;;; by a local function or macro via flet, labels, or macrolet and if the
;;; function has not been declared notinline.  If no global compiler-macro
;;; definition exists, or if the global function has been overridden by a local
;;; function, then this function returns NIL.

;;; As a GL specific extension of this function, a second value is returned.  If
;;; this function has been declared as a local function, the global name of the
;;; local function is returned as a second value.  This will only occur when the
;;; first value is NIL.

(defun gl:compiler-macro-function (symbol &optional env)
  (multiple-value-bind (type? local? decls?)
      (gl:function-information symbol env)
    (if (eq type? :function)
	(if local?
	    (values nil (cdr (assq 'local-function-global-name decls?)))
	    (if (not (eq (cdr (assq 'inline decls?)) 'notinline))
		(get symbol :compiler-macro-definition)
		nil))
	nil)))

(defsetf gl:compiler-macro-function set-compiler-macro-function)

(defun set-compiler-macro-function (symbol new-expander-function)
  (setf (get symbol :compiler-macro-definition) new-expander-function))




;;; The macro `gl:define-compiler-macro' is used to define a macro expansion
;;; that is used to optimize some calls to a function with the same name.
;;; Compiler macros are expanded during compilations of calls to this function
;;; and are also used by walk when expanding forms (walk is a GL
;;; extension).  See CLtL 2, Sec. 8.4, pp. 205-207 for details and an example.

;;; The biggest trick to glean from the example is the use of &whole and
;;; returning the original form when you want to decline to expand this call
;;; further.  Compiler-macroexpand-1 uses an EQ of the result of the expander
;;; with the original form as another means of determining when no expansion
;;; occurred, which in turn is used to stop the gl:compiler-macroexpand from
;;; repeatedly re-expanding the form.

(defmacro gl:define-compiler-macro (name lambda-list &body body)
  (unless (eval-feature :translator)
    `(gl:progn
       (gl:eval-when (compile load eval)
	 (gl:setf (gl:compiler-macro-function ',name)
		  (function ,(gl:parse-macro name lambda-list body))))
       ',name)))




;;; The function `gl:compiler-macroexpand-1' expands calls to compiler macros
;;; just like gl:macroexpand-1 expands normal macros.  CLtL 2, p. 206.

(defun gl:compiler-macroexpand-1 (form &optional env)
  (setq env (env-or-default env))
  (cond
    ((consp form)
     (let ((op-name (cons-car form)))
       (if (symbolp op-name)
	   (multiple-value-bind (expander? global-name-for-local-func?)
	       (gl:compiler-macro-function op-name env)
	     (cond (expander?
		    (let ((new-form (funcall gl:*macroexpand-hook*
					     expander? form env)))
		      (if (eq new-form form)
			  (values form nil)
			  (values new-form t))))
		   (global-name-for-local-func?
		    (values (cons global-name-for-local-func? (cons-cdr form))
			    t))
		   (t
		    (values form nil))))
	   (values form nil))))
    (t (values form nil))))




;;; The function `gl:compiler-macroexpand' repeated performs compiler
;;; macroexpansions on the given form until there are no more to do.  It returns
;;; the expanded form and a second value that indicates whether or not any
;;; expansions were performed.

(defun gl:compiler-macroexpand (form &optional env)
  (setq env (env-or-default env))
  (multiple-value-bind (new-form expanded?)
      (gl:compiler-macroexpand-1 form env)
    (if expanded?
	(loop do
	  (multiple-value-setq (new-form expanded?)
	    (gl:compiler-macroexpand-1 new-form env))
	      while expanded?
	      finally
		(return (values new-form t)))
	(values new-form nil))))






;;;; Code Walking




;;; The function `walk' is used to implement macroexpansions and code walking
;;; through all levels of a given form.  It takes a form, an environment?, and a
;;; walk-function?.  This form will macroexpand and compiler-macroexpand the
;;; given form, recurse to walk the subforms of the expanded form, reconstruct
;;; the returned subforms into an s-expression, then call the given
;;; walk-function and return its result.  The walk function is called with three
;;; arguments, the form, the current environment, and an augmented environment.
;;; In most cases the augmented environment will be EQ to the current
;;; environment, but if the form being given to the walker is a special form,
;;; then the augmented environment is the environment that is in effect for the
;;; subforms of the special form.  If no walk function is given, then walk
;;; returns the the expanded form (this is what macroexpand-all does).

(defun gl:walk (form env walk-function? required-type)
  (setq env (env-or-default env))

  ;; Macroexpand and compiler-macroexpand the given form.  Note that if
  ;; compiler-macroexpansion occurs, then we must loop and check if the
  ;; resulting compiler-macroexpanded form needs further macroexpansion, and
  ;; then further compiler-macroexpansion.
  (loop for macro-expanded? = nil
	for compiler-expanded? = nil
	for first-time? = t then nil
	do
    (multiple-value-setq (form macro-expanded?)
      (gl:macroexpand form env))
    (when (and (or first-time? macro-expanded?)
	       (consp form)
	       (symbolp (car form)))
      (multiple-value-setq (form compiler-expanded?)
	(gl:compiler-macroexpand form env)))
	while compiler-expanded?)

  ;; Recurse into the subforms of the macroexpanded form.  Note that GL walk
  ;; will only recurse into GL defined special-forms and macros.  If a macro or
  ;; special form defined only in the underlying lisp (not through gl:defmacro)
  ;; is given to this form, then neither it nor its subforms will be expanded.
  ;; It will be given to the walk-function, but with its subforms in their
  ;; original state.
  (let ((augmented-environment env))
    (when (and (consp form) (symbolp (car form)))
      (multiple-value-bind (type? local? decls?)
	  (gl:function-information (car form) env)
	(declare (ignore local?))
	(case type?
	  ((:special-form)
	   (let ((special-walker (cdr (assq 'special-form-walker decls?))))
	     (when (null special-walker)
	       (error "No walker function for special form ~A." (car form)))
	     (multiple-value-setq (form augmented-environment)
	       (funcall special-walker form env walk-function? required-type))
	     (when (null augmented-environment)
	       (setq augmented-environment env))))
	  ((:function)
	   (let ((ftype? (cdr (assq 'ftype decls?))))
	     (setq form
		   (cons (car form)
			 (if ftype?
			     (loop with optional?
				     = (position '&optional (second ftype?))
				   with arg-type-list
				     = (if optional?
					   (remove '&optional (second ftype?))
					   (second ftype?))
				   initially
				     (when (if optional?
					       (not (<= optional? (length (cdr form))
							(length arg-type-list)))
					       (/= (length arg-type-list)
						   (length (cdr form))))
				       (translation-warning
					 "Argument list mismatch in a call to ~s~%  arg-types = ~s,~%  arg-list = ~s"
					 (car form) (second ftype?) (cdr form)))
				   for arg in (cdr form)
				   for type in arg-type-list
				   collect (gl:walk arg env walk-function? type))
			     (loop for arg in (cdr form)
				   collect (gl:walk arg env walk-function? t)))))))
	  ((nil)
	   (setq form
		 (cons (car form)
		       (loop for arg in (cdr form)
			     collect (gl:walk arg env walk-function? t))))))))
    
    ;; If the form is a symbol, then it is a reference to the value of a
    ;; variable binding.  Update the variable's structures with information about
    ;; the required type of this reference.
    (when (and (symbolp form) form)
      (additional-variable-getter-type form env required-type))
    
    ;; If a walk-function? is supplied, call it, else return the fully
    ;; macroexpanded form.
    (if walk-function?
	(funcall walk-function? form env augmented-environment required-type)
	form)))






;;;; Macroexpand-all




;;; The function macroexpand-all takes a form and an optional environment, fully
;;; macroexpands the form to all levels, and returns it.

(defun gl:macroexpand-all (form &optional env)
  (gl:walk form env nil t))






;;;; GL Extensions for Macrowriting




;;; There are several utilities defined in GL for writing variables, parameters,
;;; constants, and functions that will only be used at compile time, and which
;;; should not be included in the translated output.  Note that all of these
;;; macros include eval-when compile wrappers around their expansions.  The
;;; macro `defun-for-macro' defines functions that are only included in the
;;; compile-time environment.  The macro `defvar-for-macro' introduces a
;;; variable in the style of defvar.  It arranges to have that variable defined
;;; only when we are defining macros, but not in the translated image.  The macros
;;; `defparameter-for-macro' is the equivalent operation for parameters.

(defmacro gl:defun-for-macro (name arglist &body body)
  (unless (eval-feature :translator)
    `(eval-when (compile load eval) 
       (defun ,name ,arglist
	 ,@body))))

(defmacro gl:defvar-for-macro (name &body body)
  (unless (eval-feature :translator)
    `(eval-when (compile load eval)
       (defvar ,name ,@body))))

(defmacro gl:defparameter-for-macro (name &body body)
  (unless (eval-feature :translator)
    `(eval-when (compile load eval)
       (defparameter ,name ,@body))))






;;;; GSI Size Reduction Bogosity




;;; The following macros are all taken from lisp/bootstrap, and were attempts by
;;; the GSI guys to reduce their image size.  Stubbing these facilities for now
;;; since I think this approach of system-specific translations is bogus and
;;; that we will instead attempt to split things up by files and by subsystems.
;;; When this happens, then conditional compilation by system won't work.
;;; -jallard 2/12/97

(def-gl-macro gl:defvar-excluding-gsi (&rest args)
  `(gl:defvar ,@args))

(def-gl-macro gl:defvar-excluding-gsi-no-utf-g (&rest args)
  `(gl:defvar ,@args))

(def-gl-macro gl:defparameter-excluding-gsi (name &rest args)
  `(gl:defparameter ,name ,@args))

(def-gl-macro gl:defparameter-excluding-no-macros-gsi (name &rest args)
  `(gl:defparameter ,name ,@args))

(def-gl-macro gl:defvar-excluding-no-macros-gsi (name &rest args)
  `(gl:defvar ,name ,@args))

(def-gl-macro gl:defconstant-excluding-gsi (&rest args)
  `(gl:defconstant ,@args))

(define-declaration gl:eliminate-for-no-macros-gsi (decl-spec env)
  (declare (ignore decl-spec env))
  (values :declare nil))

(define-declaration gl:eliminate-for-gsi (decl-spec env)
  (declare (ignore decl-spec env))
  (values :declare nil))

(define-declaration gl:no-op-for-gsi (decl-spec env)
  (declare (ignore decl-spec env))
  (values :declare nil))

(define-declaration gl:no-op-for-no-macros-gsi (decl-spec env)
  (declare (ignore decl-spec env))
  (values :declare nil))

(define-declaration gl:eliminate-for-gsi-no-utf-g (decl-spec env)
  (declare (ignore env decl-spec))
  (values :function nil))

(def-gl-macro gl:defun-for-top-level (name arglist &body decls-and-body)
  `(gl:defun ,name ,arglist
     (gl:declare (gl:allow-unwind-protect))
     ,@decls-and-body))
