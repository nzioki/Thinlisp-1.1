(in-package "GLI")

;;;; Module SETF

;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Generalized Variable Setting




;;; This module implements setf and its setter function defining buddies.  Note
;;; that we have departed from CLtL II here in that setter functions are not
;;; named using a list whose car is setf.  In fact, there is no concept of
;;; setter functions here.  What we do have is a way of associating an operation
;;; with a setter-operation (the simple form of defsetf), a way of associating a
;;; setf-method with an operation (the complex form of defsetf), and we have
;;; location mutating macros that perform macro expansions of these defined
;;; getter-to-setter relationships.  This is the optimization that most Lisp
;;; implementations provide anyway, we are only declining to add the function
;;; interface to this batch of information.

;;; The macro `gl:defsetf' associates a

(defmacro simple-setf-update-fn (access-fn)
  `(get ,access-fn :simple-setf-update-fn))

(defmacro setf-method (access-fn)
  `(get ,access-fn :setf-method))

(defmacro gl:define-modify-macro (name lambda-list function)
  (let* ((env (gensym))
	 (access-function-returning-form
	   `(append
	      '(,function)
	      (list place)
	      ,@(loop for arg-cons = lambda-list then (cdr arg-cons)
		      with within-keyword-args? = nil
		      while arg-cons
		      for arg = (cons-car arg-cons)
		      for arg-symbol = (if (consp arg) (car arg) arg)
		      nconcing
		      (cond ((eq arg '&rest)
			     (setq arg-cons (cdr arg-cons))
			     (list (car arg-cons)))
			    ((eq arg '&optional)
			     nil)
			    ((eq arg '&key)
			     (setq within-keyword-args? t)
			     nil)
			    (within-keyword-args?
			     (list `(list
				      ,(intern (symbol-name arg-symbol)
					       *keyword-package*))
				   `(list ,arg-symbol)))
			    (t
			     (list `(list ,arg-symbol))))))))
    `(gl:defmacro ,name ,(append `(&environment ,env place) lambda-list)
       (cond
	 ((and (symbolp place)
	       (not (eq (gl:variable-information place ,env)
			:symbol-macro)))
	  `(gl:setf ,place ,,access-function-returning-form))
	 ((and (consp place)
	       (eq (cons-car place) 'gl:the))
	  (multiple-value-bind (vars vals stores store-form access-form)
	      (gl:get-setf-expansion (cons-third place) ,env)
	    `(gl:let* ,(loop for var in (append vars stores)
			     for value
				 in (append
				      vals
				      (let* ((type (cons-second place))
					     (place `(gl:the ,type ,access-form)))
					`((gl:the
					    ,type
					    ,,access-function-returning-form))))
			     collect `(,var ,value))
	       (gl:declare (gl:type ,(cons-second place) ,(car stores)))
	       ,store-form)))
	 (t
	  (multiple-value-bind (vars vals stores store-form access-form)
	      (gl:get-setf-expansion place ,env)
	    `(gl:let* ,(loop for var in (append vars stores)
			     for value
				 in (append
				      vals
				      (let ((place access-form))
					`(,,access-function-returning-form)))
			     collect `(,var ,value))
	       ,store-form)))))))

(def-gl-macro gl:define-setf-method (name lambda-list &body body)
  (when (eval-feature :translator)
    (return-from gl:define-setf-method nil))
  (let ((setf-method-name (intern (format nil "~a-SETF-METHOD" name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,setf-method-name
	     ,@(cons-cdr (gl:parse-macro name lambda-list body)))
	 (setf (setf-method ',name)
	       (function ,setf-method-name)))
       ',name)))


(def-gl-macro gl:defsetf (access-fn update-fn-or-lambda-list
				    &rest store-var-and-body)
  (when (eval-feature :translator)
    (return-from gl:defsetf nil))
  `(gl:eval-when (:compile-toplevel :load-toplevel :execute)
     ,(cond
	((null store-var-and-body)
	 `(setf (simple-setf-update-fn ',access-fn)
		',update-fn-or-lambda-list))
	(t
	 (let ((store-var (cons-caar store-var-and-body))
	       (body (cons-cdr store-var-and-body))
	       (arg-vars (loop for arg-cons on update-fn-or-lambda-list
			       for arg = (cons-car arg-cons)
			       do
			   (when (eq arg '&environment)
			     (setq arg-cons (cons-cddr arg-cons))
			     (setq arg (cons-car arg-cons)))
			       unless (memqp arg '(&optional &rest &key))
				 collect (if (consp arg) (car arg) arg))))
	   `(gl:define-setf-method ,access-fn ,update-fn-or-lambda-list
	      (let ((temps (list ,@(loop repeat (length arg-vars)
					 collect '(gensym))))
		    (value-var (gensym)))
		(values temps
			(list ,@arg-vars)
			(list value-var)
			(gl:destructuring-bind-strict
			  ,(cons store-var arg-vars)
			  (cons value-var temps)
			  ,@body)
			`(,',access-fn ,@temps)))))))))


(def-gl-macro gl:setf (&environment env &rest places-and-values)
  (cond
    ((null places-and-values)
     nil)
    ((/= (length places-and-values) 2)
     `(gl:progn
	,@(loop for pair-cons on places-and-values by #'cddr
		for place = (cons-car pair-cons)
		for value = (if (consp (cons-cdr pair-cons))
				(cons-second pair-cons)
				(error "Uneven number of arguments to setf ~s"
				       places-and-values))
		collect `(gl:setf ,place ,value))))
    (t
     (let ((place (cons-car places-and-values))
	   (value (cons-second places-and-values)))
       (cond
	 ((symbolp place)
	  (multiple-value-bind (binding-type local-binding)
	      (gl:variable-information place env)
	    (cond ((not (eq binding-type :symbol-macro))
		   `(gl:setq ,place ,value))
		  (t
		   `(gl:setf ,local-binding ,value)))))
	 ((or (not (consp place)) (not (symbolp (cons-car place))))
	  (error "SETF place ~s was not a valid form." place))
	 ((eq (cons-car place) 'gl:the)
	  `(gl:setf ,(cons-third place) (gl:the ,(cons-second place) ,value)))
	 (t
	  (let* ((op (cons-car place))
		 (simple-update? (simple-setf-update-fn op))
		 (setf-method? (if (null simple-update?) (setf-method op))))
	    (cond
	      (simple-update?
	       `(,simple-update? ,@(cons-cdr place) ,value))
	      (setf-method?
	       (multiple-value-bind (vars vals stores store-form)
		   (funcall setf-method? place env)
		 `(gl:let* ,(loop for var in (append vars stores)
				  for value in (append vals (list value))
				  collect (list var value))
		    ,store-form)))
	      (t
	       (multiple-value-bind (new-place expanded?)
		   (gl:macroexpand-1 place env)
		 (cond
		   (expanded?
		     `(gl:setf ,new-place ,value))
		   ((not (eval-feature :translator))
		    ;; Give the underlying Lisp level a shot at setfing this form.
		    `(setf ,place ,value))
		   (t
		    (error "SETF cannot find a setter for ~s" place)))))))))))))




;;; See setf and get-setf-expansion documentation in CLtL2, p. 140.

(defun gl:get-setf-method (place &optional env)
  (gl:get-setf-expansion place env))

(defun gl:get-setf-expansion (place &optional env)
  (cond
    ((symbolp place)
     (multiple-value-bind (binding-type local-binding)
	 (gl:variable-information place env)
       (cond ((eq binding-type :symbol-macro)
	      (gl:get-setf-expansion local-binding env))
	     (t
	      (let ((new-value (gensym)))
		(values nil nil (list new-value)
			`(gl:setq ,place ,new-value)
			place))))))
    ((or (not (consp place)) (not (symbolp (cons-car place))))
     (error "GET-SETF-EXPANSION place ~s was not a valid form." place))
    (t
     (let* ((op (cons-car place))
	    (simple-update? (simple-setf-update-fn op))
	    (setf-method? (if (null simple-update?) (setf-method op))))
       (cond
	 (simple-update?
	  (let ((temporaries (loop repeat (length (cons-cdr place))
				   collect (gensym)))
		(store-var (gensym)))
	    (values
	      temporaries
	      (cons-cdr place)
	      (list store-var)
	      `(,simple-update? ,@temporaries ,store-var)
	      `(,op ,@temporaries))))
	 (setf-method?
	  (funcall setf-method? place env))
	 (t
	  (multiple-value-bind (new-place expanded?)
	      (gl:macroexpand-1 place env)
	    (cond
	      (expanded?
	       (gl:get-setf-expansion new-place env))
	      ((not (eval-feature :translator))
	       ;; When we are not translating, let the underlying Lisp
	       ;; implementation have a shot at the expansion.
	       (get-setf-expansion place))
	      (t
	       (error "GET-SETF-EXPANSION cannot find a setter for ~s" place))))))))))

