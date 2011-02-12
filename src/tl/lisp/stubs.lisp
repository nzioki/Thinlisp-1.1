(in-package "TL")

;;;; Module STUBS

;;; Copyright (c) 1999-2001 The ThinLisp Group
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






;;;; Basic functions for inlining




(declaim (functional eq car cdr)
	 (inline eq car cdr))

(defun eq (x y)
  (declare (return-type t))
  (tli::eq-trans x y))

(defun car (x)
  (declare (return-type t))
  (tli::car-trans x))

(defun cdr (x)
  (declare (return-type t))
  (tli::cdr-trans x))






;;;; Stubs for Operations




;;; This module contains macros that implement stand-in operations for functions
;;; and macros that do not have translatable implementations in TL.  These stubs
;;; should expand into the Lisp versions of the same functions.  These
;;; operations are provided for use in macros, defun-for-macros, and while
;;; debugging the interpreter.  During development of TL, some stub operations
;;; will also be included for operations that we intend to implement as
;;; translatable operations, but have not gotten to yet.






;;;; Stubs yet to be Translated




;;; The following operations are macros that will stand in for operations that
;;; we cannot yet translate, but intend to.  This will allow development of the
;;; TL library while the base is still under construction.

(defmacro file-write-date (&rest args)
  `(common-lisp:file-write-date ,@args))

(defmacro directory (&rest args)
  `(common-lisp:directory ,@args))

(defmacro make-broadcast-stream (&rest args)
  `(common-lisp:make-broadcast-stream ,@args))

(defmacro union (&rest args)
  `(common-lisp:union ,@args))

(defmacro revappend (&rest args)
  `(common-lisp:revappend ,@args))

(defmacro compile-file (&rest args)
  `(common-lisp:compile-file ,@args))

(defmacro load (&rest args)
  `(common-lisp:load ,@args))

(defmacro unread-char (&rest args)
  `(common-lisp:unread-char ,@args))

(defmacro multiple-value-list (&rest args)
  `(common-lisp:multiple-value-list ,@args))

(defmacro coerce (object result-type)
  (if (and (constantp result-type)
	   (tli::tl-subtypep (common-lisp:eval result-type) 'tli::double-float))
      `(coerce-to-double-float ,object)
    `(common-lisp:coerce ,object ,result-type)))

(defmacro pathnamep (object)
  `(common-lisp:pathnamep ,object))

(deftype pathname () 'tli::pathname)

(deftype bignum () 'tli::bignum)

(defmacro rename-file (&rest args)
  `(common-lisp:rename-file ,@args))

(defmacro merge-pathnames (&rest args)
  `(common-lisp:merge-pathnames ,@args))

(defmacro namestring (pathname)
  `(common-lisp:namestring ,pathname))

(defmacro pathname (pathname)
  `(common-lisp:pathname ,pathname))

(defmacro truename (pathname)
  `(common-lisp:truename ,pathname))

(defmacro clear-input (&optional input-stream)
  `(common-lisp:clear-input
     ,@(if input-stream `(,input-stream) nil)))

(defmacro vector (&rest args)
  `(common-lisp:vector ,@args))

(defmacro random (&rest args)
  `(common-lisp:random ,@args))

(defmacro read-char-no-hang (&rest args)
  `(common-lisp:read-char-no-hang ,@args))

(defmacro make-pathname (&key (host nil) (device nil)
			 (directory nil) (name nil) (type nil)
			 (version nil) (defaults nil) (case nil))
  `(common-lisp:make-pathname 
		,@(if host `(:host ,host) nil)
		,@(if device `(:device ,device) nil)
		,@(if directory `(:directory ,directory) nil)
		,@(if name `(:name ,name) nil)
		,@(if type `(:type ,type) nil)
		,@(if version `(:version ,version) nil)
		,@(if defaults `(:defaults ,defaults) nil)
		,@(if case `(:case ,case) nil)))

(defmacro get-decoded-time ()
  `(common-lisp:get-decoded-time))

(defmacro read (&optional input-stream eof-error-p eof-value recursive-p)
  `(common-lisp:read
     ,@(if input-stream `(,input-stream) nil)
     ,@(if eof-error-p `(,eof-error-p) nil)
     ,@(if eof-value `(,eof-value) nil)
     ,@(if recursive-p `(,recursive-p) nil)))

(defmacro pathname-name (pathname &key (case nil))
  `(common-lisp:pathname-name ,pathname
		,@(if case `(:case ,case) nil)))

(defmacro array-element-type (array)
  `(common-lisp:array-element-type ,array))

(defmacro parse-integer (string &key (start nil) (end nil)
				(radix nil) (junk-allowed nil))
  `(common-lisp:parse-integer ,string
		,@(if start `(:start ,start) nil)
		,@(if end `(:end ,end) nil)
		,@(if radix `(:radix ,radix) nil)
		,@(if junk-allowed `(:junk-allowed ,junk-allowed) nil)))

(defmacro type-of (object)
  `(tli::tl-type-of ,object))


(defvar c-native-clock-ticks-per-second 60)
(defvar maximum-backtrace-depth 50)
(defvar def-foreign-function 0)



;;;; Macro Time Only Operations




;;; The following operations are defined only for use in defun-for-macro and
;;; defmacro forms.  If they are used locations that result in attempts to
;;; translate these forms, translation time errors will occur.

(defmacro string-trim (character-bag string)
  `(common-lisp:string-trim ,character-bag ,string))
    
(defmacro fmakunbound (symbol)
  `(common-lisp:fmakunbound ,symbol))

(defmacro gethash (key hashtable &optional (default nil))
  (if (common-lisp:eql default nil)
      `(common-lisp:gethash ,key ,hashtable)
      `(common-lisp:gethash ,key ,hashtable ,default)))

(defsetf gethash set-gethash)

(defmacro set-gethash (key hashtable new-or-default-value
			   &optional (new-value nil supplied?))
  `(common-lisp:setf (common-lisp:gethash ,key ,hashtable
			    ,@(if supplied? `(,new-or-default-value)))
	      ,(if supplied? new-value new-or-default-value)))
    
(defmacro eval (form)
  `(common-lisp:eval ,form))

(defmacro butlast (list &optional (count 1))
  (if (common-lisp:eql count 1)
      `(common-lisp:butlast ,list)
      `(common-lisp:butlast ,list ,count)))

(defmacro subst (&rest args)
  `(common-lisp:subst ,@args))

(defmacro sublis (&rest args)
  `(common-lisp:sublis ,@args))

(defmacro do-symbols (&rest args)
  `(common-lisp:do-symbols ,@args))

(defmacro do-all-symbols (&rest args)
  `(common-lisp:do-symbols ,@args))

(defmacro maphash (function hash-table)
  `(common-lisp:maphash ,function ,hash-table))

(defmacro get-universal-time ()
  `(common-lisp:get-universal-time))

(defmacro decode-universal-time (&rest args)
  `(common-lisp:decode-universal-time ,@args))

(defmacro encode-universal-time (&rest args)
  `(common-lisp:encode-universal-time ,@args))

(defmacro subseq (sequence start &optional end)
  `(common-lisp:subseq ,sequence ,start ,@(if end `(,end) nil)))

(defmacro remove-duplicates (sequence &key (from-end nil) (test nil)
			 (test-not nil) (start nil) (end nil) (key nil))
  `(common-lisp:remove-duplicates ,sequence
		,@(if from-end `(:from-end ,from-end) nil)
		,@(if test `(:test ,test) nil)
		,@(if test-not `(:test-not ,test-not) nil)
		,@(if start `(:start ,start) nil)
		,@(if end `(:end ,end) nil)
		,@(if key `(:key ,key) nil)))

(defmacro ldiff (list sublist)
  `(common-lisp:ldiff ,list ,sublist))

(defmacro compile (name &optional definition)
  `(common-lisp:compile ,name ,@(if definition `(,definition) nil)))

(defmacro disassemble (name-or-compiled-function)
  `(common-lisp:disassemble ,name-or-compiled-function))

(defmacro inspect (object)
  `(common-lisp:inspect ,object))

(defmacro write-to-string (object &rest args)
  `(common-lisp:write-to-string ,object ,@args))

