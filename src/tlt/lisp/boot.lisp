(in-package "CL-USER")

;;;; Module BOOT

;;; Copyright (c) 1999-2001 The ThinLisp Group
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






;;;; Bootstrapping the Gensym Language Translator




;;; Loading this module will load bootstrapping functions for TLT, including
;;; compile-tlt, which you should call to complete the compiling and loading of
;;; the TLT system..  This file sets implementation specific switches, creates
;;; all needed packages, and defines compile-tlt.  Note that this does not occur
;;; through tlt:def-system, but through minimal code that is made and used only
;;; within this file.

;;; You should bootstrap your Lisp environment with the following code (which
;;; requries that the base level of your sandbox is the current working
;;; directory, e.g. /bt/jra/).

;;; (progn (load "tlt/lisp/boot") (compile-tlt) (compile-system <system>))


;;;; TLT System Support


;;; For bootstrapping purposes within this file, the parameter `tlt-modules',
;;; and the functions `compile-tlt' and `load-tlt' are defined.
;;; They provide the most minimal system defining support I can get away with to
;;; load TLT itself and to perform "make-like" minimal compiles and loads of
;;; this system.  Note that all files are expected to be within the same
;;; directory and that this will be the default directory when these functions
;;; are executed.

;;; The parameter `tlt-modules' contains a list of all lisp file modules in the
;;; TLT system, with the notable exception of this file BOOT.

(defparameter tlt-modules
  '(exports
    tli-util
    system
    destruct
    env
    types
    decls
    macros
    special
    defun
    defvar
    regions
    setf
    ;bit-pack
    ;clos
    defstruct
    backquote
    c-names
    c-files
    c-types
    c-expr
    c-decls
    c-state
    c-func
    c-type-util
    c-coerce
    trandata
    l-expr
    l-const
    l-stack
    l-trans
    symbols
    l-top
    tlt-foreign
    tlt-prim
    tlt-math
    tlt-out
    makefiles
    trans
    ))


;;;; Packages

;;; All symbols exported from these packages are found in the module
;;; EXPORTS.

(defpackage "TL"
  (:documentation "ThinLisp package; contains the language implemented by the ThinLisp translator.")
  (:use "COMMON-LISP")
  (:export #:function
           #:define-declaration))

(defpackage "TLI"
  (:use "COMMON-LISP")
  (:documentation "The TLI (ThinLisp Internals) package is used to implement the translator.")
  (:import-from "TL" #:define-declaration))

(defpackage "TLT"
  (:use "COMMON-LISP")
  (:documentation "TLT (ThinLisp Translator) package contains all ThinLisp translator interfacing functions."))

(defpackage "AB-LISP"
  (:use "COMMON-LISP"))

(defpackage "TL-USER"
  (:use "TL"))

;;; Set the copyright string that will appear in the heading comment
;;; of generated files

(defvar tl-user::*TL-CURRENT-PROJECT-COPYRIGHT-STRING*
  "Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.")


;;; Some pathnames crud

(defvar *lisp-file-type* "lisp")

(defvar *binary-file-type* 
    #+lucid                    "sbin"
    #+aclpc                    "acl"
    #+allegro                  "fasl"
    #+cmu                      "x86f"
    #+mcl                      "pfsl"
    #+clisp                    "fas"
    #-(or lucid aclpc allegro cmu mcl clisp) "bin")


;;; The function `compile-tlt' will compile and load all modules in TLT as
;;; necessary.  A module is compiled if the corresponding binary file for it
;;; does not exist or if the binary file isn't newer than the lisp file.  A
;;; module is loaded if the :tlt-load-date property of the module symbol is
;;; absent or if the file write date within that property is less than the file
;;; write date of the binary file.  Note that this file, BOOT, is handled
;;; specially, and that the value of the parameter tlt-modules is not read until
;;; after any new versions of BOOT have already been compiled and loaded.

(defun compile-tlt (&key recompile from (safe t))
  ;; If BOOT is not compiled, or the compile is out of date, compile and load
  ;; it.  Otherwise don't bother to compile or load it, since we are already
  ;; running within a function within that file, and so can assume that it has
  ;; been loaded.
  (write-line "Compiling and loading TLT...")
  (unwind-protect
       (progn
	 (if safe
	     (safest-compilations)
	   (fastest-compilations))
	  (loop with delete-from-preventer = from
 	       for module in tlt-modules 
 	       while recompile
 	       do
 	   (when (and delete-from-preventer
 		      (string= (symbol-name delete-from-preventer) 
 			       (symbol-name module)))
 	     (setq delete-from-preventer nil))
 	   (unless delete-from-preventer
 	     (delete-tlt-module-binary module)))
	 (compile-load-tlt-module 'boot (and recompile (null from)) 
				  1 (1+ (length tlt-modules)))
	 ;; After loading BOOT, call this function to get into the
	 ;; newest compiled form.
	 (compile-tlt-modules recompile from))
    (safest-compilations)))

(defmacro with-deferred-warnings (&body forms) 
  `(with-compilation-unit () ,@forms))

(defun compile-tlt-modules (recompile from)
  (with-deferred-warnings
    (loop with *readtable* = (copy-readtable nil)
	  with recompile-module? = (and recompile (null from))
	  with total-modules = (1+ (length tlt-modules))
	  for module in tlt-modules 
	  for module-count from 2 do
      (when (and recompile
		 (null recompile-module?)
		 (string= (symbol-name from) (symbol-name module)))
	(setq recompile-module? t))
      (compile-load-tlt-module module recompile-module? 
			       module-count total-modules)))
  (let ((tli-compile-tlt (intern "COMPILE-TLT" "TLI"))
	(tl-compile-tlt (intern "COMPILE-TLT" "TL")))
    (unless (fboundp tli-compile-tlt)
      (setf (symbol-function tli-compile-tlt)
	(symbol-function 'compile-tlt)))
    (unless (fboundp tl-compile-tlt)
      (setf (symbol-function tl-compile-tlt)
	(symbol-function 'compile-tlt)))))


;;; The function `fastest-compilations' will set the optimize flags to get the
;;; fastest code out of compilations.  Typically this will be the case for TLT
;;; itself.  The function `safest-compilations' sets the optimize flags for
;;; safest code, at the expense of speed.  This will typically be used for
;;; systems defined within TL.

(defun fastest-compilations ()
  (pushnew :fastest-tlt *features*)
  (proclaim
;    '(optimize
;      (compilation-speed 3)
;      (speed 1)
;      (safety 3))
    '(optimize
;      #+cmu (c::brevity 1)
      (compilation-speed 0)
      (speed 3)
      (safety 0)
      )
    ))

(defun safest-compilations ()
  (setq *features* (delete :fastest-tlt (the list *features*)))
  (proclaim '(optimize
	      #+cmu (debug 3)
	      (compilation-speed #-cmu 3 #+cmu 2)
	      (speed #-cmu 1 #+cmu 0)
	      (safety 3)
	      )))


;;; all this stuff should go into a defsystem-type facility

;;; The variable `exports-file-write-date' contains the file write date of the
;;; file tlt/lisp/exports.lisp.  Any Lisp, C, or TLT file that is not compiled
;;; up to date with this file will be recompiled.  Since exports of the TL
;;; package are included here, this is a reasonable precaution and is a nice
;;; feature -- it gives us a way to force full recompiles of everything.

(defvar exports-file-write-date nil)

(defun compile-load-tlt-module (module force-recompile? count total)
  (let* ((file-name (string-downcase (symbol-name module)))
	 (lisp-file (merge-pathnames (make-pathname :name file-name
						    :type *lisp-file-type*)
				     *tlt-source-path*))
	 (bin-file (merge-pathnames (make-pathname :name file-name
						   :type *binary-file-type*)
				    *tlt-bin-path*))
	 (lisp-date (and (probe-file lisp-file)
			 (file-write-date lisp-file)))
	 (bin-date? (and (probe-file bin-file)
			 (file-write-date bin-file)))
	 (load-date? (get module :tlt-load-date)))
    (when (null lisp-date)
      (warn "Module ~a does not have a corresponding lisp file ~a."
	    module lisp-file))
    (ensure-directories-exist bin-file :verbose nil)
    (when (eq module 'exports)
      (setq exports-file-write-date lisp-date))
    (when (or force-recompile?
	      (null bin-date?)
	      (and lisp-date
		   (<= bin-date? lisp-date))
	      (and exports-file-write-date
		   (<= bin-date? exports-file-write-date)))
      ;; The following weird construction forces line output buffering.
      (write-string (format nil "Compiling   ~40a    [~3d/~3d] ~%" lisp-file count total))
      (force-output)
      (compile-file lisp-file :output-file bin-file :verbose nil :print nil)
      (setq bin-date? (file-write-date bin-file)))
    (when (or (null load-date?)
	      (/= load-date? bin-date?))
      ;; The following weird construction forces line output buffering.
      (write-string (format nil "Loading     ~40a    [~3d/~3d] ~%" bin-file count total))
      (force-output)
      (load bin-file :verbose nil)
      (setf (get module :tlt-load-date) bin-date?))))


  ;; (defun compile-load-tlt-module (module force-recompile-p count total)
;;     (let* ((file-name (string-downcase (symbol-name module)))
;;   	 (lisp-file 
;;   	  (merge-pathnames *tlt-source-path*
;;   			   (pathname (concatenate 'string file-name ".lisp"))))
;;   	 (lisp-date (and (probe-file lisp-file)
;;   			 (file-write-date lisp-file)))
;;   	 (load-date (get module :tlt-load-date)))
;;       (when (null lisp-date)
;;         (warn "Module ~a does not have a corresponding lisp file ~a."
;;   	    module lisp-file))
;;       (when (eq module 'exports)
;;         (setq exports-file-write-date lisp-date))
;;       (when (or force-recompile-p
;;   	      (and lisp-date
;;   	      (and exports-file-write-date

;;         ;; The following weird construction forces line output buffering.
;;         (write-string (format nil "Compiling   ~40a    [~3d/~3d] ~%" lisp-file count total))
;;         (force-output)
;;         (compile-file lisp-file)
;;         (setq bin-date? (file-write-date bin-file)))
;;       (when (or (null load-date)
;;   	      (/= load-date bin-date?))
;;         ;; The following weird construction forces line output buffering.
;;         (write-string (format nil "Loading     ~40a    [~3d/~3d] ~%" bin-file count total))
;;         (force-output)
;;         (load bin-file :verbose nil)
;;         (setf (get module :tlt-load-date) bin-date?))))


;; (defun compile-load-tlt-module (module force-recompile-p count total)
;;   (let* ((file-name (string-downcase (symbol-name module)))
;;  	 (lisp-file (merge-pathnames *tlt-source-path*
;; 				     (pathname (concatenate 'string file-name ".lisp")))))
;;     (write-string (format nil "Compiling   ~40a    [~3d/~3d] ~%" lisp-file count total))
;;     (force-output)
;;     (load (compile-file lisp-file))))


;;; The function `delete-tlt-module-binary' takes a symbol naming a TLT module.
;;; If the binary file for that module exists, it will be deleted.  This is used
;;; when recompiling, so that a failed recompile can't play gotcha with old
;;; binary files when you attempt to continue compiling after fixing a bug.

 (defun delete-tlt-module-binary (module)
   (let* ((file-name (string-downcase (symbol-name module)))
	  (bin-file (merge-pathnames (make-pathname :name file-name
						    :type *binary-file-type*)
				     *tlt-bin-path*)))
     (when (probe-file bin-file)
       (format t "Deleting ~a~%" bin-file)
       (delete-file bin-file))))


;;; Set the default float format to doubles.

(setq *read-default-float-format* 'double-float)


;;; Set the print-case to :upcase.  This is supposed to be the default value,
;;; but ACL Win 3.02 defaults it to :downcase, which breaks many of my symbol
;;; generating macros.

(setq *print-case* :upcase)


;;;; Features

(pushnew :tl *features*)


(unless (fboundp (intern "COMPILE-TLT" (find-package "TLI")))
  (setf (symbol-function (intern "COMPILE-TLT" (find-package "TLI")))
	(symbol-function 'compile-tlt)))




;;; The symbol `defmacro' must be shadowed for Allegro, since they don't
;;; seem to get the eval-when semantics right.  Sigh, here we go again.

;; (defmacro defmacro-replacement (name arglist &body decls-and-forms)
;;   `(eval-when (:compile-toplevel :load-toplevel :execute)
;;      (defmacro ,name ,arglist ,@decls-and-forms)))

;; (defun install-replacement-defmacro ()
;;   (let ((tli-package (find-package "TLI"))
;; 	(ab-lisp-package (find-package "AB-LISP"))
;; 	(replacement (macro-function 'defmacro-replacement)))
;;     (shadow '(DEFMACRO) tli-package)
;;     (shadow '(DEFMACRO) ab-lisp-package)
;;     (setf (macro-function (find-symbol "DEFMACRO" tli-package))
;;           replacement)
;;     (setf (macro-function (find-symbol "DEFMACRO" ab-lisp-package))
;;           replacement)
;;     nil))