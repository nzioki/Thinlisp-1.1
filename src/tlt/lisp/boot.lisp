(in-package "USER")

;;;; Module BOOT

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






;;;; Bootstrapping the Gensym Language Translator




;;; Loading this module will load bootstrapping functions for GLT, including
;;; compile-glt, which you should call to complete the compiling and loading of
;;; the GLT system..  This file sets implementation specific switches, creates
;;; all needed packages, and defines compile-glt.  Note that this does not occur
;;; through glt:def-system, but through minimal code that is made and used only
;;; within this file.

;;; You should bootstrap your Lisp environment with the following code (which
;;; requries that the base level of your sandbox is the current working
;;; directory, e.g. /bt/jra/).

;;; (progn (load "glt/lisp/boot") (compile-glt) (compile-system <system>))






;;;; GLT System Support




;;; For bootstrapping purposes within this file, the parameter `glt-modules',
;;; and the functions `compile-glt' and `load-glt' are defined.
;;; They provide the most minimal system defining support I can get away with to
;;; load GLT itself and to perform "make-like" minimal compiles and loads of
;;; this system.  Note that all files are expected to be within the same
;;; directory and that this will be the default directory when these functions
;;; are executed.

;;; The parameter `glt-modules' contains a list of all lisp file modules in the
;;; GLT system, with the notable exception of this file BOOT.

(defparameter glt-modules
  '(exports
    gli-util
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
    l-trans
    symbols
    l-top
    glt-foreign
    glt-prim
    glt-math
    glt-out
    makefiles
    trans
    ))




;;; The function `compile-glt' will compile and load all modules in GLT as
;;; necessary.  A module is compiled if the corresponding binary file for it
;;; does not exist or if the binary file isn't newer than the lisp file.  A
;;; module is loaded if the :glt-load-date property of the module symbol is
;;; absent or if the file write date within that property is less than the file
;;; write date of the binary file.  Note that this file, BOOT, is handled
;;; specially, and that the value of the parameter glt-modules is not read until
;;; after any new versions of BOOT have already been compiled and loaded.

(defun compile-glt (&key recompile from (safe t))
  ;; If BOOT is not compiled, or the compile is out of date, compile and load
  ;; it.  Otherwise don't bother to compile or load it, since we are already
  ;; running within a function within that file, and so can assume that it has
  ;; been loaded.
  (write-line "Compiling and loading GLT...")
  (unwind-protect
       (progn
	 (if safe
	     (safest-compilations)
	   (fastest-compilations))
	 (loop while recompile
	       with delete-from-preventer = from
	       for module in glt-modules 
	       do
	   (when (and delete-from-preventer
		      (string= (symbol-name delete-from-preventer) 
			       (symbol-name module)))
	     (setq delete-from-preventer nil))
	   (unless delete-from-preventer
	     (delete-glt-module-binary module)))
	 (compile-load-glt-module 'boot (and recompile (null from)) 
				  1 (1+ (length glt-modules)))
	 ;; After loading BOOT, call this function to get into the
	 ;; newest compiled form.
	 (compile-glt-modules recompile from))
    (safest-compilations)))

(defun compile-glt-modules (recompile from)
  (with-compilation-unit ()
    (loop with *readtable* = (copy-readtable nil)
	with recompile-module? = (and recompile (null from))
	with total-modules = (1+ (length glt-modules))
	for module in glt-modules 
	for module-count from 2 do
      (when (and recompile
		 (null recompile-module?)
		 (string= (symbol-name from) (symbol-name module)))
	(setq recompile-module? t))
      (compile-load-glt-module module recompile-module? 
			       module-count total-modules)))
  (let ((gli-compile-glt (intern "COMPILE-GLT" "GLI"))
	(gl-compile-glt (intern "COMPILE-GLT" "GL")))
    (unless (fboundp gli-compile-glt)
      (setf (symbol-function gli-compile-glt)
	(symbol-function 'compile-glt)))
    (unless (fboundp gl-compile-glt)
      (setf (symbol-function gl-compile-glt)
	(symbol-function 'compile-glt)))))

(defconstant lisp-file-type 
   #-aclpc "lisp"
   #+aclpc "lsp")

(defconstant binary-file-type 
    #+lucid                    "sbin"
    #+aclpc                    "acl"
    #+allegro                  "fasl"
    #+cmu                      "x86f"
    #+mcl                      "pfsl"
    #-(or lucid aclpc allegro cmu mcl) "bin")




;;; The function `fastest-compilations' will set the optimize flags to get the
;;; fastest code out of compilations.  Typically this will be the case for GLT
;;; itself.  The function `safest-compilations' sets the optimize flags for
;;; safest code, at the expense of speed.  This will typically be used for
;;; systems defined within GL.

(defun fastest-compilations ()
  (pushnew :fastest-glt *features*)
  (proclaim
;    '(optimize
;      (compilation-speed 3)
;      (speed 1)
;      (safety 3))
    '(optimize
      (compilation-speed 0)
      (speed 3)
      (safety 0))
    ))

(defun safest-compilations ()
  (setq *features* (delete :fastest-glt (the list *features*)))
  (proclaim '(optimize
	      #+cmu(debug 3)
	      (compilation-speed #-cmu 3 #+cmu 2)
	      (speed #-cmu 1 #+cmu 0)
	      (safety 3)
	      )))





;;; The macro `finalize-pathname' takes a pathname and performs any work on
;;; that pathname which might be required by particular Lisp
;;; implementations.  For example, LOAD in Allegro CL for Windows doesn't
;;; work well with relative directory paths, so all pathnames are best
;;; merged with the defaults BEFORE being used.

(defun finalize-pathname (pathname)
;  #+allegro
;  (merge-pathnames pathname)
;  #-allegro
  pathname)




;;; The variable `exports-file-write-date' contains the file write date of the
;;; file glt/lisp/exports.lisp.  Any Lisp, C, or GLT file that is not compiled
;;; up to date with this file will be recompiled.  Since exports of the GL
;;; package are included here, this is a reasonable precaution and is a nice
;;; feature -- it gives us a way to force full recompiles of everything.

(defvar exports-file-write-date nil)

(defun compile-load-glt-module (module force-recompile? count total)
  (let* ((file-name (string-downcase (symbol-name module)))
	 (lisp-file 
	   (finalize-pathname
	     (make-pathname :directory '(:relative "glt" "lisp")
			    :name file-name
			    :type lisp-file-type)))
	 (bin-file 
	  (finalize-pathname (make-pathname
			      :directory '(:relative "glt" "lisp" "dev")
			      :name file-name
			      :type binary-file-type)))
	 (relative-bin-file 
	  #+lucid
	  (finalize-pathname (make-pathname
			      :directory '(:relative "dev")
			      :name file-name
			      :type binary-file-type))
	  #-lucid
	  bin-file)
	 (lisp-date (file-write-date lisp-file))
	 (bin-date? (file-write-date bin-file))
	 (load-date? (get module :glt-load-date)))
    (when (null lisp-date)
      (warn "Module ~a does not have a corresponding lisp file ~a."
	    module lisp-file))
    (when (eq module 'exports)
      (setq exports-file-write-date lisp-date))
    (when (or force-recompile?
	      (null bin-date?)
	      (and lisp-date
		   (<= bin-date? lisp-date))
	      (and exports-file-write-date
		   (<= bin-date? exports-file-write-date)))
      (format t "Compiling ~40a  [~3d/~3d] ~%" lisp-file count total)
      (force-output)
      (compile-file lisp-file :output-file relative-bin-file)
      (setq bin-date? (file-write-date bin-file)))
    (when (or (null load-date?)
	      (/= load-date? bin-date?))
      (format t "Loading   ~40a  [~3d/~3d] ~%" bin-file count total)
      (force-output)
      (load bin-file)
      (setf (get module :glt-load-date) bin-date?))))




;;; The function `delete-glt-module-binary' takes a symbol naming a GLT module.
;;; If the binary file for that module exists, it will be deleted.  This is used
;;; when recompiling, so that a failed recompile can't play gotcha with old
;;; binary files when you attempt to continue compiling after fixing a bug.

(defun delete-glt-module-binary (module)
  (let* ((file-name (string-downcase (symbol-name module)))
	 (bin-file 
	  (finalize-pathname 
	   (make-pathname :directory '(:relative "glt" "lisp" "dev")
			  :name file-name
			  :type binary-file-type))))
    (when (probe-file bin-file)
      (format t "Deleting ~a~%" bin-file)
      (delete-file bin-file))))

			  
	  






;;;; Lisp Implementation Specific Switches




;;; Set the default float format to doubles.

(setq *read-default-float-format* 'double-float)




;;; Set the print-case to :upcase.  This is supposed to be the default value,
;;; but ACL Win 3.02 defaults it to :downcase, which breaks many of my symbol
;;; generating macros.

(setq *print-case* :upcase)




 ;;; For Lucid, we suppress messages about every file being read or created with
;;; the :file-messages option.  We suppress the messages about which compiler is
;;; being used with the :optimize-message option.  All other options are left at
;;; their defaults as documented in the Lucid 4.0 User's Guide, pp. 6-13 through
;;; 6-15.

#+lucid
(lcl:compiler-options :file-messages nil :optimize-message nil)




;;; The variable lcl::*redefinition-action* controls whether or not warnings are
;;; issued for redefined functions.  We will suppress these until we can find a
;;; way for our forward reference declarations to stop causing these bogus
;;; warnings.  -jallard, 6/4/97

#+lucid
(setq lcl::*redefinition-action* nil)




;;; The following features control the temporary area implementations in the
;;; Lisp directories.

#+lucid
(pushnew :using-egc *features*)
#+lucid
(pushnew :no-lucid-temporary-areas *features*)




;;; The Lucid global variable `*load-verbose*' controls whether the load
;;; function issues messages by default every time it loads a file (LCL 4.0
;;; Advanced Users Guide, p. 7-55.  By default it does give messages, here we
;;; turn it off.  Note that all of the calls to load in bootstrap currently
;;; override the default with the :verbose keyword, though calls to load from
;;; this file do not.

;;; The Allegro global variable lisp:*load-verbose* performs the same function.

#+lucid
(setq lcl::*load-verbose* nil)

#+(or aclpc allegro cmu)
(setq *load-verbose* nil)

#+(or allegro cmu)
(setq *compile-verbose* nil
      *compile-print* nil)

#+cmu
(setq *GC-VERBOSE* nil)

#+allegro
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)




;;; The following sets up the standard printer debugger settings for Lucid.

#+lucid
(setq *print-level* nil
      *print-length* nil
      lcl:*debug-print-level* 5
      lcl:*debug-print-length* 20)

#-lucid
(setq *print-level* nil
      *print-length* nil)




;;; The following grows the memory in one chunk so that incremental growth
;;; doesn't have to happen as much.  The number of bytes given should be split
;;; up amongt the various allocation pools for different Lisps so that the total
;;; process size approaches the number given.

(defvar memory-expanded-limit 0)

(defun expand-memory-to-limit (bytes)
  (unless (>= memory-expanded-limit bytes)
    ;; Within Lucid, use two-thirds of the memory for reserved space, and then use
    ;; the other third split amongst the two hemispheres.  -jallard 10/30/97
    #+lucid
    (let ((target-blocks (ceiling bytes 65536))
	  (lcl:*gc-silence* t))
      (lcl:change-memory-management
	;; Kill Lisp if larger than 384 Meg.
	:growth-limit 8000
	;; Grow 3 Meg at a time (2 hemispheres * 25 seqments * 64Kbytes).
	:growth-rate 25
	;; Add one sixth of the expansion to the default in each hemisphere.
	:expand (floor target-blocks 6)
	;; Only expand if there is less than 30% free after a GC, default is 33%
	:reclamation-ratio 0.3
	;; Grow the reserved area by 25 segments at a time, i.e. 1.2 Meg.
	:reserved-growth-rate 25
	;; Add 10.2 Meg reserve to the 7 Meg already reserved.
	:expand-reserved (floor (* target-blocks 2) 3))
      ;; Set the ephemeral level sizes to 1 Meg, 1.2 Meg, and 1.2 Meg to
      ;; attempt to get lots of garbage reclaimed here.  This is double the
      ;; default sizes.  -jra 1/6/92
      (lcl:egc-options :level-sizes '(16 20 20)))
    #-lucid
    nil

    (setq memory-expanded-limit bytes)))

(expand-memory-to-limit 5000000)




;;; The following form suppresses warnings in ACL about differences between
;;; CLtL1 and CLtL2 in eval-when handling of some kinds of top level
;;; forms.  Doc can be found on pp. 7-33 to 7-34 of the ACL 4.3 Volume 1
;;; User Manual.

;;; Turning these warnings back on for now.  -jallard 5/10/99

#+(and allegro ignore)
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)






;;;; Features




;;; The :gl feature is pushed onto features to represent that this translator
;;; has been loaded into the environment.

(pushnew :gl *features*)




;;; Set up features that describe the Lisp version and machine that we are
;;; currently running on.  These are largely inherited from the old features set
;;; up in lisp/load.lisp.

#+(and sun sparc)
(pushnew :sun4 *features*)

#+(and lucid :lcl3.0)
(pushnew :lucid-3 *features*)

#+(and lucid :lcl4.0)
(pushnew :lucid-4 *features*)






;;;; Packages




;;; The GLI (Gensym Language Internals) package is used to implement the
;;; translator.  Users of the translator can find all of its interfacing
;;; functions within the GLT (Gensym Lanaguage Translator) package.  The
;;; language implemented by this translator is found in the GL (Gensym Language)
;;; package.  All symbols exported from these packages are found in the module
;;; EXPORTS.

(unless (find-package "GLI")
  (make-package "GLI"     :use '("LISP")))

(unless (find-package "GLT")
  (make-package "GLT"     :use nil))

(unless (find-package "GL")
  (make-package "GL"      :use nil))

(unless (find-package "AB-LISP")
  (make-package "AB-LISP" :use '("LISP")))

(unless (find-package "GL-USER")
  (make-package "GL-USER" :use '("GL")))

(unless (fboundp (intern "COMPILE-GLT" (find-package "GLI")))
  (setf (symbol-function (intern "COMPILE-GLT" (find-package "GLI")))
	(symbol-function 'compile-glt)))




;;; The symbol `defmacro' must be shadowed for Allegro, since they don't
;;; seem to get the eval-when semantics right.  Sigh, here we go again.

(defmacro defmacro-replacement (name arglist &body decls-and-forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmacro ,name ,arglist ,@decls-and-forms)))

(defun install-replacement-defmacro ()
  (let ((gli-package (find-package "GLI"))
	(ab-lisp-package (find-package "AB-LISP"))
	(replacement (macro-function 'defmacro-replacement)))
    (shadow '(DEFMACRO) gli-package)
    (shadow '(DEFMACRO) ab-lisp-package)
    (setf (macro-function (find-symbol "DEFMACRO" gli-package))
          replacement)
    (setf (macro-function (find-symbol "DEFMACRO" ab-lisp-package))
          replacement)
    nil))

#+(and allegro ignore)
(install-replacement-defmacro)
