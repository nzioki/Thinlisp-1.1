(in-package "USER")

;;;; Module BOOT

;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






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
	 (compile-load-glt-module 'boot (and recompile (null from)))
	 (loop with recompile-module? = (and recompile (null from))
	       for module in glt-modules do
	   (when (and recompile
		      (null recompile-module?)
		      (string= (symbol-name from) (symbol-name module)))
	     (setq recompile-module? t))
	   (compile-load-glt-module module recompile-module?))
	 (let ((gli-compile-glt (intern "COMPILE-GLT" "GLI"))
	       (gl-compile-glt (intern "COMPILE-GLT" "GL")))
	   (unless (fboundp gli-compile-glt)
	     (setf (symbol-function gli-compile-glt)
		   (symbol-function 'compile-glt)))
	   (unless (fboundp gl-compile-glt)
	     (setf (symbol-function gl-compile-glt)
		   (symbol-function 'compile-glt)))))
    (safest-compilations)))

(defconstant lisp-file-type 
   #-aclpc "lisp"
   #+aclpc "lsp")

(defconstant binary-file-type 
    #+lucid                    "sbin"
    #+aclpc                    "acl"
    #+allegro                  "fasl"
    #-(or lucid aclpc allegro) "bin")




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

(defun compile-load-glt-module (module force-recompile?)
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
      (format t "Compiling ~a~%" lisp-file)
      (force-output)
      (compile-file lisp-file :output-file relative-bin-file)
      (setq bin-date? (file-write-date bin-file)))
    (when (or (null load-date?)
	      (/= load-date? bin-date?))
      (format t "Loading   ~a~%" bin-file)
      (force-output)
      (load bin-file)
      (setf (get module :glt-load-date) bin-date?))))






;;;; Lisp Implementation Specific Switches




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




;;; The following sets up the standard printer debugger settings for Lucid.

#+lucid
(setq *print-level* nil
      *print-length* nil
      lcl:*debug-print-level* 5
      lcl:*debug-print-length* 20)




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

#+allegro
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)






;;;; Features




;;; The :glt feature is pushed onto features to represent that this translator
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




;;; The following features are old deadwood that are needed by various bits of
;;; AB lisp code, but which have not been expunged from the sources.  As much as
;;; possible, these should be eliminated sometime in the rosy future when we
;;; have all the time we could ever want.  -jra 12/30/96

#+unix
(pushnew :x11-windows *features*)

#+unix
(pushnew :x11-unix *features*)






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
  `(eval-when (compile load eval)
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

#+allegro
(install-replacement-defmacro)