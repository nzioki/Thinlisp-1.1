(in-package "GLI")

;;;; Module SYSTEM

;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Declaring Systems




;;; The `gl:system' stucture is used to store the characteristics of a system.
;;; This structure is stored on the :system property of the system name.

(defstruct (system)
  name
  nicknames
  used-systems
  is-library-p
  lisp-dir
  c-dir
  extra-c-files
  extra-h-files
  main-function
  modules
  module-properties-alist
  alias
  properties)




;;; The variable `current-systems' contains a list of the systems that have been
;;; (or are currently being) loaded into this Lisp environment.

(defvar current-systems nil)

(defun normalize-system-name (name)
  (if (eq (symbol-package name) *gl-user-package*)
      name
      (intern (symbol-name name) *gl-user-package*)))

(defun normalize-module-name (name)
  (normalize-system-name name))

(defvar gl:current-system-being-loaded nil)

(defvar gl:all-systems nil)




;;; The macro `gl:find-system' takes a symbol and returns the system structure,
;;; if any, that corresponds to that system.

(defun gl:find-system (system-name)
  (setq system-name (normalize-system-name system-name))
  (or (get system-name :system)
      (let ((standard-boot-name
	      (make-pathname
		:directory
		(list :relative
		      (string-downcase (symbol-name system-name))
		      "lisp")
		:name "boot"
		:type lisp-file-type)))
	(or (and (probe-file standard-boot-name)
		 (load standard-boot-name)
		 (get system-name :system))
	    (let ((backup-standard-boot-name
		   (make-pathname
		     :directory (list :relative "lisp")
		     :name (format nil "~(~a~)-boot" system-name)
		     :type lisp-file-type)))
	      (or (and (probe-file backup-standard-boot-name)
		       (load backup-standard-boot-name)
		       (get system-name :system))
		  (error
		    "No system ~a exists, or no file ~a exist, or if it did ~
		     it did not define ~a."
		    system-name standard-boot-name system-name)))))))

(defsetf gl:find-system set-find-system)

(defun set-find-system (system-name system-structure)
  (setf (get (normalize-system-name system-name) :system)
	system-structure))




;;; The macro `gl:declare-system' is used to define sets of files as a
;;; compilable unit.  Systems have a name, are compiled either as a library or
;;; an executable, contain a set of used systems, and contain a set of Lisp
;;; files.  Systems are presumed to lie within a subdirectory of a sandbox that
;;; has the same name as the system, i.e. a directory of the form
;;; "/gensym/bt/<sandbox-name>/<system-name>".  Within this directory, there
;;; should be lisp, c, opt, o, and o-pg directories.  These default directories
;;; can be overridden using the :lisp-dir options to change the Lisp directory,
;;; and the :c-dir option to change the location of the c, opt, o, and o-pg
;;; directories.  The overrides should be of the form of relative directory
;;; names within a sandbox, but if the first character of these directories are
;;; either forward or backward slash, then they are taken as absolute directory
;;; names.

;;; If there are extra, hand-written C files included in the C directory for a
;;; system, include them in the :extra-c-files argument, as a list of file name
;;; strings.

;;; If there are extra, hand-written H files included in the C directory that
;;; are needed to implement def-inlined-pseudo-functions, then include them in
;;; the :extra-h-files argument as a list of file name strings.  These files
;;; will be included into the translated C files.

;;; The syntax of a declare-system is as follows:

;;;   (declare-system (<name> [keyword options and arguments])
;;;     module-spec ...)

;;; where module-spec is either a symbol or a list of a symbol naming a module
;;; and then alternating keyword properties and values.  The only supported
;;; property now is :include-test.  The value for include test is feature form
;;; acceptible to eval-feature, which is tested to check if this module should
;;; be included in this system given the current feature settings.

;;; For example, the current GSI declaration might be

;;;   (declare-system (gsi :lisp-dir "lisp/" :c-dir "gsi/c/")
;;;     load
;;;     bootstrap
;;;     delta
;;;     systems ...)

;;; A more bland example might be for a simple Lisp-based echo command.  

;;;   (declare-system (lecho)
;;;     boot
;;;     echo)

;;; This declaration would expect to find a "lecho" directory as a sibling to
;;; glt and gl.  Within lecho, there should be lisp, c, opt, o, and o-pg
;;; subdirectories.  In the Lisp directory there should be files boot.lisp and
;;; echo.lisp.  Inside of boot.lisp, it should default to the package "GL-USER",
;;; there should be the following declare-system form, and any needed packages
;;; should be created.  Note that the symbol naming the main function must have
;;; a package matching what will be found in the body of the system.  In the
;;; second file, echo.lisp, there should be an in-package to the lecho package,
;;; and the function gl-user::main (or some such name) that takes a single
;;; argument, which is the list of given args.

(defmacro gl:declare-system ((name &key
				   (library nil)
				   (main-function nil)
				   (used-systems '(gl))
				   (nicknames nil)
				   (lisp-dir nil)
				   (c-dir nil)
				   (extra-c-files nil)
				   (extra-h-files nil)
				   (alias nil)
				   (properties nil))
			     &rest modules)
  
  (setq name (normalize-system-name name))
  (unless (or library main-function)
    (warn "System ~a must be declared a library or have a main function."
	  name))
  (unless library
    (setq extra-c-files (append extra-c-files '("main"))))
  (setq nicknames
	(loop for nickname in nicknames
	      collect (normalize-system-name nickname)))
  (setq used-systems
	(loop for used in used-systems
	      collect (normalize-system-name used)))
  (when alias
    (setq alias (normalize-system-name alias)))
  (setq modules (copy-tree modules))
  (loop for module-cons on modules
	for mod = (cons-car module-cons)
	do
    (cond
      ((consp mod)
       (setf (car mod) (normalize-module-name (car mod)))
       (loop for prop-cons on (cons-cdr mod) by #'cddr do
	 (unless (and (consp prop-cons)
		      (consp (cons-cdr prop-cons))
		      ;; Add more clauses to this cond as further features are
		      ;; supported.
		      (cond
			((eq (cons-car prop-cons) :include-test)
			 (or (and (consp (cons-cdr prop-cons))
				  (null (cons-cddr prop-cons))
				  (well-formed-eval-feature-clause
				    (cons-cadr prop-cons)))
			     (progn
			       (warn "Bad :include-test in module ~s, must ~
                                              be logical combinations of keywords. ~s"
				     mod (second prop-cons))
			       nil)))))
	   (warn "Unsupported property ~s in module def ~s."
		 (car prop-cons) mod))))
      ((symbolp mod)
       (setf (car module-cons)
	     (normalize-module-name mod)))
      (t
       (error "Bad module format, wasn't a symbol or list of symbol and ~
                 properties: ~s"
	      mod))))
  (let ((defined-get (if (eval-feature :translator) 'gl:get 'get)))
    `(gl:progn
       ,@(unless (eval-feature :translator)
	   `((gl:setf (gl:find-system ',name)
		      (make-new-system
			',name ',nicknames ,library ',main-function
			',used-systems ,lisp-dir ,c-dir ',extra-c-files
			',extra-h-files ',modules ',alias ',properties))))
       ,@(unless (eq name 'gl:debug)
	   `((gl:setq gl:current-system-being-loaded ',name)
	     (gl:setq gl:all-systems (gl:cons ',name gl:all-systems))
	     ))
       (gl:setf (,defined-get ',name :system-nicknames)
		',nicknames)
       ,@(loop for nickname in nicknames collect
	       `(gl:setf (,defined-get ',nickname :nicknames-to)
			 ',name))
       (gl:setf (,defined-get ',name :system-used-systems)
		',used-systems)
       (gl:setf (,defined-get ',name :system-modules)
		',(loop for module in modules
			collect (if (consp module)
				    (car module)
				    module)))
       ,@(when alias
	   `((gl:setf (,defined-get ',name :alias) ',alias)))
       ',name)))

(defun make-new-system
    (name nicknames library main-function used-systems lisp-dir c-dir
	  extra-c-files extra-h-files modules alias properties)
  (make-system
    :name name
    :nicknames nicknames
    :is-library-p library
    :main-function main-function
    :used-systems used-systems
    :lisp-dir (if lisp-dir
		  (pathname lisp-dir)
		  (make-pathname
		    :directory
		    (list :relative
			  (string-downcase (symbol-name name))
			  "lisp")))
    :c-dir (if c-dir
	       (pathname c-dir)
	       (make-pathname
		 :directory
		 (list :relative
		       (string-downcase (symbol-name name))
		       "c")))
    :extra-c-files extra-c-files
    :extra-h-files extra-h-files
    :modules (loop for mod in modules
		   collect (if (consp mod)
			       (cons-car mod)
			       mod))
    :module-properties-alist (loop for mod in modules
				   when (consp mod)
				     collect mod)
    :alias alias
    :properties properties))





;;; The function `system-lisp-file' takes a system and a symbol naming a Lisp
;;; module.  This module returns a pathname to the Lisp file for that module.
;;; Note that the names of modules are always downcased.  The functions
;;; `system-lisp-binary-file', `system-c-file', `system-h-file', and
;;; `system-trans-data-file' perform similar functions for their various types
;;; of files.  The function `system-lisp-relative-binary-file' returns a
;;; pathname to the Lisp binary file that is relative to the Lisp file, for
;;; those systems that require this as the :output-file argument to
;;; compile-file (i.e. Lucid).  For all other systems, this returns exactly
;;; what system-lisp-binary-file returns.

(defun module-file-name-string (module-name-symbol)
  (string-downcase (symbol-name module-name-symbol)))

(defun make-system-file-pathname
    (module-name-symbol module-type module-dirs defaults)
  (merge-pathnames 
    (make-pathname
      :name (module-file-name-string module-name-symbol)
      :type module-type
      :directory module-dirs)
    defaults))

(defun system-lisp-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol lisp-file-type nil (system-lisp-dir system)))

(defun system-lisp-binary-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol
    lisp-binary-file-type
    (list
      :relative
      (if (eval-feature :development)
	  lisp-dev-binary-directory-name
	  lisp-macro-binary-directory-name))
    (system-lisp-dir system)))

(defun system-lisp-relative-binary-file (system module-name-symbol)
  #+lucid
  (declare (ignore system))
  #+lucid
  (make-pathname
    :name (string-downcase (symbol-name module-name-symbol))
    :type lisp-binary-file-type
    :directory (list :relative
		     (if (eval-feature :development)
			 lisp-dev-binary-directory-name
		       lisp-macro-binary-directory-name)))
  #-lucid
  (system-lisp-binary-file system module-name-symbol))

(defun system-trans-data-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol trans-data-file-type nil (system-c-dir system)))

(defun system-temporary-trans-data-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol temporary-trans-data-file-type nil (system-c-dir system)))

(defun system-c-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol c-file-type nil (system-c-dir system)))

(defun system-temporary-c-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol temporary-c-file-type  nil (system-c-dir system)))

(defun system-h-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol h-file-type nil (system-c-dir system)))

(defun system-temporary-h-file (system module-name-symbol)
  (make-system-file-pathname
    module-name-symbol temporary-h-file-type nil (system-c-dir system)))

(defun system-h-file-name (system module-name-symbol)
  (declare (ignore system))
  (make-pathname :name (module-file-name-string module-name-symbol)
		 :type h-file-type))

(defun system-makefile-info-file (system)
  (make-system-file-pathname
    (intern (format nil "~a-FILES" (system-name system)))
    "txt" nil (system-c-dir system)))

(defun system-makefile (system &optional port-name)
  (make-system-file-pathname
   (if port-name 
       (intern (format nil "makefile~a~a"
		       (if (string= port-name "config")
			   "."
			 "-")
		       port-name))
     'makefile)
   nil nil (system-c-dir system)))

(defun system-temporary-makefile (system)
  (make-system-file-pathname
   'maketemp "txt" nil (system-c-dir system)))

(defun system-bin-dir (system)
  (let ((c-dir (system-c-dir system)))
    (make-pathname 
     :directory (append (butlast (pathname-directory c-dir)) '("bin"))
     :defaults c-dir)))

(defun system-binary-makefile (system &optional port-name)
  (make-system-file-pathname
   (if port-name 
       (intern (format nil "makefile-~a" port-name))
     'makefile)
   nil nil (system-bin-dir system)))


(defun relative-path-to-directory (start-pathname end-pathname output)
  (let* ((source-directory (pathname-directory start-pathname))
	 (target-directory (pathname-directory end-pathname))
	 (new-path
	  (cond 
	   ((eq (car target-directory) :absolute)
	    target-directory)
	   ((eq (car source-directory) :absolute)
	    (translation-error "Can't make a path to ~a from ~a"
			       start-pathname end-pathname))
	   ((not (and (eq (car source-directory) :relative)
		      (eq (car target-directory) :relative)))
	    (translation-error
	     "Pathnames weren't :absolute or :relative.  Huh?: ~a ~a"
	     start-pathname end-pathname))
	   (t
	    (loop for source = source-directory then (cdr source)
		for target = target-directory then (cdr target)
		while (and source target 
			   (equalp (car source) (car target)))
		finally
		  (return 
		    (append '(:relative)
			    (loop repeat (length source) collect :up)
			    target)))))))
    (dolist (dir new-path)
      (cond ((eq dir :relative)
	     ;; do nothing
	     nil)
	    ((eq dir :absolute)
	     (glt-write-char #\/ output))
	    ((eq dir :up)
	     (glt-write-string "../" output))
	    (t
	     (format output "~a/" dir))))))




;;; The function `write-system-lisp-file-names' takes a system name and writes
;;; to standard output the names of all Lisp modules in that system.  It is
;;; intended that this will be redirected into a file and then used for grep,
;;; tags, and make-id calls to get the set of all Lisp modules, in order.

(defun write-system-lisp-file-names (system-name)
  (loop for module in (system-modules (gl:find-system system-name)) do
    (format t "~a.~a~%" (module-file-name-string module) lisp-file-type)))




;;; The function `loaded-system-lisp-binary-write-date' takes a system and a
;;; module and returns the file write date of this file when last loaded, or NIL
;;; if the file has not ever been loaded.  This operation is setf'able.  The
;;; function `loaded-system-trans-data-write-date' performs the same operation
;;; for the translation data file.

(defun loaded-system-file-write-date (system module property)
  (cdr (assq (system-name system) (get module property))))

(defun set-loaded-system-file-write-date (system module property write-date)
  (let* ((module-write-date-alist (get module property))
	 (existing-alist-entry?
	   (assq (system-name system) module-write-date-alist)))
    (if existing-alist-entry?
	(setf (cdr existing-alist-entry?) write-date)
	(setf (get module property)
	      (cons (cons (system-name system) write-date)
		    module-write-date-alist)))
    write-date))

(defun loaded-system-lisp-binary-write-date (system module)
  (loaded-system-file-write-date
    system module :system-lisp-binary-write-date-alist))

(defun set-loaded-system-lisp-binary-write-date (system module write-date)
  (set-loaded-system-file-write-date
    system module :system-lisp-binary-write-date-alist write-date))

(defsetf loaded-system-lisp-binary-write-date
    set-loaded-system-lisp-binary-write-date)

(defun loaded-system-trans-data-write-date (system module)
  (loaded-system-file-write-date
    system module :system-trans-data-write-date-alist))

(defun set-loaded-system-trans-data-write-date (system module write-date)
  (set-loaded-system-file-write-date
    system module :system-trans-data-write-date-alist write-date))

(defsetf loaded-system-trans-data-write-date
    set-loaded-system-trans-data-write-date)




;;; The function `system-module-included-p' checks whether or not there are any
;;; conditions placed on the inclusion of of the given module in the given
;;; system, and if so whether or not the current set of features allows
;;; inclusion of that module.

(defun system-module-included-p (system module)
  (let* ((module-properties
	   (cdr (assq module (system-module-properties-alist system))))
	 (inclusion-features?
	   (getf module-properties :include-test)))
    (or (null inclusion-features?)
	(eval-feature inclusion-features?))))




;;; The function `system-all-used-systems' returns the linearized list of
;;; system-names of the given system's used-systems, merged with their used
;;; systems, etc.  Since several of the systems used by this system might use
;;; the same subsystems, you should iterate over this list instead of
;;; recursively iterating the directly used systems.  This avoids duplication.

(defvar systems-so-far nil)

(defun system-all-used-systems (system)
  (let ((systems-so-far nil))
    (collect-subsystems
      (if (symbolp system) (gl:find-system system) system))
    (nreverse systems-so-far)))

(defun collect-subsystems (system)
  (loop for subsystem-name in (system-used-systems system)
	for subsystem = (gl:find-system subsystem-name)
	do
    (collect-subsystems subsystem))
  (pushnew (system-name system) systems-so-far))




;;; The function `gl:load-system' is used to load Lisp binaries for a given
;;; system.  It will also load the corresponding files from its used systems.
;;; The :verbose keyword argument that controls whether or not information will
;;; be printed to standard output as the process continues.  It defaults to T.
;;; The :print keyword argument controls whether it attempts to cause the
;;; underlying implementation to print something per form in the files being
;;; loaded.  The :to keyword argument defaults to NIL, or takes a symbol naming
;;; a module in this system.  If :to is provided, load-system will stop after
;;; processing the named file.

(defun gl:load-system (system &key (verbose t) (print nil) (to nil))
  (let* ((system-name (if (symbolp system)
			  (normalize-system-name system)
			  (system-name system)))
	 (system-struct (gl:find-system system-name))
	 (subsystem-names (system-all-used-systems system-struct)))
    (loop for subsystem-name in subsystem-names do
      (load-system-1
	subsystem-name verbose print
	(if (eq subsystem-name system-name) to nil)))))

(defun load-system-1 (system verbose print to)
  (when (symbolp system)
    (setq system (gl:find-system system)))
  (pushnew (system-name system) current-systems)
  (loop for nickname in (system-nicknames system) do
    (pushnew nickname current-systems))
  (when to
    (setq to (normalize-module-name to)))
  (when verbose
    (format t "~%Loading System ~a" (system-name system))
    (force-output))
  (loop with *package* = *package*
	with modules
	  = (loop for module in (system-modules system)
		  when (system-module-included-p system module)
		    collect module)
	with total-modules = (length modules)
	for module-count from 1
	for module in modules
	for binary-file = (system-lisp-binary-file system module)
	for write-date = (file-write-date binary-file)
	for loaded-date? = (loaded-system-lisp-binary-write-date system module)
	do
    (unless (eql loaded-date? write-date)
      (when verbose
	(format t "~%Loading ~14a      [~3d/~3d] "
		module module-count total-modules)
	(force-output))
      (load binary-file :verbose print)
      (setf (loaded-system-lisp-binary-write-date system module) write-date))
	until (eq module to)))




;;; The function `gl:compile-system' is used to compile and load Lisp binaries
;;; for a given system.  By default it will also compile the corresponding files
;;; from its used systems.  Keyword arguments are as follows:

;;;   :recompile defaults to NIL and controls whether all files are forced to
;;;   recompile instead of recompiling only when the binary file is out of date.
;;;   This switch is controlled by the :from switch.

;;;   :from defaults to NIL.  When given a symbol naming a module in this
;;;   system, it controls where a recompile starts from (keeping all files in
;;;   the system prior to the named file).

;;;   :to defaults to NIL.  When given a symbol naming a module in this system,
;;;   it stops compilation after having processed the named module.

;;;   :verbose controls whether or not information will be printed to standard
;;;   output as the process continues.  It defaults to T.

;;;   :print controls whether it attempts to cause the underlying
;;;   implementation to print someting per form in the files being compiled and
;;;   loaded.

;;;   :compile-used-systems defaults to T and controls whether or not the used
;;;   systems are processed with compile-system or load-system.

;;;   :recompile-used-systems defaults to NIL and controls whether or not the
;;;   used systems are recompiled.  When you supply T to this argument, you do
;;;   not need to supply a value for :compile-used-systems.

(defun gl:compile-system
    (system &key (verbose t) (recompile nil) (from nil) (to nil)
	    (compile-used-systems t) (recompile-used-systems nil)
	    (print nil))
  (let* ((system-name (if (symbolp system)
			  (normalize-system-name system)
			  (system-name system)))
	 (system-struct (gl:find-system system-name))
	 (used-system-names (system-all-used-systems system-struct)))
    (when from
      (setq from (normalize-module-name from)))
    (when to
      (setq to (normalize-module-name to)))
    (loop for used-system-name in used-system-names do
      (with-compilation-unit ()
	(if (eq used-system-name system-name)
	    (compile-system-1
	     used-system-name :verbose verbose :print print
	     :recompile recompile :from from :to to)
	  (if (or compile-used-systems recompile-used-systems)
	      (compile-system-1
	       used-system-name :verbose verbose :print print
	       :recompile recompile-used-systems)
	    (load-system-1
	     used-system-name verbose print nil)))))))

(defun compile-system-1
    (system &key (verbose t) (print nil) (recompile nil) (from nil) (to nil))
  (when (symbolp system)
    (setq system (gl:find-system system)))
  (pushnew (system-name system) current-systems)
  (loop for nickname in (system-nicknames system) do
    (pushnew nickname current-systems))
  (when recompile
    (when verbose
      (format t "~%Deleting binaries for System ~a" (system-name system))
      (when from (format t " from module ~a" from)))
    (with-faster-standard-output 
      (loop for module in (system-modules system)
	    for binary-file = (system-lisp-binary-file system module)
	    do
	(when (or (null from) (eq from module))
	  (setq from nil)
	  (when (probe-file binary-file)
	    (when verbose
	      (format t "~%Deleting ~a" binary-file))
	    (delete-file binary-file)))
	    until (eq module to)
	    finally
	      (when from
		(format t "~%Warning: Module ~a not found in ~a"
			from (system-name system))))))
  (when verbose
    (format t "~%Compiling and Loading System ~a" (system-name system))
    (force-output))
  (loop with *package* = *package*
	with development? = (eval-feature :development)
	with modules
	  = (loop for mod in (system-modules system)
		  when (system-module-included-p system mod)
		    collect mod)
	with total-modules = (length modules)
	for module-count from 1
	for module in modules
	do
    (let* ((lisp-file (system-lisp-file system module))
	   (relative-binary-file
	     (system-lisp-relative-binary-file system module))
	   (binary-file (system-lisp-binary-file system module))
	   (lisp-write-date (file-write-date lisp-file))
	   (binary-write-date? (file-write-date binary-file))
	   (loaded-date? (loaded-system-lisp-binary-write-date system module))
	   (*current-system-name* (system-name system))
	   (*current-module-name* module))
      (when (and lisp-write-date
		 (or (null binary-write-date?)
		     (<= binary-write-date? lisp-write-date)
		     (and user::exports-file-write-date
			  (<= binary-write-date? 
			      user::exports-file-write-date))))
	(when verbose
	  (format t "~%Compiling ~14a    [~3d/~3d] ~a"
		  module module-count total-modules
		  (if development? " (development)" ""))
	  (force-output))
	(compile-file lisp-file
		      :output-file relative-binary-file
		      #-lucid :verbose #-lucid print
		      #-lucid :print #-lucid print)
	(setq binary-write-date? (file-write-date binary-file)))
      (unless (eql loaded-date? binary-write-date?)
	(when verbose
	  (format t "~%Loading ~14a      [~3d/~3d] ~a"
		  module module-count total-modules
		  (if development? " (development)" ""))
	  (force-output))
	(load binary-file :verbose print :print print)
	(setf (loaded-system-lisp-binary-write-date system module)
	  binary-write-date?)))
    (gc-a-little)
	until (eq module to)))






;;;; Convenience Forms




;;; The following macro emits macros of the form load-g2, compile-g2, and
;;; translate-g2 for all of the systems listed below.  These expand into calls
;;; to gl:load-system, gl:compile-system, and gl:translate-system and are here
;;; purely for convenience.

(defmacro def-system-convenience-forms (&rest system-names)
  (cons
    'progn
    (loop for system in system-names
	  for load-name = (intern (format nil "LOAD-~a" system) *gl-package*)
	  for cl-load-name = (intern (symbol-name load-name) *cl-user-package*)
	  for compile-name = (intern (format nil "COMPILE-~a" system)
				     *gl-package*)
	  for cl-compile-name = (intern (symbol-name compile-name) *cl-user-package*)
	  for translate-name = (intern (format nil "TRANSLATE-~a" system)
				       *gl-package*)
	  for cl-translate-name = (intern (symbol-name translate-name) *cl-user-package*)
	  appending
	  `((make-convenince-names-visible '(,(symbol-name load-name)
					     ,(symbol-name compile-name)
					     ,(symbol-name translate-name)))
	    (defmacro ,load-name (&key (verbose t) (print nil) (to nil))
	      `(gl:load-system ',',system
			       :verbose ,verbose :print ,print :to ,to))
	    (defmacro ,cl-load-name (&key (verbose t) (print nil) (to nil))
	      `(gl:load-system ',',system
			       :verbose ,verbose :print ,print :to ,to))
	    (defmacro ,compile-name
		(&key (verbose t) (recompile nil) (from nil) (to nil)
		      (compile-used-systems t) (recompile-used-systems nil)
		      (print nil))
	      `(gl:compile-system
		 ',',system
		 :verbose ,verbose :recompile ,recompile :from ,from :to ,to
		 :compile-used-systems ,compile-used-systems
		 :recompile-used-systems ,recompile-used-systems
		 :print ,print))
	    (defmacro ,cl-compile-name
		(&key (verbose t) (recompile nil) (from nil) (to nil)
		      (compile-used-systems t) (recompile-used-systems nil)
		      (print nil))
	      `(gl:compile-system
		 ',',system
		 :verbose ,verbose :recompile ,recompile :from ,from :to ,to
		 :compile-used-systems ,compile-used-systems
		 :recompile-used-systems ,recompile-used-systems
		 :print ,print))
	    (defmacro ,translate-name
		(&key (verbose t)
		      (recompile nil) (from nil) (to nil)
		      (compile-used-systems t)
		      (recompile-used-systems nil)
		      (retranslate-used-systems nil)
		      (retranslate nil)
		      (print nil))
	      `(gl:translate-system
		 ',',system
		 :verbose ,verbose :recompile ,recompile :from ,from :to ,to
		 :compile-used-systems ,compile-used-systems
		 :recompile-used-systems ,recompile-used-systems
		 :retranslate-used-systems ,retranslate-used-systems
		 :retranslate ,retranslate
		 :print ,print))
	    (defmacro ,cl-translate-name
		(&key (verbose t)
		      (recompile nil) (from nil) (to nil)
		      (compile-used-systems t)
		      (recompile-used-systems nil)
		      (retranslate-used-systems nil)
		      (retranslate nil)
		      (print nil))
	      `(gl:translate-system
		 ',',system
		 :verbose ,verbose :recompile ,recompile :from ,from :to ,to
		 :compile-used-systems ,compile-used-systems
		 :recompile-used-systems ,recompile-used-systems
		 :retranslate-used-systems ,retranslate-used-systems
		 :retranslate ,retranslate
		 :print ,print))))))

(defun make-convenince-names-visible (name-strings)
  (let ((symbols (loop for name in name-strings
		       collect (intern name *gl-package*))))
    (export symbols *gl-package*)))

(def-system-convenience-forms gl lecho)
