(in-package "GLI")

;;;; Module MAKEFILES

;;; Copyright (c) 1999 Jim Allard
;;; All rights reserved.

;;; Jim Allard






;;;; Makefile Generation




;;; This module implements a simple makefile generator to bootstrap compilation
;;; of GL translations.

;;; The variable `makefile-element-alist' is an alist of symbols naming
;;; variables to be bound in a makefile, associated with the strings that should
;;; be used to initialize those variables.  The default settings here were made
;;; for compiling using the Cygnus tools on Windows with the environment
;;; variable MAKE_MODE set to UNIX.

(defparameter makefile-element-alist
    '((cc          . "gcc -o")
      (cc-flags    . "-O2 -ansi -pedantic -W -Wall -fomit-frame-pointer -c")
      (debug-flags . "-ggdb3 -ansi -pedantic -W -Wall -c")
      (wild        . "%")
      (stem        . "$*")
      (target      . "$@")
      (first-dep   . "$<")
      
      (archive     . "ar rsc")
      (link        . "gcc -o")
      (link-flags  . "-O2")
      (debug-link  . "-g")
      (system-libs . "")

      (lib-prefix  . "lib")
      (lib-postfix . ".a")
      (exe-prefix  . "")
      (exe-postfix . ".exe")
      (obj-postfix . ".o")
      ))

(defun makeup (key)
  (or (cdr (assoc key makefile-element-alist))
      ""))

(defparameter debuggable-makefile t)




;;; The function generate-makefile takes a system, and uses information from the
;;; system and from the makefile-info file to generate a GNU compatible
;;; makefile.

(defun generate-makefile (system verbose)
  (let ((path (system-makefile system))
	(temp-path (system-temporary-makefile system))
	(bin-dir (system-bin-dir system))
	(current-year
	 (sixth (multiple-value-list
		 (decode-universal-time (get-universal-time)))))
	(target (if (system-is-library-p system)
		    (format nil "~a~(~a~)~a"
			    (makeup 'lib-prefix)
			    (system-name system)
			    (makeup 'lib-postfix))
		  (format nil "~a~(~a~)~a"
			  (makeup 'exe-prefix)
			  (system-name system)
			  (makeup 'exe-postfix))))
	(obj (makeup 'obj-postfix))
	(pattern (makeup 'wild))
	(files-per-line 7))
    (with-open-file (output temp-path :direction :output :if-exists :supersede)
      (format output "#~%# ~a Makefile~%#~%# Copyright (c) ~a Jim Allard~%~%"
	      (system-name system) current-year)
      
      (format output "CC=~a~%" (makeup 'cc))
      (format output "CFLAGS=~a~%" 
	      (makeup (if debuggable-makefile 'debug-flags 'cc-flags)))
      (cond ((system-is-library-p system)
	     (format output "ARCHIVE=~a~%" (makeup 'archive)))
	    (t
	     (format output "LINK=~a~%" (makeup 'link))
	     (format output "LINKFLAGS=~a~%" 
		     (makeup (if debuggable-makefile 'debug-link 'link-flags)))
	     (format output "LIBS=")
	     (loop for subsystem in (butlast (system-all-used-systems system)) do
	       (glt-write-char #\space output)
	       (relative-path-to-directory
		bin-dir (system-bin-dir (gl:find-system subsystem))
		output)
	       (format output "lib~(~a~).a" subsystem))
	     (format output " ~a~%" (makeup 'system-libs))))
      (glt-write-string "OBJECTS=" output)
      (loop for file-count = 1 then (1+ file-count)
	  for file-name in (system-extra-c-files system) do
	(when (= (mod file-count files-per-line) 0)
	  (format output " \\~%       "))
	(format output " ~a~a" file-name obj))
      (loop with file-count = (length (system-extra-c-files system))
	  for module in (system-modules system)
	  do
	(when (system-module-included-p system module)
	  (incf file-count)
	  (when (= (mod file-count files-per-line) 0)
	    (format output " \\~%       "))
	  (format output " ~a~a" (module-file-name-string module) obj)))
            
      (format output "~%~%all : ~a~%~%clean :~%" target)
      (glt-write-char #\tab output)
      (format output "-rm *~a~%" obj)
      (glt-write-char #\tab output)
      (format output "-rm ~a~%~%" target)
      
      (format output "~a : makefile $(OBJECTS) $(LIBS)~%" target)
      (glt-write-char #\tab output)
      (format output "-rm ~a~%" target)
      (glt-write-char #\tab output)
      (if (system-is-library-p system)
	  (format output "$(ARCHIVE) ~a $(OBJECTS)~%~%" target)
	(format output "$(LINK) ~a $(LINKFLAGS) $(OBJECTS) $(LIBS)~%~%"
		target))
      
      (format output "~a~a : ../c/~a.c ../c/~a.h makefile"
	      pattern obj pattern pattern)
      (loop for file in (system-extra-h-files system) do
	(format output " ../c/~a.h" file))
      (glt-write-char #\newline output)
      (glt-write-char #\tab output)
      (format output "$(CC) ~a $(CFLAGS) -I ../c -I "
	      (makeup 'target))
      (relative-path-to-directory
       bin-dir (system-c-dir (gl:find-system 'gl)) output)
      (format output " ~a~%" (makeup 'first-dep)))
    
    ;; If the newly created makefile is different from the existing one,
    ;; overwrite the existing one with the newly created temporary.
    (when (or (not (probe-file path))
	      (not (file-contents-equal temp-path path)))
      (when verbose
	(format t "~%Installing new C dir makefile for ~a" (system-name system)))
      (with-open-file (input temp-path)
	(with-open-file (output path :direction 
			 :output :if-exists :supersede)
	  (loop for line = (read-line input nil :eof)
	      until (eq line :eof) do
	    (write-line line output)))))
    (delete-file temp-path)
    
    ;; If the makefile is different from the one in the binary directory, push
    ;; it into the binary directory.
    (let ((binary-makefile (system-binary-makefile system)))
      (ensure-directories-exist binary-makefile)
      (when (or (not (probe-file binary-makefile))
		(not (file-contents-equal path binary-makefile)))
	(when verbose
	  (format t "~%Installing new bin dir makefile for ~a" (system-name system)))
	(with-open-file (input path)
	  (with-open-file (output binary-makefile 
			   :direction :output :if-exists :supersede)
	    (loop for line = (read-line input nil :eof)
		until (eq line :eof)
		do
	      (write-line line output))))))))
