(in-package "LECHO")

;;;; Module ECHO

;;; Copyright (c) 1999 Jim Allard
;;; All rights reserved.

;;; Jim Allard






;;;; Echoing Arguments




;;; The `lecho' system is a simple, Lisp-based echo program that is primarily
;;; here to demonstrate the typical use of declare-system, package creation, and
;;; main functions with the GL translator.

;;; Typically, your systems should have a main function that is in the
;;; GL-USER package so that there are no problems mentioning it in the
;;; BOOT module, which must both create packages and name the main
;;; function.

;;; The main function that is declared in your declare-system form
;;; will be called with one argument, which is a list of strings which
;;; are the arguments to this execution of the program.  This
;;; corresponds to the argv argument of a C main function.  Main
;;; should return an integer: zero to indicate normal execution and
;;; completion, and other values to indicate failure modes.  If the
;;; operation is at all non-trivial, you should look for an argument
;;; of "--help" or "?" and print out a usage banner, then exit.

(defun gl-user::main (args)
  (when (and args (string= (car args) "--help"))
    (format t "Usage: lecho [arg] ...~%  all arguments will be echoed back to stdout with a space between each")
    (return-from gl-user::main -1))
  (loop for arg-cons on args do
    (when (not (eq arg-cons args))
      (write-char " "))
    (write-string (cons-car arg)))
  0)
