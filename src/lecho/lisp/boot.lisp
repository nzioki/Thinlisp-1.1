(in-package "GL-USER")

;;;; Module BOOT

;;; Copyright (c) 1999 Jim Allard
;;; All rights reserved.

;;; Jim Allard






;;;; Bootstrapping Lecho




;;; The `lecho' system is a simple, Lisp-based echo program that is primarily
;;; here to demonstrate the typical use of declare-system, package creation, and
;;; main functions with the GL translator.

;;; Typically, your systems should have a boot.lisp file in this position.  If
;;; GL is attempting to find information about a system and currently has no
;;; information about it, it will load a filename of the form
;;; <sys-name>/lisp/boot.lisp.  If that doesn't exist or doesn't contain the
;;; desired declare-system, then it will attempt to load
;;; lisp/<sys-name>-boot.lisp.

;;; Typically this file should be in the GL-USER package, and you can create any
;;; further packages you might want here.  For example's sake, I'm going to make
;;; a LECHO package that will be used for the remaining file(s) in this system.

(unless (find-package "LECHO")
  (make-package "LECHO"))

(declare-system (lecho :main-function gl-user::main)
  boot
  echo)
