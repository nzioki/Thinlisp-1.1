(in-package "GL")

;;;; Module BOOT

;;; Copyright (c) 1996 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Bootstrapping GL




;;; The following global variables are modified by the expansion of
;;; declare-system, so they must be defined before it.

(defvar current-system-being-loaded nil)
    
(defvar all-systems nil)




;;; Loading this module will define the GL system, which implements the
;;; translated primitives in GL, the Gensym Language.  GL stands upon GLT, the
;;; Gensym Language Translator, which should already have been loaded when this
;;; file is loaded.

(declare-system (gl :library t :used-systems nil
		    :extra-c-files ("glt" "notify")
		    :extra-h-files ("glt"))
  boot
  stubs
  gl-types
  inline
  gl-prim
  do
  format
  input
  gl-basics
  loop
  apply
  generic-math
  generic-prim
  packages
  gl-util
  versions
  forward
  glbasics)
