(in-package "GL")

;;;; Module BOOT

;;; Copyright (c) 1999 The ThinLisp Group
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






;;;; Bootstrapping GL




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
