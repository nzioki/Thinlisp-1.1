(in-package "TLI")

;;;; Module DEFSTRUCT

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






;;;; Defstruct




;;; This module implements the `tl:defstruct' macro.  During Lisp development
;;; time it expands into the underlying Lisp's defstruct.  Translated, each
;;; structure type has a unique type number.  These numbers are produced via a
;;; depth first walk of the structure type hierarchy.  Subtype checks are
;;; preformed by determining that the object is a structure, and that the
;;; structure type number is greater than or equal to the structure number, and
;;; less than or equal to the highest numbered type in the type subtree below
;;; this structure.  In translation, the type slot of the Hdr structure will
;;; contain the type number for all structures, and the fill slot will contain
;;; the unique number for this type.

(defmacro tl:defstruct (name-and-options &rest doc-and-slot-descriptions)
  (let ((name (if (consp name-and-options) 
		  (cons-car name-and-options)
		name-and-options))
	(options (if (consp name-and-options)
		     (cdr name-and-options)
		   nil))
	(doc (if (stringp (car doc-and-slot-descriptions))
		 (pop doc-and-slot-descriptions)
	       nil))
	(slot-descriptions 
	 (loop for slot in doc-and-slot-descriptions
	       collect
	       (if (symbolp slot)
		   (list slot nil)
		 slot)))
	())
    nil))
