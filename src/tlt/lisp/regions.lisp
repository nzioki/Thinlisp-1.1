(in-package "GLI")

;;;; Module REGIONS

;;; Copyright (c) 1996 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard






;;;; Memory Regions




;;; This module contains operations for specifying and manipulating memory
;;; regions within GL applications.

;;; The function `region-number-for-type-and-area' takes a Lisp type and an area
;;; name, one of either gl:permannent or gl:temporary.  This function returns an
;;; integer that is the region number for creating the given type in the given
;;; area.

(defun region-number-for-type-and-area (lisp-type area)
  (cond ((eq area 'gl:temporary)
	 (unless (eq lisp-type 'double-float)
	   (translation-warning
	     "Attempting to allocate a ~s in a temporary area.  Only floats can go there."
	     lisp-type))
	 2)
	((eq area 'gl:permanent)
	 (if (eq lisp-type 'symbol)
	     1
	     0))
	(t
	 (translation-error "Bad area name ~s." area))))




;;; The function `declared-area-name' takes an environment and a Lisp type and
;;; returns the symbol naming the current area declared within the environment.
;;; If none is visible, this function issues a translation warning and returns
;;; gl:permanent.

(defun declared-area-name (env type-to-allocate)
  (let ((area? (gl:declaration-information 'gl:consing-area env)))
    (unless area?
      (translation-warning
	"Consing a ~s with no surrounding consing-area declaration."
	type-to-allocate)
      (setq area? 'gl:permanent))
    area?))




;;; The macros `with-temporary-area' and `with-permanent-area' are implemented
;;; by rebinding the variables Current-region and Temporary-area-top.  These
;;; variables are defined in GL.

(def-gl-macro gl:with-temporary-area (&body forms)
  `(gl:let* ((current-region
	       ,(region-number-for-type-and-area 'double-float 'gl:temporary))
	     (temporary-area-top temporary-area-top))
     (gl:declare (gl:consing-area gl:temporary))
     ,@forms))

(def-gl-macro gl:with-permanent-area (&body forms)
  `(gl:let* ((current-region
	       ,(region-number-for-type-and-area 'double-float 'gl:permanent)))
     (gl:declare (gl:consing-area gl:permanent))
     ,@forms))
