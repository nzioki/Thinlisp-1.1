(in-package "GL")

;;;; Module INLINE

;;; Copyright (c) 1997 Gensym Corporation.
;;; All rights reserved.

;;; Jim Allard, Glenn Iba






;;;; Functions that are to be inlined via compiler macros




;;; This module contains basic function definitions that are needed early on,
;;; and that are normally to be inlined via compiler macros.





;;; Note that the implementation of `eql' can rely primarily on EQ tests,
;;; but only needs to compare values of numbers in the case where they are both
;;; double floats.  Since fixnums and characters are immediate, EQ is an
;;; accurate test for these types.

(declaim (functional eql))

(defun eql (a b)
  (declare (return-type t))
  ;; The silly if wrapper here puts the translation of this function into a
  ;; required type of boolean, which translates better than the general value
  ;; returning translations of AND and OR.  -jra 2/23/96
  (if (or (eq a b)
	  (and (typep a 'double-float)
	       (typep b 'double-float)
	       (= (the double-float a) (the double-float b))))
      t
      nil))
