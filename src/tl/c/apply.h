/***
 *
 * Module:      tl/c/apply.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/apply.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern Obj error_one_arg(Obj, Obj);
extern Obj error_three_args(Obj, Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern sint32 length(Obj);
extern Obj nthcdr(sint32, Obj);
