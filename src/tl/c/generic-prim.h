/***
 *
 * Module:      tl/c/generic-prim.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/generic-prim.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern Obj error_one_arg(Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj nth(sint32, Obj);
extern Obj nthcdr(sint32, Obj);
