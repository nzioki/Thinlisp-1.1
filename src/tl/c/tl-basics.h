/***
 *
 * Module:      tl/c/tl-basics.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-basics.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern Obj eql(Obj, Obj);
extern Obj equal(Obj, Obj);
extern Obj error_one_arg(Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj last(Obj);
extern sint32 length(Obj);
