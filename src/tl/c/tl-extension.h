/***
 *
 * Module:      tl/c/tl-extension.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-extension.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern Obj find_package_1(Obj);
extern Obj generic_fceiling_one(Obj);
extern Obj generic_ffloor_one(Obj);
extern Obj memq(Obj, Obj);
extern sint32 two_arg_gcdf(sint32, sint32);
extern unsigned char write_char(unsigned char, Obj);
extern sint32 write_fixnum(sint32, sint32, sint32, Obj);
extern unsigned char * write_string_function(unsigned char *, Obj, sint32, 
    Obj);
