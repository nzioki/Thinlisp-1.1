/***
 *
 * Module:      tl/c/generic-math.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/generic-math.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern Obj error_three_args(Obj, Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern sint32 fixnum_floor_first(sint32, sint32);
extern double fmod(double, double);
extern void integer_divide_error(Obj, Obj);
extern sint32 mod_fixnums(sint32, sint32);
extern double mod_float(double, double);
