/***
 *
 * Module:      lecho/c/echo.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/echo.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj SpackageS;

extern unsigned char * coerce_to_string(Obj);
extern Obj find_package_1(Obj);
extern Obj format_function(Obj, unsigned char *, Obj);
extern Obj terpri(Obj);
extern unsigned char write_char(unsigned char, Obj);
extern unsigned char * write_string_function(unsigned char *, Obj, sint32, 
    Obj);
