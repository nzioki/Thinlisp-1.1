/***
 *
 * Module:      tl/c/tl-prim.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-prim.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym tl_tl_prim_symbols[12];

extern Obj SpackageS;

extern Obj symbol_plist_of_nil;

extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj reverse_list(Obj);
extern unsigned char * reverse_string(unsigned char *);
