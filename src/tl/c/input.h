/***
 *
 * Module:      tl/c/input.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/input.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym tl_input_symbols[5];

extern Obj SpackageS;

extern Obj Sterminal_ioS;

extern sint32 delete_named_file(char *);
extern Obj error_one_arg(Obj, Obj);
extern Obj error_or_value(Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
