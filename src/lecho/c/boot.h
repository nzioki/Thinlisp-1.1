/***
 *
 * Module:      lecho/c/boot.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/boot.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym lecho_boot_symbols[2];

extern Obj SpackageS;

extern Obj all_systems;

extern Obj current_system_being_loaded;

extern Sym tl_boot_symbols[];

extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj make_package_1(unsigned char *, Obj);
extern Obj set_get(Obj, Obj, Obj);
