/***
 *
 * Module:      tl/c/versions.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/versions.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym tl_versions_symbols[36];

extern Obj SpackageS;

extern Obj current_region;

extern Sym tl_boot_symbols[];

extern void collect_all_used_systems(Obj);
extern Obj find_package_1(Obj);
extern Obj find_package_or_error_1(Obj);
extern Obj format_function(Obj, unsigned char *, Obj);
extern Obj get(Obj, Obj, Obj);
extern sint32 get_platform_code(void);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj intern_string_in_package(unsigned char *, sint32, Obj);
extern Obj memq(Obj, Obj);
extern Obj nreverse(Obj);
extern Obj set_get(Obj, Obj, Obj);
extern sint32 sxhash_string(unsigned char *);
