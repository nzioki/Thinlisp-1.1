/***
 *
 * Module:      tl/c/tl-util.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-util.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym tl_tl_util_symbols[3];

extern Func tl_tl_util_funcs[1];

extern Obj SpackageS;

extern Obj current_region;

extern Obj copy_list(Obj);
extern Obj eql(Obj, Obj);
extern Obj error_one_arg(Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj generic_aref(Obj, sint32);
extern Obj generic_set_aref(Obj, sint32, Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern sint32 length(Obj);
extern Obj nsubst_eql_ident_aux(Obj, Obj, Obj);
extern Obj nthcdr(sint32, Obj);
extern Obj string_equal(Obj, Obj);
extern sint32 sxhash_array_16(uint16 *);
extern sint32 sxhash_cons_tree(Obj);
extern sint32 sxhash_double_float(double);
extern sint32 sxhash_string(unsigned char *);
