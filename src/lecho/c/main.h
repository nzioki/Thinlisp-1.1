/***
 *
 * Module:      lecho/c/main.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/main.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Obj all_packages;

extern void init_lecho_boot(void);
extern void init_lecho_echo(void);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern void init_tl_apply(void);
extern void init_tl_boot(void);
extern void init_tl_do(void);
extern void init_tl_format(void);
extern void init_tl_forward(void);
extern void init_tl_generic_math(void);
extern void init_tl_generic_prim(void);
extern void init_tl_inline(void);
extern void init_tl_input(void);
extern void init_tl_loop(void);
extern void init_tl_packages(void);
extern void init_tl_stubs(void);
extern void init_tl_tl_basics(void);
extern void init_tl_tl_extension(void);
extern void init_tl_tl_prim(void);
extern void init_tl_tl_types(void);
extern void init_tl_tl_util(void);
extern void init_tl_versions(void);
extern void main_1(Obj);
extern Obj make_package_1(unsigned char *, Obj);
extern void syms_lecho_boot(void);
extern void syms_lecho_echo(void);
extern void syms_tl_apply(void);
extern void syms_tl_boot(void);
extern void syms_tl_do(void);
extern void syms_tl_format(void);
extern void syms_tl_forward(void);
extern void syms_tl_generic_math(void);
extern void syms_tl_generic_prim(void);
extern void syms_tl_inline(void);
extern void syms_tl_input(void);
extern void syms_tl_loop(void);
extern void syms_tl_packages(void);
extern void syms_tl_stubs(void);
extern void syms_tl_tl_basics(void);
extern void syms_tl_tl_extension(void);
extern void syms_tl_tl_prim(void);
extern void syms_tl_tl_types(void);
extern void syms_tl_tl_util(void);
extern void syms_tl_versions(void);
