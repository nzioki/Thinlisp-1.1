/***
 *
 * Module:      tl/c/packages.h
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/packages.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


extern Sym tl_packages_symbols[3];

extern Obj SpackageS;

extern Obj Sprint_escapeS;

extern Obj Sstandard_outputS;

extern Obj Sterminal_ioS;

extern Obj current_region;

extern Sym tl_format_symbols[];

extern Obj error_one_arg(Obj, Obj);
extern Obj error_three_args(Obj, Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj format_function(Obj, unsigned char *, Obj);
extern sint32 generic_set_fill_pointer(Obj, sint32);
extern Obj get_string_or_file_stream_for_output(Obj, sint32);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern void insert_symbol_into_package(Obj, Obj);
extern unsigned char * string_upcase_function(unsigned char *, sint32, 
    Obj);
extern unsigned char * write_string_function(unsigned char *, Obj, sint32, 
    Obj);
