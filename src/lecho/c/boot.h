/***
 *
 * Module:      lecho/c/boot.h
 *
 * Translated on 12/2/2011 15:5:57 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of lecho/lisp/boot.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */


typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str_9;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

extern Sym lecho_boot_symbols[2];

extern Obj SpackageS;

extern Obj all_systems;

extern Obj current_system_being_loaded;

extern Sym tl_boot_symbols[];

extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj make_package_1(unsigned char *, Obj);
extern Obj set_get(Obj, Obj, Obj);
