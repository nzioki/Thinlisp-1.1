/***
 *
 * Module:      lecho/c/boot.c
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/boot.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "boot.h"


typedef struct{
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str_9;

static const Str_9 str_const
  = { 7, 7, 7, "TL-USER" };

static const Str_9 str_const_1
  = { 7, 5, 5, "LECHO" };

typedef struct{
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

static const Str_5 str_const_2
  = { 7, 2, 2, "TL" };

static Obj cons_const[2]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL}
#else
  {(Obj)(&str_const_2), (Obj)NULL}              /* "TL" */
#endif
  ;

static Obj cons_const_1[2]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL}
#else
  {(Obj)(tl_boot_symbols+0), (Obj)NULL}         /* TL */
#endif
  ;

static Obj cons_const_2[4]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL}
#else
  {(Obj)(tl_boot_symbols+4), (Obj)(((uint32)(&(cons_const_2[2])))   /* BOOT */
      +2), (Obj)(lecho_boot_symbols+1), (Obj)NULL}  /* ECHO */
#endif
  ;

static const Str_5 str_const_3
  = { 7, 4, 4, "ECHO" };

Sym lecho_boot_symbols[2];

/* Translated from SYMS-LECHO-BOOT() = VOID */

void syms_lecho_boot (void)
{
  Obj cached_tl_user_package;

  cached_tl_user_package = find_package_1((Obj)(&str_const));   /* "TL-USER" */
  init_symbol_into_package((Obj)(&(lecho_boot_symbols[0])),(Obj)(&str_const_1),     /* "LECHO" */
      1851,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(lecho_boot_symbols[1])),(Obj)(&str_const_3),     /* "ECHO" */
      1019,cached_tl_user_package);
  return;
}


/* Translated from INIT-LECHO-BOOT() = VOID */

void init_lecho_boot (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL-USER" */
#if defined(NO_ADDRESS_CONSTANTS)
    (cons_const[0]) = (Obj)(&str_const_2);      /* "TL" */
#endif
  if (find_package_1((Obj)(&str_const_1))==NULL)    /* "LECHO" */
    make_package_1(((Str *)(&str_const_1))->body,(Obj)((    /* "LECHO" */
        (uint32)(&(cons_const[0])))+2));
  current_system_being_loaded = (Obj)(lecho_boot_symbols+0);    /* LECHO */
  all_systems = alloc_cons((Obj)(lecho_boot_symbols+0),all_systems,     /* LECHO */
      0);
  set_get((Obj)(lecho_boot_symbols+0),(Obj)(tl_boot_symbols     /* LECHO */
      +1),(Obj)NULL);                           /* SYSTEM-NICKNAMES */
#if defined(NO_ADDRESS_CONSTANTS)
    (cons_const_1[0]) = (Obj)(tl_boot_symbols+0);   /* TL */
#endif
  set_get((Obj)(lecho_boot_symbols+0),(Obj)(tl_boot_symbols     /* LECHO */
      +2),(Obj)(((uint32)(&(cons_const_1[0])))  /* SYSTEM-USED-SYSTEMS */
      +2));
#if defined(NO_ADDRESS_CONSTANTS)
  {
    (cons_const_2[0]) = (Obj)(tl_boot_symbols+4);   /* BOOT */
    (cons_const_2[1]) = (Obj)(((uint32)(&(cons_const_2[2])))+2);
    (cons_const_2[2]) = (Obj)(lecho_boot_symbols+1);    /* ECHO */
  }
#endif
  set_get((Obj)(lecho_boot_symbols+0),(Obj)(tl_boot_symbols     /* LECHO */
      +3),(Obj)(((uint32)(&(cons_const_2[0])))+2));     /* SYSTEM-MODULES */
  return;
}

