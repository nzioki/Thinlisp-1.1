/***
 *
 * Module:      tl/c/inline.c
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/inline.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "inline.h"


typedef struct{
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from EQL(T T) = T */

Obj eql (Obj a, Obj b)
{
  if ((a==b) || ((((a!=NULL) && (((((uint32)a)&3)==0) && (((Hdr *)a)->type
      ==5))) && ((b!=NULL) && (((((uint32)b)    /* DOUBLE-FLOAT type tag */
      &3)==0) && (((Hdr *)b)->type==5)))) && (  /* DOUBLE-FLOAT type tag */
      ((Ldouble *)a)->body==(((Ldouble *)b)->body)))) 
    return (Obj)(&T);
  else 
    return (Obj)NULL;
}

Obj symbol_plist_of_nil = (Obj)(&Unbound);

/* Translated from SYMS-TL-INLINE() = VOID */

void syms_tl_inline (void)
{
  return;
}


/* Translated from INIT-TL-INLINE() = VOID */

void init_tl_inline (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (symbol_plist_of_nil==(Obj)(&Unbound)) 
    symbol_plist_of_nil = (Obj)NULL;
  return;
}

