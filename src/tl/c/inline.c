/***
 *
 * Module:      tl/c/inline.c
 *
 * Translated on 12/2/2011 15:5:52 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/inline.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */

#include "tl.h"
#include "inline.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from EQL(T T) = T */

Obj eql (Obj a, Obj b)
{
  if ((a==b) || ((((a!=NULL) && ((IMMED_TAG(a)==0) && (STD_TAG(a)==5)))     /* DOUBLE-FLOAT-P */
       && ((b!=NULL) && ((IMMED_TAG(b)==0) && (STD_TAG(b)==5))))    /* DOUBLE-FLOAT-P */
       && (((Ldouble *)a)->body==(((Ldouble *)b)->body)))) 
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

