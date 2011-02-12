/***
 *
 * Module:      tl/c/do.c
 *
 * Translated on 12/2/2011 15:5:53 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/do.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */

#include "tl.h"
#include "do.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from SYMS-TL-DO() = VOID */

void syms_tl_do (void)
{
  return;
}


/* Translated from INIT-TL-DO() = VOID */

void init_tl_do (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

