/***
 *
 * Module:      tl/c/forward.c
 *
 * Translated on 12/2/2011 15:5:56 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/forward.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */

#include "tl.h"
#include "forward.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from SYMS-TL-FORWARD() = VOID */

void syms_tl_forward (void)
{
  return;
}


/* Translated from INIT-TL-FORWARD() = VOID */

void init_tl_forward (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

