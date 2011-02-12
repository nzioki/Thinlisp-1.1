/***
 *
 * Module:      tl/c/loop.c
 *
 * Translated on 12/2/2011 15:5:54 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/loop.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */

#include "tl.h"
#include "loop.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from SYMS-TL-LOOP() = VOID */

void syms_tl_loop (void)
{
  return;
}


/* Translated from INIT-TL-LOOP() = VOID */

void init_tl_loop (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

