/***
 *
 * Module:      tl/c/forward.c
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/forward.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "forward.h"


typedef struct{
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

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

