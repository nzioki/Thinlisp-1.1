/***
 *
 * Module:      tl/c/tl-types.c
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-types.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-types.h"


typedef struct{
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj current_region = (Obj)(&Unbound);

Obj temporary_area_top = (Obj)(&Unbound);

Obj most_positive_fixnum = BOXFIX(536870911);

Obj most_negative_fixnum = BOXFIX(-536870912);

/* Translated from SYMS-TL-TL-TYPES() = VOID */

void syms_tl_tl_types (void)
{
  return;
}


/* Translated from INIT-TL-TL-TYPES() = VOID */

void init_tl_tl_types (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (current_region==(Obj)(&Unbound)) 
    current_region = BOXFIX(0);
  if (temporary_area_top==(Obj)(&Unbound)) 
    temporary_area_top = (Obj)NULL;
  return;
}

