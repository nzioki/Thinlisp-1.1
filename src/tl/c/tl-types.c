/***
 *
 * Module:      tl/c/tl-types.c
 *
 * Translated on 12/2/2011 15:5:52 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/tl-types.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */

#include "tl.h"
#include "tl-types.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj current_region = (Obj)(&Unbound);

Obj temporary_area_top = (Obj)(&Unbound);

Obj fixnum_signed_byte_width = BOXFIX(30);

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

