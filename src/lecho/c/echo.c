/***
 *
 * Module:      lecho/c/echo.c
 *
 * Copyright (c) 1999 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/echo.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "echo.h"


typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str_9;

static const Str_9 str_const
  = { 7, 5, 5, "LECHO" };

static const Str_9 str_const_1
  = { 7, 6, 6, "--help" };

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[69];
} Str_69;

static const Str_69 str_const_2
  = { 7, 66, 66, "Usage: lecho [arg] ...~%  all arguments will be echoed to stdout~%" };

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

static const Str_5 str_const_3
  = { 7, 2, 2, "-n" };

/* Translated from MAIN(T) = FIXNUM */

sint32 main_1 (Obj args)
{
  Obj g;
  unsigned char *g_1;
  unsigned char *g_2;
  int temp;
  Obj terpriP;
  unsigned char *g_3;
  unsigned char *g_4;
  Obj firstP, arg, tl_loop_list_;

  g = ((args!=NULL) ? CAR(args) : (Obj)NULL);
  args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
  (void)g;
  if (args!=NULL) {
    g_1 = coerce_to_string((args!=NULL) ? CAR(args) : (Obj)NULL);
    g_2 = (((Str *)(&str_const_1))->body);      /* "--help" */
    temp = (strcmp((char *)g_1,(char *)g_2)==0);
  }
  else 
    temp = 0;
  if (temp) {
    format_function((Obj)(&T),((Str *)(&str_const_2))->body,    /* "Usage: lecho [arg] ...~%  all arguments will be ec..." */
        (Obj)NULL);
    return -1;
  }
  else {
    g_3 = coerce_to_string((args!=NULL) ? CAR(args) : (Obj)NULL);
    g_4 = (((Str *)(&str_const_3))->body);      /* "-n" */
    if (strcmp((char *)g_3,(char *)g_4)==0) {
      g = ((args!=NULL) ? CAR(args) : (Obj)NULL);
      args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
      (void)g;
      terpriP = (Obj)NULL;
    }
    else 
      terpriP = (Obj)(&T);
    firstP = (Obj)(&T);
    arg = (Obj)NULL;
    tl_loop_list_ = args;
    for (;tl_loop_list_!=NULL;firstP = (Obj)NULL) {
      arg = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      if (firstP==NULL) 
        write_char(' ',(Obj)NULL);
      write_string_function(((Str *)arg)->body,(Obj)NULL,0,(Obj)NULL);
    }
    if (terpriP!=NULL) 
      terpri((Obj)NULL);
    return 0;
  }
}

/* Translated from UFFDA-X(T) = T */

Obj uffda_x_1 (Obj uffda_1)
{
  return ((uffda *)uffda_1)->uffda_x;
}

/* Translated from SETF-UFFDA-X(T T) = T */

Obj setf_uffda_x (Obj uffda_1, Obj new_value)
{
  return ((uffda *)uffda_1)->uffda_x = new_value;
}

/* Translated from UFFDA-Y(T) = T */

Obj uffda_y_1 (Obj uffda_1)
{
  return ((uffda *)uffda_1)->uffda_y;
}

/* Translated from SETF-UFFDA-Y(T T) = T */

Obj setf_uffda_y (Obj uffda_1, Obj new_value)
{
  return ((uffda *)uffda_1)->uffda_y = new_value;
}

/* Translated from UFFDA-Z(T) = T */

Obj uffda_z_1 (Obj uffda_1)
{
  return ((uffda *)uffda_1)->uffda_z;
}

/* Translated from SETF-UFFDA-Z(T T) = T */

Obj setf_uffda_z (Obj uffda_1, Obj new_value)
{
  return ((uffda *)uffda_1)->uffda_z = new_value;
}

/* Translated from MAKE-UFFDA-FUNCTION(T T T) = UFFDA */

Obj make_uffda_function (Obj x, Obj y, Obj z)
{
  Obj g;

  g = alloc_struct(sizeof(uffda),4,-1,17);
  ((uffda *)g)->uffda_x = x;
  ((uffda *)g)->uffda_y = y;
  ((uffda *)g)->uffda_z = z;
  return g;
}

/* Translated from UFFDA-P(T) = T */

Obj uffda_p (Obj x)
{
  sint32 temp;

  return (TYPE_TAG(x,temp)==32) ? ((Obj)(&T)) : (Obj)NULL;
}

/* Translated from COPY-UFFDA(UFFDA) = UFFDA */

Obj copy_uffda (Obj uffda_1)
{
  Obj new_1;

  new_1 = alloc_struct(sizeof(uffda),4,-1,17);
  ((uffda *)new_1)->uffda_x = (((uffda *)uffda_1)->uffda_x);
  ((uffda *)new_1)->uffda_y = (((uffda *)uffda_1)->uffda_y);
  ((uffda *)new_1)->uffda_z = (((uffda *)uffda_1)->uffda_z);
  return new_1;
}

/* Translated from BLATZ(UFFDA) = * */

Obj blatz (Obj x)
{
  Values_count = 1;
  return ((uffda *)x)->uffda_x;
}

/* Translated from SYMS-LECHO-ECHO() = VOID */

void syms_lecho_echo (void)
{
  return;
}


/* Translated from INIT-LECHO-ECHO() = VOID */

void init_lecho_echo (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "LECHO" */
  return;
}

