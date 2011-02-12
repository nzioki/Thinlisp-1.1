/***
 *
 * Module:      lecho/c/echo.h
 *
 * Translated on 12/2/2011 15:5:57 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of lecho/lisp/echo.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 * ThinLisp is Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *             Copyright (c) 1997-1998 Gensym Corporation.  All rights reserved.
 *
 */


typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str_9;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[69];
} Str_69;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

extern Obj SpackageS;

extern unsigned char * coerce_to_string(Obj);
extern Obj find_package_1(Obj);
extern Obj format_function(Obj, unsigned char *, Obj);
extern Obj terpri(Obj);
extern unsigned char write_char(unsigned char, Obj);
extern unsigned char * write_string_function(unsigned char *, Obj, sint32, 
    Obj);
