/***
 *
 * Module:      tl/c/stubs.h
 *
 * Translated on 12/2/2011 15:5:52 GMT
 * 
 * Copyright (c) 1999-2000 The ThinLisp Group.  All rights reserved.
 *
 * Description: Translation of tl/lisp/stubs.lisp.
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
  unsigned char body[5];
} Str_5;

extern Obj SpackageS;

extern Obj find_package_1(Obj);
