/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 +
 + Module:      notify.c
 +
 + Copyright (c) 1999 The Thinlisp Group
 + Copyright (c) 1995 Gensym Corporation
 + All Rights Reserved.
 +
 + This file is part of ThinLisp.
 +
 + ThinLisp is open source; you can redistribute it and/or modify it
 + under the terms of the ThinLisp License as published by the ThinLisp
 + Group; either version 1 or (at your option) any later version.
 +
 + ThinLisp is distributed in the hope that it will be useful, but
 + WITHOUT ANY WARRANTY; without even the implied warranty of
 + MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 +
 + For additional information see <http://www.thinlisp.org/>
 +
 + Author   Jim Allard
 +
 + Description:
 +   Functions for notifying the user, warning the user, informing the user of
 +   errors and long-jumping to an error handler, and for notifying the user of
 +   a fatal error and exiting the process.
 + 
 + Key:
 +   +++++++ Module Header.   Used for file-wide information.
 +   %%%%%%% Section Header.  Used to delimit logical sections.
 +   ******* Function Header. Used to define a single function.
 +
 +   0000000 Externally visible function
 +   1111111 Internal (static) function
 +   ??????? Function existence is questionable.
 +
 +   A function banner may contain the following: 
 +      Multiply Defined    Function appears multiple times, each definition
 +                          for an #ifdef specified platform.
 +      Mixed Ifdefs        Single definition for function containing platform
 +                          specific code #ifdef's.
 +
 + File Organization:
 +   Section:      INCLUDE FILES 
 +   Section:      Notification Functions
 +      
 +
 + External Interface:
 +
 + Dependencies:
 +      This file has no external dependencies.
 +
 + Notes:
 +
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      INCLUDE FILES
 %
 % Description:  All required include files are referenced here.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "tlt.h"
#include "notify.h"
#include "time.h"

/* On the Sun, we have warnings turned way up, and the standard Sun include file
 * time.h doesn't have a declaration for time(). 
 */
#if defined(sun4)
extern time_t time(time_t *timeptr);
#endif


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      User Notification Functions
 %
 % Description:
 %      The functions notify, warn, error, and fatal_error all take a char*
 %      containing the message to deliver.  Each then performs slightly
 %      different wrapping message behavior or exits.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static char *current_time_string(void)
{
  time_t now;

  now = time(NULL);
  return ctime(&now);
}

void notify (char *message)
{
  printf("%s%s\n", current_time_string(), message);
  return;
}

void warn (char *message)
{
  printf("%s**** WARNING ****\n%s\n", current_time_string(), message);
  return;
}

void error (char *message)
{
  /* Insert throw calls here instead of exit.  -jra 12/18/95 */
  fatal_error(message);
}

void type_cast_error(char *source_type, char *target_type)
{
  char error_message[256];
  sprintf(error_message,"Unable to coerce type %s to type %s.", 
	  source_type, target_type);
  error(error_message);
}

void fatal_error (char *message)
{
  printf("%s**** ERROR ****\n%s\n", current_time_string(), message);
  exit(1);
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Formatting Tools
 %
 % Description:
 %      The functions in this section implement printing primitives into
 %      fill-pointered strings.  Each of these functions takes an Str struct
 %      that should have enough room to hold the formatting of the given value,
 %      and a value to print.  If there is not enough room in the string given,
 %      then these functions should call error.  The functions are
 %      write_string_into_str, write_char_into_str, write_fixnum_into_str, and
 %      write_double_into_str.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void write_fixnum_into_str (sint32 value, sint32 width, Str *output)
{
  char *base, *current_end, *new_end;

  base = (char *)(output->body);
  current_end = base + output->fill_length;

  if (width > 0)
    sprintf(current_end, "%*ld", (int)width, (long)value);
  else
    sprintf(current_end, "%ld", (long)value);
  new_end = strchr(current_end, 0);

  if (new_end > base + output->length)
    error("Overflow in write_string_into_str.");

  output->fill_length = (uint32)new_end - (uint32)base;
  return;
}

void write_double_into_str (double value, sint32 width, Str *output)
{
  char *base, *current_end, *new_end;

  base = (char *)(output->body);
  current_end = base + output->fill_length;

  if (width > 0)
    sprintf(current_end, "%*g", (int)width, value);
  else
    sprintf(current_end, "%g", value);
  new_end = strchr(current_end, 0);

  if (new_end > base + output->length)
    error("Overflow in write_string_into_str.");

  output->fill_length = (uint32)new_end - (uint32)base;
  return;
}
