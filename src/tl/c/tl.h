/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 +
 + Copyright (c) 1994-1995 Gensym Corporation.  All Rights Reserved.
 +
 + Module:      glt.h
 +
 + Author(s):   Jim Allard
 +
 + Description: Declarations and externs for the Gensym Language (GL) runtime
 + library, itself called GLL.
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
 +   Section:      Typedefs
 +      Uint8, Sint8, Uint16, Sint16, Uint32, Sint32,
 +      Hdr, Obj, Str, Sv, Sa_uint8, Sa_uint16, Sa_double
 +   Section:      Externs for glt.c
 +      Values_count, Values_buffer
 +
 +   Section:      TESTING
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

#include <ctype.h>
#include <math.h>

#if defined(__STDC__)
#  include <string.h>
#endif

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
/**
 * Get rid of the old inlining cruft.  We have new cruft here.  -jallard 5/21/99
 * #include "ab-macros.h"
 */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Typedefs
 %
 % Description:
 %      All gll built-in types are found here.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*****************************************************************************
 * Integer Types
 *
 *   Description:
 *     Integer types in GL translated code will always refer to the following
 *     types, representing signed and unsigned integers containing the given
 *     number of bits.  The one except to this rule is the type unsigned char,
 *     which may appear in GL translated code and is assumed to be equivalent to
 *     uint8.  Twos complement representation of all integers is assumed.
 * 
 *       uint8, sint8, uint16, sint16, uint32, sint32
 *   Notes:
 *
 *   Modifications:
 ******************************************************************************/

typedef unsigned char  uint8;
typedef   signed char  sint8;
typedef unsigned short uint16;
typedef   signed short sint16;
typedef unsigned int   uint32;
typedef   signed int   sint32;

/*****************************************************************************
 * Base GLL Stucture Types
 *
 *   Description:
 *     Structure types in GL provide the base level data structure needed to
 *     build the rest of the runtime system.
 *
 *       Hdr - The header of all heap allocated Lisp objects, has type tag,
 *       Obj - an unsigned int 32 holding pointers and immediate values,
 *       Sv - heap allocated simple-vectors,
 *       Str - heap allocated strings,
 *       Sa_uint8 - heap allocated (unsigned-byte 8) simple-arrays,
 *       Sa_uint16 - heap allocated (unsigned-byte 16) simple-arrays,
 *       Sa_double - heap allocated double-float simple-arrays,
 *       Ldouble - heap allocated double-floats,
 *       Mdouble - heap allocated managed-floats,
 *       Sym - symbols,
 *       Func - compiled-functions,
 *       Pkg - packages,
 *       String_strm - string-streams,
 *       File_strm - file-streams.
 *
 *   Notes:
 *     See glt.txt for a description of the design of the other types.  -jra
 *     1/4/95
 *   Modifications:
 *****************************************************************************/

typedef struct {
  unsigned int type:  8;
  unsigned int fill: 24;
} Hdr;

#if defined(alphaosf)
typedef uint32 Obj;
#else
typedef void *Obj;
#endif

typedef struct {
  Obj car;
  Obj cdr;
} Cons;

typedef struct {
  unsigned int type:    8;
  unsigned int length: 24;
  Obj body[1];
} Sv;

typedef struct {
  unsigned int type:         8;
  unsigned int length:      24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str;

typedef struct {
  unsigned int type:         8;
  unsigned int length:      24;
  unsigned int fill_length: 24;
  uint8 body[4];
} Sa_uint8;

typedef struct {
  unsigned int type:         8;
  unsigned int length:      24;
  unsigned int fill_length: 24;
  uint16 body[2];
} Sa_uint16;

typedef struct {
  unsigned int type:    8;
  unsigned int length: 24;
  double body[1];
} Sa_double;

typedef struct {
  unsigned int type: 8;
  double body;
} Ldouble;

typedef struct {
  unsigned int type: 8;
  union {
    double value;
    Obj    next_object;
  } body;
} Mdouble;

typedef struct {
  unsigned int type:        8;
  unsigned int local_value: 1;
  unsigned int external:    1;
    signed int balance:     4;
  unsigned int imported:    1;
  unsigned int name_hash:  16;
  Obj symbol_name;
  Obj symbol_value;
  Obj symbol_plist;
  Obj symbol_package;
  Obj symbol_function;
  Obj left_branch;
  Obj right_branch;
} Sym;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj);
} Func;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(void);
} Func_0;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj);
} Func_1;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj);
} Func_2;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj);
} Func_3;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj);
} Func_4;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj);
} Func_5;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj);
} Func_6;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_7;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_8;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_9;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_10;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_11;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_12;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj);
} Func_13;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj);
} Func_14;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj);
} Func_15;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj, Obj);
} Func_16;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj, Obj, Obj);
} Func_17;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj, Obj, Obj, Obj);
} Func_18;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_19;

typedef struct {
  unsigned int type:               8;
  unsigned int arg_count:          8;
  unsigned int optional_arguments: 8;
  unsigned int sets_values_count:  1;
  Obj default_arguments;
  Obj name;
  Obj (*c_function)(Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj,
		    Obj, Obj, Obj, Obj, Obj, Obj, Obj, Obj);
} Func_20;

typedef struct {
  unsigned int type:  8;
  Obj name;
  Obj root_symbol;
  Obj used_packages;
} Pkg;

typedef struct {
  unsigned int type:  8;
  Obj strings;
  unsigned char *input_string;
  sint32 input_index;
  sint32 input_index_bounds;
} String_strm;

typedef struct {
  unsigned int type:  8;
  FILE       *input;
  FILE       *output;
  char       *filename;
  char       *mode;
} File_strm;



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Externs for Handwritten GLT C files
 %
 % Description:
 %      Externs for the hand-written C utilities in glt.c and notify.c
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* The first set are for glt.c */

extern sint32 Values_count;

extern Obj Values_buffer[20];

extern Hdr Unbound;

#define THROW_STACK_MAX 2048

extern Obj Throw_stack[THROW_STACK_MAX];

extern sint32 Throw_stack_top;

extern Obj Current_throw;

extern void store_values_on_stack(Obj first_value);

extern Obj retrieve_values_from_stack(void);

extern void throw_towards_catch_tag(Obj throw_tag, Obj first_value);

extern void malloc_block_into_region(sint32 region, sint32 byte_count,
				     sint32 silent);

extern sint32 region_number_bytes_size(sint32 region);

extern sint32 region_number_bytes_used(sint32 region);

extern sint32 region_number_bytes_available(sint32 region);

extern Obj alloc_cons(Obj new_car, Obj new_cdr, sint32 region);

extern Obj hook_up_cdrs(Obj *cons_array, sint32 count, Obj final_cdr);

extern Obj alloc_list(sint32 length, sint32 init_cars_p, Obj init_elt, sint32 region);

extern Obj alloc_simple_vector(sint32 length, sint32 region, sint32 type_tag);

extern Obj alloc_string(sint32 dimension, sint32 region, sint32 type_tag);

extern Obj alloc_uint8_array(sint32 length, sint32 region, sint32 type_tag);

extern Obj alloc_uint16_array(sint32 length, sint32 region, sint32 type_tag);

extern Obj alloc_double_array(sint32 length, sint32 region, sint32 type_tag);

extern Obj alloc_ldouble(double new_value, sint32 region, sint32 type_tag);

extern Obj alloc_mdouble(double new_value, sint32 region, sint32 type_tag);

extern Obj alloc_symbol(sint32 region, sint32 type_tag);

extern Sym T;

extern Obj alloc_package(Obj name, Obj used, sint32 region, sint32 type_tag);

extern Obj alloc_string_strm(sint32 region, sint32 type_tag);

extern Obj alloc_file_strm (FILE *input, FILE* output, char *filename,
				 char* mode, sint32 region, sint32 type_tag);


/* The following are for notify.c */

extern void notify(char *message);

extern void warn(char *message);

extern void error(char *message);

extern void type_cast_error(char *source_type, char *target_type);

extern void fatal_error(char *message);

extern void write_fixnum_into_str(sint32 value, sint32 width, Str *output);

extern void write_double_into_str(double value, sint32 width, Str *output);



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Externs for System Library Functions
 %
 % Description:
 %      In some cases it seems that externs have been explicitly left out on
 %      some platforms.  The following section implements declarations for those
 %      functions to suppress warnings from C compilers.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if defined(sun4)
extern int printf(const char *format, ...);
extern int fprintf(FILE *stream, const char *format, ...);
extern int fputs(const char*s, FILE *stream);
extern void *memset(void *ptr, int val, size_t len);
extern int _flsbuf(unsigned char c, FILE *f);
extern int _filbuf(FILE *f);
extern int fflush(FILE *stream);
extern int fclose(FILE *stream);
extern int unlink(char *filename);
#endif



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Defines for DLL generation
 %
 % Description:
 %      External and imported functions of DLL libraries on Windows platforms
 %      need an extra declaration.  The macros DLLIMPORT and DLLEXPORT abstract
 %      those declarations for use within our machine independent C translated
 %      files.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if defined(WIN32) && defined(GSI_DLL)
#define DLLIMPORT __declspec( dllimport )
#define DLLEXPORT __declspec( dllexport )
#else
#define DLLIMPORT
#define DLLEXPORT
#endif



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 % Section:      Defines for Translated Files
 %
 % Description:
 %      This section contains a few defines used by the translator.  The number
 %      of defines has purposefully been kept to a minimum in order to keep it
 %      clear to the reader of translated C code what the cost is of various
 %      operations.  However, for a few operations, such as CAR and CDR, there
 %      are single machine instruction macros that greatly improve the clarity
 %      of the translated code.
 %
 % Notes:
 %
 % Modifications:
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define CAR(cons_as_obj) (((Cons *)((uint32)(cons_as_obj)-2))->car)
#define CDR(cons_as_obj) (((Cons *)((uint32)(cons_as_obj)-2))->cdr)

#if defined(__osf__)
#  define NO_ADDRESS_CONSTANTS 1
#endif

/**
 * The macro BOXFIX takes sint32 values and convert them to fixnum format (left
 * shift by 2 and add the immediate type tag 1).  The macro UNBOXFIX does the
 * inverse transform.  The macro BOXCHAR and UNBOXCHAR do the same for unsigned
 * char and Lisp characters (immediate type tag of 3).
 */

#define BOXFIX(any_int) (Obj)(((uint32)(any_int)<<2)+1)
#define UNBOXFIX(fixnum_int) ((sint32)(fixnum_int)>>2)

#define BOXCHAR(char_int) (Obj)((((uint32)(char_int)&0xff)<<2)+3)
#define UNBOXCHAR(character) (unsigned char)((uint32)(character)>>2)

/**
 * The macro StrHDR() takes a pointer to an unsigned char, and returns a Str* to
 * the Str structure that contains it.  ObjStrHDR() does the same, but returns
 * it as type Obj.  SvHDR takes a pointer to the Obj array in a simple-vector,
 * and returns an Sv* to its containing header.  ObjSvHDR() does the same, but
 * returning it as an Obj.
 */

#define StrHDR(string) ((Str *)((uint32)(string) - (uint32)(&(((Str *)NULL)->body[0]))))
#define ObjStrHDR(string) ((Obj)((uint32)(string) - (uint32)(&(((Str *)NULL)->body[0]))))

#define SvHDR(obj_ptr) ((Sv *)((uint32)(obj_ptr) - sizeof(Hdr)))
#define ObjSvHDR(obj_ptr) ((Obj)((uint32)(obj_ptr) - sizeof(Hdr)))
