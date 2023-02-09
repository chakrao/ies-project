/*
** $Id: luaconf_internal.h,v ... $
** Configuration file for Lua (internal)
** See Copyright Notice in lua.h
*/


#ifndef lconfig_internal_h
#define lconfig_internal_h

#ifdef lua_assert
# include <assert.h>
#endif

/*
** ==================================================================
** Search for "@@" to find all configurable definitions.
** ===================================================================
*/


/*
@@ LUA_ANSI controls the use of non-ansi features.
** CHANGE it (define it) if you want Lua to avoid the use of any
** non-ansi feature or library.
*/
#if defined(__STRICT_ANSI__)
#define LUA_ANSI
#endif


#if !defined(LUA_ANSI) && defined(_WIN32)
#define LUA_WIN
#endif

#if defined(LUA_USE_LINUX)
#define LUA_USE_POSIX
#define LUA_USE_DLOPEN		/* needs an extra library: -ldl */
#define LUA_USE_READLINE	/* needs some extra libraries */
#endif

#if defined(LUA_USE_MACOSX)
#define LUA_USE_POSIX
#define LUA_DL_DYLD		/* does not need extra library */
#endif



/*
@@ LUA_USE_POSIX includes all functionallity listed as X/Open System
@* Interfaces Extension (XSI).
** CHANGE it (define it) if your system is XSI compatible.
*/
#if defined(LUA_USE_POSIX)
#define LUA_USE_MKSTEMP
#define LUA_USE_ISATTY
#define LUA_USE_POPEN
#define LUA_USE_ULONGJMP
#endif


/*
@@ LUA_DIRSEP is the directory separator (for submodules).
** CHANGE it if your machine does not use "/" as the directory separator
** and is not Windows. (On Windows Lua automatically uses "\".)
*/
#if defined(_WIN32)
#define LUA_DIRSEP	"\\"
#else
#define LUA_DIRSEP	"/"
#endif


/*
@@ LUA_PATHSEP is the character that separates templates in a path.
@@ LUA_PATH_MARK is the string that marks the substitution points in a
@* template.
@@ LUA_EXECDIR in a Windows path is replaced by the executable's
@* directory.
@@ LUA_IGMARK is a mark to ignore all before it when bulding the
@* luaopen_ function name.
** CHANGE them if for some reason your system cannot use those
** characters. (E.g., if one of those characters is a common character
** in file/directory names.) Probably you do not need to change them.
*/
#define LUA_PATHSEP	";"
#define LUA_PATH_MARK	"?"
#define LUA_EXECDIR	"!"
#define LUA_IGMARK	"-"


/*
** Default number mode
*/
#if (!defined LNUM_DOUBLE) && (!defined LNUM_FLOAT) && (!defined LNUM_LDOUBLE)
# define LNUM_DOUBLE
#endif

/*
** Require C99 mode for COMPLEX, FLOAT and LDOUBLE (only DOUBLE is ANSI C).
*/
#if defined(LNUM_COMPLEX) && (__STDC_VERSION__ < 199901L)
# error "Need C99 for complex (use '--std=c99' or similar)"
#elif defined(LNUM_LDOUBLE) && (__STDC_VERSION__ < 199901L) && !defined(_MSC_VER)
# error "Need C99 for 'long double' (use '--std=c99' or similar)"
#elif defined(LNUM_FLOAT) && (__STDC_VERSION__ < 199901L)
/* LNUM_FLOAT not supported on Windows */
# error "Need C99 for 'float' (use '--std=c99' or similar)"
#endif

/*
** COMPLEX mode currently only with integer optimization
*/
#if defined(LNUM_COMPLEX) && !(defined(LNUM_INT32) || defined(LNUM_INT64))
# error "LNUM_COMPLEX needs to be used together with LNUM_INTxx"
#endif
 
/*
** Number mode identifier to accompany the version string.
*/
#ifdef LNUM_COMPLEX
# define _LNUM1 "complex "
#else
# define _LNUM1 ""
#endif
#ifdef LNUM_DOUBLE
# define _LNUM2 "double"
#elif defined(LNUM_FLOAT)
# define _LNUM2 "float"
#elif defined(LNUM_LDOUBLE)
# define _LNUM2 "ldouble"
#endif
#ifdef LNUM_INT32
# define _LNUM3 " int32"
#elif defined(LNUM_INT64)
# define _LNUM3 " int64"
#else
# define _LNUM3 ""
#endif
#ifdef __FAST_MATH__
# define _LNUM4 "fastmath"
#else
# define _LNUM4 ""
#endif
#define LUA_LNUM _LNUM1 _LNUM2 _LNUM3 _LNUM4


/* 
** LUAI_MAXNUMBER2STR: size of a buffer fitting any number->string result.
**
**  double:  24 (sign, x.xxxxxxxxxxxxxxe+nnnn, and \0)
**  int64:   21 (19 digits, sign, and \0)
**  long double: 43 for 128-bit (sign, x.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxe+nnnn, and \0)
**           30 for 80-bit (sign, x.xxxxxxxxxxxxxxxxxxxxe+nnnn, and \0)
*/
#ifdef LNUM_LDOUBLE
# define _LUAI_MN2S 44
#else
# define _LUAI_MN2S 24
#endif

#ifdef LNUM_COMPLEX
# define LUAI_MAXNUMBER2STR (2*_LUAI_MN2S)
#else
# define LUAI_MAXNUMBER2STR _LUAI_MN2S
#endif

/*
@@ lua_number2int is a macro to convert lua_Number to int.
@@ lua_number2integer is a macro to convert lua_Number to lua_Integer.
** CHANGE them if you know a faster way to convert a lua_Number to
** int (with any rounding method and without throwing errors) in your
** system. In Pentium machines, a naive typecast from double to int
** in C is extremely slow, so any alternative is worth trying.
**
** Note: Using '-msse' or '-msse3' is highly recommended for newer x86
**      processors. They have fast instructions to bypass the Pentium FP->int
**      efficiency problem altogether.  --AKa 6-Apr-2009
*/

/* On old Pentium (SSE not enabled), resort to a trick */
#if defined(LNUM_DOUBLE) && !defined(LUA_ANSI) && !defined(__SSE2__) && \
    (defined(__i386) || defined (_M_IX86) || defined(__i386__))

/* On a Microsoft compiler, use assembler */
# if defined(_MSC_VER)
#  define lua_number2int(i,d)   __asm fld d   __asm fistp i
# else

/* the next trick should work on any Pentium, but sometimes clashes
   with a DirectX idiosyncrasy */
union luai_Cast { double l_d; long l_l; };
#  define lua_number2int(i,d) \
  { volatile union luai_Cast u; u.l_d = (d) + 6755399441055744.0; (i) = u.l_l; }
# endif

# ifndef LNUM_INT64
#  define lua_number2integer    lua_number2int
# endif

/* this option always works (and is fast on most platforms) */
#else
# define lua_number2int(i,d)        ((i)=(int)(d))
#endif

/* Note: Some compilers (OS X gcc 4.0?) may choke on double->long long conversion 
 *       since it can lose precision. Others do require 'long long' there.  
 */
#ifndef lua_number2integer
# define lua_number2integer(i,d)    ((i)=(lua_Integer)(d))
#endif

/*
** 'luai_abs()' to give absolute value of 'lua_Integer'
*/
#ifdef LNUM_INT64
# if (__STDC_VERSION__ >= 199901L)
#  define luai_abs llabs
# else
#  define luai_abs(v) ((v) >= 0 ? (v) : -(v))
# endif
#else
# define luai_abs abs
#endif

/*
** LUAI_UACNUMBER is the result of an 'usual argument conversion' over a number.
** LUAI_UACINTEGER the same, over an integer.
*/
#define LUAI_UACNUMBER	double
#define LUAI_UACINTEGER long

/* ANSI C only has math funcs for 'double. C99 required for float and long double
 * variants.
 */
#ifdef LNUM_DOUBLE
# define _LF(name) name
#elif defined(LNUM_FLOAT)
# define _LF(name) name ## f
#elif defined(LNUM_LDOUBLE)
# define _LF(name) name ## l
#endif


/*
@@ LUAI_FUNC is a mark for all extern functions that are not to be
@* exported to outside modules.
@@ LUAI_DATA is a mark for all extern (const) variables that are not to
@* be exported to outside modules.
** CHANGE them if you need to mark them in some special way. Elf/gcc
** (versions 3.2 and later) mark them as "hidden" to optimize access
** when Lua is compiled as a shared library.
*/
#if defined(luaall_c)
#define LUAI_FUNC	static
#define LUAI_DATA	/* empty */

#elif defined(__GNUC__) && ((__GNUC__*100 + __GNUC_MINOR__) >= 302) && \
      defined(__ELF__)
#define LUAI_FUNC	__attribute__((visibility("hidden"))) extern
#define LUAI_DATA	LUAI_FUNC

#else
