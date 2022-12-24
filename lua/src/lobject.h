/*
** $Id: lobject.h,v 2.20.1.2 2008/08/06 13:29:48 roberto Exp $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "lua.h"


/* tags for values visible from Lua */
#if defined(LUA_TINT) && (LUA_TINT > LUA_TTHREAD)
# define LAST_TAG   LUA_TINT
#else
# define LAST_TAG	LUA_TTHREAD
#endif

#define NUM_TAGS	(LAST_TAG+1)


/*
** Extra tags for non-values
*/
#define LUA_TPROTO	(LAST_TAG+1)
#define LUA_TUPVAL	(LAST_TAG+2)
#define LUA_TDEADKEY	(LAST_TAG+3)


/*
** Union of all collectable objects
*/
typedef union GCObject GCObject;


/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
#define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked


/*
** Common header in struct form
*/
typedef struct GCheader {
  CommonHeader;
} GCheader;




/*
** Union of all Lua values
*/
typedef union {
  GCObject *gc;
  void *p;
#ifdef LNUM_COMPLEX
  lua_Complex n;
#else
  lua_Number n;
#endif
#ifdef LUA_TINT
  lua_Integer i;
#endif
  int b;
} Value;


/*
** Tagged Values
*/

#define TValuefields	Value value; int tt

typedef struct lua_TValue {
  TValuefields;
} TValue;


/* Macros to test type */
#define ttisnil(o)	(ttype(o) == LUA_TNIL)

#ifdef LUA_TINT
# if (LUA_TINT & 0x0f) == LUA_TNUMBER
#  define ttisnumber(o) ((ttype(o) & 0x0f) == LUA_TNUMBER)
# else
#  define ttisnumber(o) ((ttype(o) == LUA_TINT) || (ttype(o) == LUA_TNUMBER))
# endif
# define ttisint(o) (ttype(o) == LUA_TINT)
#else
# define ttisnumber(o) (ttype(o) == LUA_TNUMBER)
#endif

#ifdef LNUM_COMPLEX
# define ttiscomplex(o) ((ttype(o) == LUA_TNUMBER) && (nvalue_img_fast(o)!=0))
#endif
#define ttisstring(o)	(ttype(o) == LUA_TSTRING)
#define ttistable(o)	(ttype(o) == LUA_TTABLE)
#define ttisfunction(o)	(ttype(o) == LUA_TFUNCTION)
#define ttisboolean(o)	(ttype(o) == LUA_TBOOLEAN)
#define ttisuserdata(o)	(ttype(o) == LUA_TUSERDATA)
#define ttisthread(o)	(ttype(o) == LUA_TTHREAD)
#define ttislightuserdata(o)	(ttype(o) == LUA_TLIGHTUSERDATA)

/* Macros to access values */
#define ttype(o)	((o)->tt)
#define gcvalue(o)	check_exp(iscollectable(o), (o)->value.gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), (o)->value.p)

#if defined(LUA_TINT) && (LUA_TINT&0x0f == LUA_TNUMBER)
/* expects never 