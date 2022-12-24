/*
** $Id: lobject.c,v 2.22.1.1 2007/12/27 13:02:25 roberto Exp $
** Some generic functions over Lua objects
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define lobject_c
#define LUA_CORE

#include "lua.h"

#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lvm.h"
#include "llex.h"
#include "lnum.h"


const TValue luaO_nilobject_ = {{NULL}, LUA_TNIL};


/*
** converts an integer to a "floating point byte", represented as
** (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
** eeeee != 0 and (xxx) otherwise.
*/
int luaO_int2fb (unsigned int x) {
  int e = 0;  /* expoent */
  while (x >= 16) {
    x = (x+1) >> 1;
    e++;
  }
  if (x < 8) return x;
  else return ((e+1) << 3) | (cast_int(x) - 8);
}


/* converts back */
int luaO_fb2int (int x) {
  int e = (x >> 3) & 31;
  if (e == 0) return x;
  else return ((x & 7)+8) << (e - 1);
}


int luaO_log2 (unsigned int x) {
  static const lu_byte log_2[256] = {
    0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
  };
  int l = -1;
  while (x >= 256) { l += 8; x >>= 8; }
  return l + log_2[x];

}


int luaO_rawequalObj (const TValue *l, const TValue *r) {
  int tl= ttype(l), tr= ttype(r);
  if (tl == tr) {
    switch (tl) {
      case LUA_TNIL:
        return 1;
#ifdef LUA_TINT
      case LUA_TINT:
        return ivalue(l) == ivalue(r);
#endif
      case LUA_TNUMBER:
#ifdef LNUM_COMPLEX
        if (!luai_numeq(nvalue_img_fast(l), nvalue_img_fast(r))) return 0;
#endif
        return luai_numeq(nvalue_fast(l), nvalue_fast(r));
      case LUA_TBOOLEAN:
        return bvalue(l) == bvalue(r);  /* boolean true must be 1 !! */
      case LUA_TLIGHTUSERDATA:
        return pvalue(l) == pvalue(r);
      default:
        lua_assert(iscollectable(l));
        return gcvalue(l) == gcvalue(r);
    }
  }
#ifdef LUA_TINT
  else if (tl==LUA_TINT && tr==LUA_TNUMBER) {
# ifdef LNUM_COMPLEX
    if (nvalue_img_fast(r) != 0) return 0;
# endif
    /* Avoid doing accuracy losing cast, if possible. */
    lua_Integer tmp;
    return tt_integer_valued(r,&tmp) ? (ivalue(l) == tmp)
                : luai_numeq( cast_num(ivalue(l)), nvalue_fast(r) );
  }
  else if (tl==LUA_TNUMBER && tr==LUA_TINT) {
    return luaO_rawequalObj(r,l);
  }
#endif
  return 0;
}

static void pushstr (lua_State *L, const char *str) {
  setsvalue2s(L, L->top, luaS_new(L, str));
  incr_top(L);
}


/* this function handles only `%d', `%c', %f, %p, and `%s' formats */
const char *luaO_pushvfstring (lua_State *L, const char *fmt, va_list argp) {
  int n = 1;
  pushstr(L, "");
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    setsvalue2s(L, L->top, luaS_newlstr(L, fmt, e-fmt));
    incr_top(L);
    switch (*(e+1)) {
      case 's': {
        const char *s = va_arg(argp, char *);
        if (s == NULL) s = "(null)";
        pushstr(L, s);
        break;
      }
      case 'c': {
        char buff[2];
        buff[0] = cast(char, 