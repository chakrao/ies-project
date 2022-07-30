/*
** $Id: ltablib.c,v 1.93.1.1 2017/04/19 17:20:42 roberto Exp $
** Library for Table Manipulation
** See Copyright Notice in lua.h
*/

#define ltablib_c
#define LUA_LIB

#include "lprefix.h"


#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"


/*
** Operations that an object must define to mimic a table
** (some functions only need some of them)
*/
#define TAB_R	1			/* read */
#define TAB_W	2			/* write */
#define TAB_L	4			/* length */
#define TAB_RW	(TAB_R | TAB_W)		/* read/write */


#define aux_getn(L,n,w)	(checktab(L, n, (w) | TAB_L), luaL_len(L, n))


static int checkfield (lua_State *L, const char *key, int n) {
  lua_pushstring(L, key);
  return (lua_rawget(L, -n) != LUA_TNIL);
}


/*
** Check that 'arg' either is a table or can behave like one (that is,
** has a metatable with the required metamethods)
*/
static void checktab (lua_State *L, int arg, int what) {
  if (lua_type(L, arg) != LUA_TTABLE) {  /* is it not a table? */
    int n = 1;  /* number of elements to pop */
    if (lua_getmetatable(L, arg) &&  /* must have metatable */
        (!(what & TAB_R) || checkfield(L, "__index", ++n)) &&
        (!(what & TAB_W) || checkfield(L, "__newindex", ++n)) &&
        (!(what & TAB_L) || checkfield(L, "__len", ++n))) {
      lua_pop(L, n);  /* pop metatable and tested metamethods */
    }
    else
      luaL_checktype(L, arg, LUA_TTABLE);  /* force an error */
  }
}


#if defined(LUA_COMPAT_MAXN)
static int maxn (lua_State *L) {
  lua_Number max = 0;
  luaL_checktype(L, 1, LUA_TTABLE);
  lua_pushnil(L);  /* first key */
  while (lua_next(L, 1)) {
    lua_pop(L, 1);  /* remove value */
    if (lua_type(L, -1) == LUA_TNUMBER) {
      lua_Number v = lua_tonumber(L, -1);
      if (v > max) max = v;
    }
  }
  lua_pushnumber(L, max);
  return 1;
}
#endif


static int tinsert (lua_State *L) {
  lua_Integer e = aux_getn(L, 1, TAB_RW) + 1;  /* first empty element */
  lua_Integer pos;  /* where to insert new element */
  switch (lua_gettop(L)) {
    case 2: {  /* called with only 2 arguments */
      pos = e;  /* insert new element at the end */
      break;
    }
    case 3: {
      lua_Integer i;
      pos = luaL_checkinteger(L, 2);  /* 2nd argument is the position */
      luaL_argcheck(L, 1 <= pos && pos <= e, 2, "position out of bounds");
      for (i = e; i > pos; i--) {  /* move up elements */
        lua_geti(L, 1, i - 1);
        lua_seti(L, 1, i);  /* t[i] = t[i - 1] */
      }
      break;
    }
    default: {
      return luaL_error(L, "wrong number of arguments to 'insert'");
    }
  }
  lua_seti(L, 1, pos);  /* t[pos] = v */
  return 0;
}


static int tremove (lua_State *L) {
  lua_Integer size = aux_getn(L, 1, TAB_RW);
  lua_Integer pos = luaL_optinteger(L, 2, size);
  if (pos != size)  /* validate 'pos' if given */
    luaL_argcheck(L, 1 <= pos && pos <= size + 1, 1, "position out of bounds");
  lua_geti(L, 1, pos);  /* result = t[pos] */
  for ( ; pos < size; pos++) {
    lua_geti(L, 1, pos + 1);
    lua_seti(L, 1, pos);  /* t[pos] = t[pos + 1] */
  }
  lua_pushnil(L);
  lua_seti(L, 1, pos);  /* t[pos] = nil */
  return 1;
}


/*
** Copy elements (1[f], ..., 1[e]) into (tt[t], tt[t+1], ...). Whenever
** possible, copy in increasing order, which is better for rehashing.
** "possible" means destination after original range, or smaller
** than origin, or copying to another table.
*/
static int tmove (lua_State *L) {
  lua_Integer f = luaL_checkinteger(L, 2);
  lua_Integer e = luaL_checkinteger(L, 3);
  lua_Integer t = luaL_checkinteger(L, 4);
  int tt = !lua_isnoneornil(L, 5) ? 5 : 1;  /* destination table */
  checktab(L, 1, TAB_R);
  checktab(L, tt, TAB_W);
  if (e >= f) {  /* otherwise, nothing to move */
    lua_Integer n, i;
    luaL_argcheck(L, f > 0 || e < LUA_MAXINTEGER + f, 3,
                  "too many elements to move");
    n = e - f + 1;  /* number of elements to move */
    luaL_argcheck(L, t <= LUA_MAXINTEGER - n + 1, 4,
                  "destination wrap around");
    if (t > e || t <= f || (tt != 1 && !lua_compare(L, 1, tt, LUA_OPEQ))) {
      for (i = 0; i < n; i++) {
        lua_geti(L, 1, f + i);
        lua_seti(L, tt, t + i);
      }
    }
    else {
      for (i = n - 1; i >= 0; i--) {
        lua_geti(L, 1, f + i);
        lua_seti(L, tt, t + i);
      }
    }
  }
  lua_pushvalue(L, tt);  /* return destination table */
  return 1;
}


static void addfield (lua_State *L, luaL_Buffer *b, lua_Integer i) {
  lua_geti(L, 1, i);
  if (!lua_isstring(L, -1))
    luaL_error(L, "invalid value (%s) at index %d in table for 'concat'",
                  luaL_typename(L, -1), i);
  luaL_addvalue(b);
}


static int tconcat (lua_State *L) {
  luaL_Buffer b;
  lua_Integer last = aux_getn(L, 1, TAB_R);
  size_t lsep;
  const char *sep = luaL_optlstring(L, 2, "", &lsep);
  lua_Integer i = luaL_optinteger(L, 3, 1);
  last = luaL_optinteger(L, 4, last);
  luaL_buffinit(L, &b);
  for (; i < last; i++) {
    addfield(L, &b, i);
    luaL_addlstring(&b, sep, lsep);
  }
  if (i == last)  /* add last value (if interval was not empty) */
    addfield(L, &b, i);
  luaL_pushresult(&b);
  return 1;
}


/*
** {======================================================
** Pack/unpack
** =======================================================
*/

static int pack (lua_State *L) {
  int i;
  int n = lua_gettop(L);  /* number of elements to pack */
  lua_createtable(L, n, 1);  /* create result table */
  lua_insert(L, 1);  /* put it at index 1 */
  for (i = n; i >= 1; i--)  /* assign elements */
    lua_seti(L, 1, i);
  lua_pushinteger(L, n);
  lua_setfield(L, 1, "n");  /* t.n = number of elements */
  return 1;  /* return table */
}


static int unpack (lua_State *L) {
  lua_Unsigned n;
  lua_Integer i = luaL_optinteger(L, 2, 1);
  lua_Integer e = luaL_opt(L, luaL_checkinteger, 3, luaL_len(L, 1));
  if (i > e) return 0;  /* empty range */
  n = (lua_Unsigned)e - i;  /* number of elements minus 1 (avoid overflows) */
  if (n >= (unsigned int)INT_MAX  || !lua_checkstack(L, (int)(++n)))
    return luaL_error(L, "too many results to unpack");
  for (; i < e; i++) {  /* push arg[i..e - 1] (to avoid overflows) */
    lua_geti(L, 1, i);
  }
  lua_geti(L, 1, e);  /* push last element */
  return (int)n;
}

/* }====================================================== */



/*
** {======================================================
** Quicksort
** (based on 'Algorithms in MODULA-3', Robert Sedgewick;
**  Addison-Wesley, 1993.)
** =======================================================
*/


/* type for array indices */
typedef unsigned int IdxT;


/*
** Produce a "random" 'unsigned int' to randomize pivot choice. This
** macro is used only when 'sort' detects a big imbalance in the result
** of a partition. (If you don't want/need this "randomness", ~0 is a
** good choice.)
*/
#if !defined(l_randomizePivot)		/* { */

#include <time.h>

/* size of 'e' measured in number of 'unsigned int's */
#define sof(e)		(sizeof(e) / sizeof(unsigned int))

/*
** Use 'time' and 'clock' as sources of "randomness". Because we don't
** know the types 'clock_t' and 'time_t', we cannot cast them to
** anything without risking overflows. A safe way to use their values
** is to copy them to an array of a known type and use the array values.
*/
static unsigned int l_randomizePivot (void) {
  clock_t c = clock();
  time_t t = time(NULL);
  unsigned int buff[sof(c) + sof(t)];
  unsigned int i, rnd = 0;
  memcpy(buff, &c, sof(c) * sizeof(unsigned int));
  memcpy(buff + sof(c), &t, sof(t) * sizeof(unsigned int));
  for (i = 0; i < sof(buff); i++)
    rnd += buff[i];
  return rnd;
}

#endif					/* } */


/* arrays larger than 'RANLIMIT' may use randomized pivots */
#define RANLIMIT	100u


static void set2 (lua_State *L, IdxT i, IdxT j) {
  lua_seti(L, 1, i);
  lua_seti(L, 1, j);
}


/*
** Return true iff value at stack index 'a' is less than the value at
** index 'b' (according to the order of the sort).
*/
static int sort_comp (lua_State *L, int a, int b) {
  if (lua_isnil(L, 2))  /* no function? */
    return lua_compare(L, a, b, LUA_OPLT);  /* a < b */
  else {  /* function */
    int r