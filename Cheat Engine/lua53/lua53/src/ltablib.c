/*
** $Id: ltablib.c,v 1.79 2014/11/02 19:19:04 roberto Exp $
** Library for Table Manipulation
** See Copyright Notice in lua.h
*/

#define ltablib_c
#define LUA_LIB

#include "lprefix.h"


#include <limits.h>
#include <stddef.h>

#include "lua.h"

#include "lauxlib.h"
#include "lualib.h"



/*
** Structure with table-access functions
*/
typedef struct {
  int (*geti) (lua_State *L, int idx, lua_Integer n);
  void (*seti) (lua_State *L, int idx, lua_Integer n);
} TabA;


/*
** Check that 'arg' has a table and set access functions in 'ta' to raw
** or non-raw according to the presence of corresponding metamethods.
*/
static void checktab (lua_State *L, int arg, TabA *ta) {
  ta->geti = NULL; ta->seti = NULL;
  if (lua_getmetatable(L, arg)) {
    lua_pushliteral(L, "__index");  /* 'index' metamethod */
    if (lua_rawget(L, -2) != LUA_TNIL)
      ta->geti = lua_geti;
    lua_pushliteral(L, "__newindex");  /* 'newindex' metamethod */
    if (lua_rawget(L, -3) != LUA_TNIL)
      ta->seti = lua_seti;
    lua_pop(L, 3);  /* pop metatable plus both metamethods */
  }
  if (ta->geti == NULL || ta->seti == NULL) {
    luaL_checktype(L, arg, LUA_TTABLE);  /* must be table for raw methods */
    if (ta->geti == NULL) ta->geti = lua_rawgeti;
    if (ta->seti == NULL) ta->seti = lua_rawseti;
  }
}


#define aux_getn(L,n,ta)	(checktab(L, n, ta), luaL_len(L, n))


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
  TabA ta;
  lua_Integer e = aux_getn(L, 1, &ta) + 1;  /* first empty element */
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
        (*ta.geti)(L, 1, i - 1);
        (*ta.seti)(L, 1, i);  /* t[i] = t[i - 1] */
      }
      break;
    }
    default: {
      return luaL_error(L, "wrong number of arguments to 'insert'");
    }
  }
  (*ta.seti)(L, 1, pos);  /* t[pos] = v */
  return 0;
}


static int tremove (lua_State *L) {
  TabA ta;
  lua_Integer size = aux_getn(L, 1, &ta);
  lua_Integer pos = luaL_optinteger(L, 2, size);
  if (pos != size)  /* validate 'pos' if given */
    luaL_argcheck(L, 1 <= pos && pos <= size + 1, 1, "position out of bounds");
  (*ta.geti)(L, 1, pos);  /* result = t[pos] */
  for ( ; pos < size; pos++) {
    (*ta.geti)(L, 1, pos + 1);
    (*ta.seti)(L, 1, pos);  /* t[pos] = t[pos + 1] */
  }
  lua_pushnil(L);
  (*ta.seti)(L, 1, pos);  /* t[pos] = nil */
  return 1;
}


static int tmove (lua_State *L) {
  TabA ta;
  lua_Integer f = luaL_checkinteger(L, 2);
  lua_Integer e = luaL_checkinteger(L, 3);
  lua_Integer t = luaL_checkinteger(L, 4);
  int tt = !lua_isnoneornil(L, 5) ? 5 : 1;  /* destination table */
  /* the following restriction avoids several problems with overflows */
  luaL_argcheck(L, f > 0, 2, "initial position must be positive");
  if (e >= f) {  /* otherwise, nothing to move */
    lua_Integer n, i;
    ta.geti = (luaL_getmetafield(L, 1, "__index") == LUA_TNIL)
      ? (luaL_checktype(L, 1, LUA_TTABLE), lua_rawgeti)
      : lua_geti;
    ta.seti = (luaL_getmetafield(L, tt, "__newindex") == LUA_TNIL)
      ? (luaL_checktype(L, tt, LUA_TTABLE), lua_rawseti)
      : lua_seti;
    n = e - f + 1;  /* number of elements to move */
    if (t > f) {
      for (i = n - 1; i >= 0; i--) {
        (*ta.geti)(L, 1, f + i);
        (*ta.seti)(L, tt, t + i);
      }
    }
    else {
      for (i = 0; i < n; i++) {
        (*ta.geti)(L, 1, f + i);
        (*ta.seti)(L, tt, t + i);
      }
    }
  }
  lua_pushvalue(L, tt);  /* return "to table" */
  return 1;
}


static void addfield (lua_State *L, luaL_Buffer *b, TabA *ta, lua_Integer i) {
  (*ta->geti)(L, 1, i);
  if (!lua_isstring(L, -1))
    luaL_error(L, "invalid value (%s) at index %d in table for 'concat'",
                  luaL_typename(L, -1), i);
  luaL_addvalue(b);
}


static int tconcat (lua_State *L) {
  TabA ta;
  luaL_Buffer b;
  size_t lsep;
  lua_Integer i, last;
  const char *sep = luaL_optlstring(L, 2, "", &lsep);
  checktab(L, 1, &ta);
  i = luaL_optinteger(L, 3, 1);
  last = luaL_opt(L, luaL_checkinteger, 4, luaL_len(L, 1));
  luaL_buffinit(L, &b);
  for (; i < last; i++) {
    addfield(L, &b, &ta, i);
    luaL_addlstring(&b, sep, lsep);
  }
  if (i == last)  /* add last value (if interval was not empty) */
    addfield(L, &b, &ta, i);
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
    lua_rawseti(L, 1, i);
  lua_pushinteger(L, n);
  lua_setfield(L, 1, "n");  /* t.n = number of elements */
  return 1;  /* return table */
}


static int unpack (lua_State *L) {
  TabA ta;
  lua_Integer i, e;
  lua_Unsigned n;
  checktab(L, 1, &ta);
  i = luaL_optinteger(L, 2, 1);
  e = luaL_opt(L, luaL_checkinteger, 3, luaL_len(L, 1));
  if (i > e) return 0;  /* empty range */
  n = (lua_Unsigned)e - i;  /* number of elements minus 1 (avoid overflows) */
  if (n >= (unsigned int)INT_MAX  || !lua_checkstack(L, (int)(++n)))
    return luaL_error(L, "too many results to unpack");
  do {  /* must have at least