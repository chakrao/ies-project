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
    if (ta->seti == NULL)