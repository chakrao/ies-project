/*
** $Id: lctype.c,v 1.12 2014/11/02 19:19:04 roberto Exp $
** 'ctype' functions for Lua
** See Copyright Notice in lua.h
*/

#define lctype_c
#define LUA_CORE

#include "lprefix.h"


#include "lctype.h"

#if !LUA_USE_CTYPE	/* { */

#include <limits.h>

LUAI