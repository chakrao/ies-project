/*
** $Id: lvm.h,v 2.5.1.1 2007/12/27 13:02:25 roberto Exp $
** Lua virtual machine
** See Copyright Notice in lua.h
*/

#ifndef lvm_h
#define lvm_h


#include "ldo.h"
#include "lobject.h"
#include "ltm.h"


#define tostring(L,o) ((ttype(o) == LUA_TSTRING) || (luaV_tostring(L, o)))

#define tonumber(o,n) (ttisnumber(o) || (((o) = luaV_tonumber(o,n)) != NULL))

#define equalobj(L,o1,o2) ((ttype_ext(o1)==ttype_ext(o2)) && lua