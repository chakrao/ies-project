/*
** $Id: ltable.c,v 2.100 2015/01/05 13:52:37 roberto Exp $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/

#define ltable_c
#define LUA_CORE

#include "lprefix.h"


/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest 'n' such that at
** least half the slots between 0 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the 'original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
*/

#include <float.h>
#include <math.h>
#include <string.h>
#include <limits.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


/*
** Maximum size of array part (MAXASIZE) is 2^MAXABITS. MAXABITS is
** the largest integer such that MAXASIZE fits in an unsigned int.
*/
#define MAXABITS	cast_int(sizeof(int) * CHAR_BIT - 1)
#define MAXASIZE	(1u << MAXABITS)

/*
** Maximum size of hash part is 2^MAXHBITS. MAXHBITS is the largest
** integer such that 2^MAXHBITS fits in a signed int. (Note that the
** maximum number of elements in a table, 2^MAXABITS + 2^MAXHBITS, still
** fits comfortably in an unsigned int.)
*/
#define MAXHBITS	(MAXABITS - 1)


#define hashpow2(t,n)		(gnode(t, lmod((n), sizenode(t))))

#define hashstr(t,str)		hashpow2(t, (str)->hash)
#define hashboolean(t,p)	hashpow2(t, p)
#define hashint(t,i)		hashpow2(t, i)


/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
*/
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashpointer(t,p)	hashmod(t, point2int(p))


#define dummynode		(&dummynode_)

#define isdummy(n)		((n) == dummynode)

static const Node dummynode_ = {
  {NILCONSTANT},  /* value */
  {{NILCONSTANT, 0}}  /* key */
};


/*
** Checks whether a float has a value representable as a lua_Integer
** (and does the conversion if so)
*/
static int numisinteger (lua_Number x, lua_Integer *p) {
  if ((x) == l_floor(x))  /* integral value? */
    return lua_numbertointeger(x, p);  /* try as an integer */
  else return 0;
}


/*
** hash for floating-point numbers
*/
static Node *hashfloat (const Table *t, lua_Number n) {
  int i;
  n = l_mathop(frexp)(n, &i) * cast_num(INT_MAX - DBL_MAX_EXP);
  i += cast_int(n);
  if (i < 0) {
    if (cast(unsigned int, i) == 0u - i)  /* use unsigned to avoid overflows */
      i = 0;  /* handle INT_MIN */
    i = -i;  /* must be a positive value */
  }
  return hashmod(t, i);
}



/*
** returns the 'main' position of an element in a table (that is, the index
** of its hash value)
*/
static Node *mainposition (const Table *t, const TValue *key) {
  switch (ttype(key)) {
    case LUA_TNUMINT:
      return hashint(t, ivalue(key));
    case LUA_TNUMFLT:
      return hashfloat(t, fltvalue(key));
    case LUA_TSHRSTR:
      return hashstr(t, tsvalue(key));
    case LUA_TLNGSTR: {
      TString *s = tsvalue(key);
      if (s->extra == 0) {  /* no hash? */
        s->hash = luaS_hash(getstr(s), s->len, s->hash);
        s->extra = 1;  /* now it has its hash */
      }
      return hashstr(t, tsvalue(key));
    }
    case LUA_TBOOLEAN:
      return hashboolean(t, bvalue(key));
    case LUA_TLIGHTUSERDATA:
      return hashpointer(t, pvalue(key));
    case LUA_TLCF:
      return hashpointer(t, fvalue(key));
    default:
      return hashpointer(t, gcvalue(key));
  }
}


/*
** returns the index for 'key' if 'key' is an appropriate key to live in
** the array part of the table, 0 otherwise.
*/
static unsigned int arrayindex (const TValue *key) {
  if (ttisinteger(key)) {
    lua_Integer k = ivalue(key);
    if (0 < k && (lua_Unsigned)k <= MAXASIZE)
      return cast(unsigned int, k);  /* 'key' is an appropriate array index */
  }
  return 0;  /* 'key' did not match some condition */
}


/*
** returns the index of a 'key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signaled by 0.
*/
static unsigned int findindex (lua_State *L, Table *t, StkId key) {
  unsigned int i;
  if (ttisnil(key)) return 0;  /* first iteration */
  i = arrayindex(key);
  if (i != 0 && i <= t->sizearray)  /* is 'key' inside array part? */
    return i;  /* yes; that's the index */
  else {
    int nx;
    Node *n = mainposition(t, key);
    for (;;) {  /* check whether 'key' is somewhere in the chain */
      /* key may be dead already, but it is ok to use it in 'next' */
      if (luaV_rawequalobj(gkey(n), key) ||
            (ttisdeadkey(gkey(n)) && iscollectable(key) &&
             deadvalue(gkey(n)) == gcvalue(key))) {
        i = cast_int(n - gnode(t, 0));  /* key index in hash table */
        /* hash elements are numbered after array ones */
        return (i + 1) + t->sizearray;
      }
      nx = gnext(n);
      if (nx == 0)
        luaG_runerror(L, "invalid key to 'next'");  /* key not found */
      else n += nx;
    }
  }
}


int luaH_next (lua_State *L, Table *t, StkId key) {
  unsigned int i = findindex(L, t, key);  /* find original element */
  for (; i < t->sizearray; i++) {  /* try first array part */
    if (!ttisnil(&t->array[i])) {  /* a non-nil value? */
      setivalue(key, i + 1);
      setobj2s(L, key+1, &t->array[i]);
      return 1;
    }
  }
  for (i -= t->sizearray; cast_int(i) < sizenode(t); i++) {  /* hash part */
    if (!ttisnil(gval(gnode(t, i)))) {  /* a non-nil value? */
      setobj2s(L, key, gkey(gnode(t, i)));
      setobj2s(L, key+1, gv