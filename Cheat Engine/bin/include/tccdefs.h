/*  tccdefs.h

    Nothing is defined before this file except target machine, target os
    and the few things related to option settings in tccpp.c:tcc_predefs().

    This file is either included at runtime as is, or converted and
    included as C-strings at compile-time (depending on CONFIG_TCC_PREDEFS).

    Note that line indent matters:

    - in lines starting at column 1, platform macros are replaced by
      corresponding TCC target compile-time macros.  See conftest.c for
      the list of platform macros supported in lines starting at column 1.

    - only lines indented >= 4 are actually included into the executable,
      check tccdefs_.h.
*/

#if __SIZEOF_POINTER__ == 4
    /* 32bit systems. */
#if defined TARGETOS_OpenBSD
    #define __SIZE_TYPE__ unsigned long
    #define __PTRDIFF_TYPE__ long
#else
    #define __SIZE_TYPE__ unsigned int
    #define __PTRDIFF_TYPE__ int
#endif
