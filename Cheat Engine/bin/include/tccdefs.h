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
    #define __ILP32__ 1
    #define __INT64_TYPE__ long long
#elif __SIZEOF_LONG__ == 4
    /* 64bit Windows. */
    #define __SIZE_TYPE__ unsigned long long
    #define __PTRDIFF_TYPE__ long long
    #define __LLP64__ 1
    #define __INT64_TYPE__ long long
#else
    /* Other 64bit systems. */
    #define __SIZE_TYPE__ unsigned long
    #define __PTRDIFF_TYPE__ long
    #define __LP64__ 1
# if defined __linux__
    #define __INT64_TYPE__ long
# else /* APPLE, BSD */
    #define __INT64_TYPE__ long long
# endif
#endif
    #define __SIZEOF_INT__ 4
    #define __INT_MAX__ 0x7fffffff
#if __SIZEOF_LONG__ == 4
    #define __LONG_MAX__ 0x7fffffffL
#else
    #define __LONG_MAX__ 0x7fffffffffffffffL
#endif
    #define __SIZEOF_LONG_LONG__ 8
    #define __LONG_LONG_MAX__ 0x7fffffffffffffffLL
    #define __CHAR_BIT__ 8
    #define __ORDER_LITTLE_ENDIAN__ 1234
    #define __ORDER_BIG_ENDIAN__ 4321
    #define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#if defined _WIN32
    #define __WCHAR_TYPE__ unsigned short
    #define __WINT_TYPE__ unsigned short
#elif defined __linux__
    #define __WCHAR_TYPE__ int
    #define __WINT_TYPE__ unsigned int
#else
    #define __WCHAR_TYPE__ int
    #define __WINT_TYPE__ int
#endif

    #if __STDC_VERSION__ == 201112L
    # define __STDC_NO_ATOMICS__ 1
    # define __STDC_NO_COMPLEX__ 1
    # define __STDC_NO_THREADS__ 1
#if !defined _WIN32
    # define __STDC_UTF_16__ 1
    # define __STDC_UTF_32__ 1
#endif
    #endif

#if defined _WIN32
    #define __declspec(x) __attribute__((x))
    #define __cdecl

#elif defined __FreeBSD__
    #define __GNUC__ 9
    #define __GNUC_MINOR__ 3
    #define __GNUC_PATCHLEVEL__ 0
    #define __GNUC_STDC_INLINE__ 1
    #define __NO_TLS 1
# if __SIZEOF_POINTER__ == 8
    /* FIXME, __int128_t is used by setjump */
    #define __int128_t struct { unsigned char _dummy[16] __attribute((aligned(16))); }
# endif

#elif defined __FreeBSD_kernel__

#elif defined __NetBSD__
    #define __GNUC__ 4
    #define __GNUC_MINOR__ 1
    #define __GNUC_PATCHLEVEL__ 0
    #define _Pragma(x)
    #define __ELF__ 1
#if defined __aarch64__
    #define _LOCORE /* avoids usage of __asm */
#endif

#elif defined __OpenBSD__
    #define __GNUC__ 4
    #define _ANSI_LIBRARY 1

#elif defined __APPLE__
    /* emulate APPLE-GCC to make libc's headerfiles compile: */
    #define __GNUC__ 4   /* darwin emits warning on GCC<4 */
    #define __APPLE_CC__ 1 /* for <TargetConditionals.h> */
    #define _DONT_USE_CTYPE_INLINE_ 1
    /* avoids usage of GCC/clang specific builtins in libc-headerfiles: */
    #define __FINITE_MATH_ONLY__ 1
    #define _FORTIFY_SOURCE 0

#else
    /* Linux */

#endif

#if !defined _WIN32
    /* glibc defines */
    #define __REDIRECT(name, proto, alias) name proto __asm__ (#alias)
    #define __REDIRECT_NTH(name, proto, alias) name proto __asm__ (#alias) __THROW
#else
  //Cheat Engine modification start (all externs are handled as __declspec(dllimport) )
  #define extern __declspec(dllimport) extern 
  #define cheatengine
  //Cheat Engine modification end
#endif

    /* skip __builtin... with -E */
    #ifndef __TCC_PP__

    #define __builtin_offsetof(type, field) ((__SIZE_TYPE__)&((type*)0)->field)
    #define __builtin_extract_return_addr(x) x
#if !defined __linux__ && !defined _WIN32
    /* used by math.h */
    #define __builtin_huge_val() 1e500
    #define __builtin_huge_valf() 1e50f
    #define __builtin_huge_vall() 1e5000L
# if defined __APPLE__
    #define __builtin_nanf(ignored_string) __nan()
    /* used by floats.h to implement FLT_ROUNDS C99 macro. 1 == to nearest */
    #define __builtin_flt_rounds() 1
    /* used by _fd_def.h */
    #define __builtin_bzero(p, ignored_size) bzero(p, sizeof(*(p)))
# else
    #define __builtin_nanf(ignored_string) (0.0F/0.0F)
# endif
#endif

    /* __builtin_va_list */
#if defined __x86_64__
#if !defined _WIN32
    /* GCC compatible definition of va_list. */
    /* This should be in sync with the declaration in our lib/libtcc1.c */
    typedef struct {
        unsigned gp_offset, fp_offset;
        union {
            unsigned overflow_offset;
            char *overflow_arg_area;
        };
        char *reg_save_area;
    } __builtin_va_list[1];

    void *__va_arg(__builtin_va_list ap, int arg_type, int size, int align);
    #define __builtin_va_start(ap, last) \
       (*(ap) = *(__builtin_va_list)((char*)__builtin_frame_address(0) - 24))
    #define __builtin_va_arg(ap, t)   \
       (*(t *)(__va_arg(ap, __builtin_va_arg_types(t), sizeof(t), __alignof__(t))))
    #define __builtin_va_copy(dest, src) (*(dest) = *(src))

#else /* _WIN64 *