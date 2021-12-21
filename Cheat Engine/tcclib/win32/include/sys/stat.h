/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_STAT
#define _INC_STAT

#ifndef _WIN32
#error Only Win32 target is supported!
#endif

#include <_mingw.h>
#include <io.h>

#pragma pack(push,_CRT_PACKING)

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _CRTIMP
#define _CRTIMP __declspec(dllimport)
#endif

#include <sys/types.h>

#ifndef __TINYC__ /* gr */
#ifdef _USE_32BIT_TIME_T
#ifdef _WIN64
#undef _USE_32BIT_TIME_T
#endif
#else
#if _INTEGRAL_MAX_BITS < 64
#define _USE_32BIT_TIME_T
#endif
#endif
#endif

#ifndef _TIME32_T_DEFINED
  typedef long __time32_t;
#define _TIME32_T_DEFINED
#endif

#ifndef _TIME64_T_DEFINED
#if _INTEGRAL_MAX_BITS >= 64
  typedef __int64 __time64_t;
#endif
#define _TIME64_T_DEFINED
#endif

#ifndef _TIME_T_DEFINED
#ifdef _USE_32BIT_TIME_T
  typedef __time32_t time_t;
#else
  typedef __time64_t time_t;
#endif
#define _TIME_T_DEFINED
#endif

#ifndef _WCHAR_T_DEFINED
  typedef unsigned short wchar_t;
#define _WCHAR_T_DEFINED
#endi