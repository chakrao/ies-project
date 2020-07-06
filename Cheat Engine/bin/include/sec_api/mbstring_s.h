/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_MBSTRING_S
#define _INC_MBSTRING_S

#include <mbstring.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _MBSTRING_S_DEFINED
#define _MBSTRING_S_DEFINED
  _CRTIMP errno_t __cdecl _mbscat_s(unsigned char *_Dst,size_t _DstSizeInBytes,const unsigned char *_Src);
  _CRTIMP errno_t __cdecl _mbscat_s_l(unsigned char *_Dst,size_t _DstSizeInBytes,const unsigned char *_Src,_locale_t _Locale