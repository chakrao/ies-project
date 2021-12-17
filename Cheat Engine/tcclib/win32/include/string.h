/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_STRING
#define _INC_STRING

#include <_mingw.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NLSCMP_DEFINED
#define _NLSCMP_DEFINED
#define _NLSCMPERROR 2147483647
#endif

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL ((void *)0)
#endif
#endif

#define _WConst_return _CONST_RETURN

#ifndef _CRT_MEMORY_DEFINED
#define _CRT_MEMORY_DEFINED
  _CRTIMP void *__cdecl _memccpy(void *_Dst,const void *_Src,int _Val,size_t _MaxCount);
  _CONST_RETURN void *__cdecl memchr(const void *_Buf ,int _Val,size_t _MaxCount);
  _CRTIMP int __cdecl _memicmp(const void *_Buf1,const void *_Buf2,size_t _Size);
  _CRTIMP int __cdecl _memicmp_l(const void *_Buf1,const void *_Buf2,size_t _Size,_locale_t _Locale);
  int __cdecl memcmp(const void *_Buf1,const void *_Buf2,size_t _Size);
  void *__cdecl memcpy(void *_Dst,const void *_Src,size_t _Size);
  void *__cdecl memset(void *_Dst,int _Val,size_t _Size);
#ifndef	NO_OLDNAMES
  void *__cdecl memccpy(void *_Dst,const void *_Src,int _Val,size_t _Size);
  int __cdecl memicmp(const void *_Buf1,const void *_Buf2,size_t _Size);
#endif
#endif
  char *__cdecl _strset(char *_Str,int _Val);
  char *__cdecl strcpy(char *_Dest,const char *_Source);
  char *__cdecl strcat(char *_Dest,const char *_Source);
  int __cdecl strcmp(const char *_Str1,const char *_Str2);
  size_t __cdecl strlen(const char *_Str);
#if 0
  size_t __cdecl strnlen(const char *_Str,size_t _MaxCount);
#endif
  void *__cdecl memmove(void *_Dst,const void *_Src,size_t _Size);
  _CRTIMP char *__cdecl _strdup(co