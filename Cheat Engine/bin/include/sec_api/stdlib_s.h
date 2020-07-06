/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_STDLIB_S
#define _INC_STDLIB_S

#include <stdlib.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

  _CRTIMP errno_t __cdecl _dupenv_s(char **_PBuffer,size_t *_PBufferSizeInBytes,const char *_VarName);
  _CRTIMP errno_t __cdecl _itoa_s(int _Value,char *_DstBuf,size_t _Size,int _Radix);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP errno_t __cdecl _i64toa_s(__int64 _Val,char *_DstBuf,size_t _Size,int _Radix);
  _CRTIMP errno_t __cdecl _ui64toa_s(unsigned __int64 _Val,char *_DstBuf,size_t _Size,int _Radix);
#endif
  _CRTIMP errno_t __cdecl _ltoa_s(long _Val,char *_DstBuf,size_t _Size,int _Radix);
  _CRTIMP errno_t __cdecl mbstowcs_s(size_t *_PtNumOfCharConverted,wchar_t *_DstBuf,size_t _SizeInWords,const char *_SrcBuf,size_t _MaxCount);
  _CRTIMP errno_t __cdecl _mbstowcs_s_l(size_t *_PtNumOfCharConverted,wchar_t *_DstBuf,size_t _SizeInWords,const char *_SrcBuf,size_t _MaxCount,_locale_t _Locale);
  _CRTIMP errno_t __cdecl _ultoa_s(unsigned long _Val,char *_DstBuf,size_t _Size,int _Radix);
  _CRTIMP errno_t __cdecl _wctomb_s_l(int *_SizeConverted,char *_MbCh,size_t _SizeInBytes,wchar_t _WCh,_locale_t _Locale);
  _CRTIMP errno_t __cdecl wcstombs_s(size_t *_PtNumOfCharConverted,char *_Dst,size_t _DstSizeInBytes,const wchar_t *_Src,size_t _MaxCountInBytes);
  _CRTIMP errno_t __cdecl _wcstombs_s_l(size_t *_PtNumOfCharConverted,char *_Dst,size_t _DstSizeInBytes,const wchar_t *_Src,size_t _MaxC