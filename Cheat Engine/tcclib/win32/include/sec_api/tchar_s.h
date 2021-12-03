/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_TCHAR_S
#define _INC_TCHAR_S

#include <tchar.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _UNICODE

#define _tprintf_s wprintf_s
#define _tprintf_s_l _wprintf_s_l
#define _tcprintf_s _cwprintf_s
#define _tcprintf_s_l _cwprintf_s_l
#define _vtcprintf_s _vcwprintf_s
#define _vtcprintf_s_l _vcwprintf_s_l
#define _ftprintf_s fwprintf_s
#define _ftprintf_s_l _fwprintf_s_l
#define _stprintf_s swprintf_s
#define _stprintf_s_l _swprintf_s_l
#define _sntprintf_s _snwprintf_s
#define _sntprintf_s_l _snwprintf_s_l
#define _vtprintf_s vwprintf_s
#define _vtprintf_s_l _vwprintf_s_l
#define _vftprintf_s vfwprintf_s
#define _vftprintf_s_l _vfwprintf_s_l
#define _vstprintf_s vswprintf_s
#define _vstprintf_s_l _vswprintf_s_l
#define _vsntprintf_s _vsnwprintf_s
