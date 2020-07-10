/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_WCHAR
#define _INC_WCHAR

#include <_mingw.h>

#pragma pack(push,_CRT_PACKING)

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WCHAR_MIN  /* also at stdint.h */
#define WCHAR_MIN 0
#define WCHAR_MAX ((wchar_t) -1) /* UINT16_MAX */
#endif

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
  typedef __builtin_va_list __gnuc_va_list;
#endif

#ifndef _VA_LIST_DEFINED
#define _VA_LIST_DEFINED
  typedef __gnuc_va_list va_list;
#endif

#ifndef WEOF
#define WEOF (wint_t)(0xFFFF)
#endif

#ifndef _FILE_DEFINED
  struct _iobuf {
    char *_ptr;
    int _cnt;
    char *_base;
    int _flag;
    int _file;
    int _charbuf;
    int _bufsiz;
    char *_tmpfname;
  };
  typedef struct _iobuf FILE;
#define _FILE_DEFINED
#endif

#ifndef _STDIO_DEFINED
#ifdef _WIN64
  _CRTIMP FILE *__cdecl __iob_func(void);
#else
#ifdef _MSVCRT_
extern FILE _iob[];	/* A pointer to an array of FILE */
#define __iob_func()	(_iob)
#else
extern FILE (*_imp___iob)[];	/* A pointer to an array of FILE */
#define __iob_func()	(*_imp___iob)
#define _iob __iob_func()
#endif
#endif

#define _iob __iob_func()
#endif

#ifndef _STDSTREAM_DEFINED
#define stdin (&__iob_func()[0])
#define stdout (&__iob_func()[1])
#define stderr (&__iob_func()[2])
#define _STDSTREAM_DEFINED
#endif

#ifndef _FSIZE_T_DEFINED
  typedef unsigned long _fsize_t;
#define _FSIZE_T_DEFINED
#endif

#ifndef _WFINDDATA_T_DEFINED
  struct _wfinddata32_t {
    unsigned attrib;
    __time32_t time_create;
    __time32_t time_access;
    __time32_t time_write;
    _fsize_t size;
    wchar_t name[260];
  };

/* #if _INTEGRAL_MAX_BITS >= 64 */

  struct _wfinddata32i64_t {
    unsigned attrib;
    __time32_t time_create;
    __time32_t time_access;
    __time32_t time_write;
    __int64 size;
    wchar_t name[260];
  };

  struct _wfinddata64i32_t {
    unsigned attrib;
    __time64_t time_create;
    __time64_t time_access;
    __time64_t time_write;
    _fsize_t size;
    wchar_t name[260];
  };

  struct _wfinddata64_t {
    unsigned attrib;
    __time64_t time_create;
    __time64_t time_access;
    __time64_t time_write;
    __int64 size;
    wchar_t name[260];
  };
/* #endif */

#ifdef _USE_32BIT_TIME_T
#define _wfinddata_t _wfinddata32_t
#define _wfinddatai64_t _wfinddata32i64_t

#define _wfindfirst _wfindfirst32
#define _wfindnext _wfindnext32
#define _wfindfirsti64 _wfindfirst32i64
#define _wfindnexti64 _wfindnext32i64
#else
#define _wfinddata_t _wfinddata64i32_t
#define _wfinddatai64_t _wfinddata64_t

#define _wfindfirst _wfindfirst64i32
#define _wfindnext _wfindnext64i32
#define _wfindfirsti64 _wfindfirst64
#define _wfindnexti64 _wfindnext64
#endif

#define _WFINDDATA_T_DEFINED
#endif

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL ((void *)0)
#endif
#endif

#ifndef _CONST_RETURN
#define _CONST_RETURN
#endif

#define _WConst_return _CONST_RETURN

#ifndef _CRT_CTYPEDATA_DEFINED
#define _CRT_CTYPEDATA_DEFINED
#ifndef _CTYPE_DISABLE_MACROS

#ifndef __PCTYPE_FUNC
#define __PCTYPE_FUNC __pctype_func()
#ifdef _MSVCRT_
#define __pctype_func() (_pctype)
#else
#define __pctype_func() (*_imp___pctype)
#endif
#endif

#ifndef _pctype
#ifdef _MSVCRT_
  extern unsigned short *_pctype;
#else
  extern unsigned short **_imp___pctype;
#define _pctype (*_imp___pctype)
#endif
#endif
#endif
#endif

#ifndef _CRT_WCTYPEDATA_DEFINED
#define _CRT_WCTYPEDATA_DEFINED
#ifndef _CTYPE_DISABLE_MACROS
#ifndef _wctype
#ifdef _MSVCRT_
  extern unsigned short *_wctype;
#else
  extern unsigned short **_imp___wctype;
#define _wctype (*_imp___wctype)
#endif
#endif

#ifdef _MSVCRT_
#define __pwctype_func() (_pwctype)
#else
#define __pwctype_func() (*_imp___pwctype)
#endif

#ifndef _pwctype
#ifdef _MSVCRT_
  extern unsigned short *_pwctype;
#else
  extern unsigned short **_imp___pwctype;
#define _pwctype (*_imp___pwctype)
#endif
#endif

#endif
#endif

#define _UPPER 0x1
#define _LOWER 0x2
#define _DIGIT 0x4
#define _SPACE 0x8

#define _PUNCT 0x10
#define _CONTROL 0x20
#define _BLANK 0x40
#define _HEX 0x80

#define _LEADBYTE 0x8000
#define _ALPHA (0x0100|_UPPER|_LOWER)

#ifndef _WCTYPE_DEFINED
#define _WCTYPE_DEFINED

  int __cdecl iswalpha(wint_t _C);
  _CRTIMP int __cdecl _iswalpha_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswupper(wint_t _C);
  _CRTIMP int __cdecl _iswupper_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswlower(wint_t _C);
  _CRTIMP int __cdecl _iswlower_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswdigit(wint_t _C);
  _CRTIMP int __cdecl _iswdigit_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswxdigit(wint_t _C);
  _CRTIMP int __cdecl _iswxdigit_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswspace(wint_t _C);
  _CRTIMP int __cdecl _iswspace_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswpunct(wint_t _C);
  _CRTIMP int __cdecl _iswpunct_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswalnum(wint_t _C);
  _CRTIMP int __cdecl _iswalnum_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswprint(wint_t _C);
  _CRTIMP int __cdecl _iswprint_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswgraph(wint_t _C);
  _CRTIMP int __cdecl _iswgraph_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswcntrl(wint_t _C);
  _CRTIMP int __cdecl _iswcntrl_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswascii(wint_t _C);
  int __cdecl isleadbyte(int _C);
  _CRTIMP int __cdecl _isleadbyte_l(int _C,_locale_t _Locale);
  wint_t __cdecl towupper(wint_t _C);
  _CRTIMP wint_t __cdecl _towupper_l(wint_t _C,_locale_t _Locale);
  wint_t __cdecl towlower(wint_t _C);
  _CRTIMP wint_t __cdecl _towlower_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswctype(wint_t _C,wctype_t _Type);
  _CRTIMP int __cdecl _iswctype_l(wint_t _C,wctype_t _Type,_locale_t _Locale);
  _CRTIMP int __cdecl __iswcsymf(wint_t _C);
  _CRTIMP int __cdecl _iswcsymf_l(wint_t _C,_locale_t _Locale);
  _CRTIMP int __cdecl __iswcsym(wint_t _C);
  _CRTIMP int __cdecl _iswcsym_l(wint_t _C,_locale_t _Locale);
  int __cdecl is_wctype(wint_t _C,wctype_t _Type);
#endif

#ifndef _WDIRECT_DEFINED
#define _WDIRECT_DEFINED

  _CRTIMP wchar_t *__cdecl _wgetcwd(wchar_t *_DstBuf,int _SizeInWords);
  _CRTIMP wchar_t *__cdecl _wgetdcwd(int _Drive,wchar_t *_DstBuf,int _SizeInWords);
  wchar_t *__cdecl _wgetdcwd_nolock(int _Drive,wchar_t *_DstBuf,int _SizeInWords);
  _CRTIMP int __cdecl _wchdir(const wchar_t *_Path);
  _CRTIMP int __cdecl _wmkdir(const wchar_t *_Path);
  _CRTIMP int __cdecl _wrmdir(const wchar_t *_Path);
#endif

#ifndef _WIO_DEFINED
#define _WIO_DEFINED

  _CRTIMP int __cdecl _waccess(const wchar_t *_Filename,int _AccessMode);
  _CRTIMP int __cdecl _wchmod(const wchar_t *_Filename,int _Mode);
  _CRTIMP int __cdecl _wcreat(const wchar_t *_Filename,int _PermissionMode);
  _CRTIMP intptr_t __cdecl _wfindfirst32(const wchar_t *_Filename,struct _wfinddata32_t *_FindData);
  _CRTIMP int __cdecl _wfindnext32(intptr_t _FindHandle,struct _wfinddata32_t *_FindData);
  _CRTIMP int __cdecl _wunlink(const wchar_t *_Filename);
  _CRTIMP int __cdecl _wrename(const wchar_t *_NewFilename,const wchar_t *_OldFilename);
  _CRTIMP wchar_t *__cdecl _wmktemp(wchar_t *_TemplateName);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP intptr_t __cdecl _wfindfirst32i64(const wchar_t *_Filename,struct _wfinddata32i64_t *_FindData);
  intptr_t __cdecl _wfindfirst64i32(const wchar_t *_Filename,struct _wfinddata64i32_t *_FindData);
  _CRTIMP intptr_t __cdecl _wfindfirst64(const wchar_t *_Filename,struct _wfinddata64_t *_FindData);
  _CRTIMP int __cdecl _wfindnext32i64(intptr_t _FindHandle,struct _wfinddata32i64_t *_FindData);
  int __cdecl _wfindnext64i32(intptr_t _FindHandle,struct _wfinddata64i32_t *_FindData);
  _CRTIMP int __cdecl _wfindnext64(intptr_t _FindHandle,struct _wfinddata64_t *_FindData);
#endif
  _CRTIMP errno_t __cdecl _wsopen_s(int *_FileHandle,const wchar_t *_Filename,int _OpenFlag,int _ShareFlag,int _PermissionFlag);
#if !defined(__cplusplus) || !(defined(_X86_) && !defined(__x86_64))
  _CRTIMP int __cdecl _wopen(const wchar_t *_Filename,int _Ope