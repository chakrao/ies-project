
/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _IO_H_
#define _IO_H_

#include <_mingw.h>
#include <string.h>

#pragma pack(push,_CRT_PACKING)

#ifndef _POSIX_

#ifdef __cplusplus
extern "C" {
#endif

_CRTIMP char* __cdecl _getcwd (char*, int);
#ifndef _FSIZE_T_DEFINED
  typedef unsigned long _fsize_t;
#define _FSIZE_T_DEFINED
#endif

#ifndef _FINDDATA_T_DEFINED

  struct _finddata32_t {
    unsigned attrib;
    __time32_t time_create;
    __time32_t time_access;
    __time32_t time_write;
    _fsize_t size;
    char name[260];
  };

/*#if _INTEGRAL_MAX_BITS >= 64*/

  struct _finddata32i64_t {
    unsigned attrib;
    __time32_t time_create;
    __time32_t time_access;
    __time32_t time_write;
    __int64 size;
    char name[260];
  };

  struct _finddata64i32_t {
    unsigned attrib;
    __time64_t time_create;
    __time64_t time_access;
    __time64_t time_write;
    _fsize_t size;
    char name[260];
  };

  struct __finddata64_t {
    unsigned attrib;
    __time64_t time_create;
    __time64_t time_access;
    __time64_t time_write;
    __int64 size;
    char name[260];
  };
/* #endif */

#ifdef _USE_32BIT_TIME_T
#define _finddata_t _finddata32_t
#define _finddatai64_t _finddata32i64_t

#ifdef _WIN64
#define _findfirst _findfirst32
#define _findnext _findnext32
#else
#define _findfirst32 _findfirst
#define _findnext32 _findnext
#endif
#define _findfirsti64 _findfirst32i64
#define _findnexti64 _findnext32i64
#else
#define _finddata_t _finddata64i32_t
#define _finddatai64_t __finddata64_t

#define _findfirst _findfirst64i32
#define _findnext _findnext64i32
#define _findfirsti64 _findfirst64
#define _findnexti64 _findnext64
#endif

#define _FINDDATA_T_DEFINED
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

#define _A_NORMAL 0x00
#define _A_RDONLY 0x01
#define _A_HIDDEN 0x02
#define _A_SYSTEM 0x04
#define _A_SUBDIR 0x10
#define _A_ARCH 0x20

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
#undef size_t
#ifdef _WIN64
#if defined(__GNUC__) && defined(__STRICT_ANSI__)
  typedef unsigned int size_t __attribute__ ((mode (DI)));
#else
  typedef unsigned __int64 size_t;
#endif
#else
  typedef unsigned int size_t;
#endif
#endif

#ifndef _SSIZE_T_DEFINED
#define _SSIZE_T_DEFINED
#undef ssize_t
#ifdef _WIN64
#if defined(__GNUC__) && defined(__STRICT_ANSI__)
  typedef int ssize_t __attribute__ ((mode (DI)));
#else
  typedef __int64 ssize_t;
#endif
#else
  typedef int ssize_t;
#endif
#endif

#ifndef _OFF_T_DEFINED
#define _OFF_T_DEFINED
#ifndef _OFF_T_
#define _OFF_T_
  typedef long _off_t;
#if !defined(NO_OLDNAMES) || defined(_POSIX)
  typedef long off_t;
#endif
#endif
#endif

#ifndef _OFF64_T_DEFINED
#define _OFF64_T_DEFINED
#if defined(__GNUC__) && defined(__STRICT_ANSI__)
  typedef int _off64_t __attribute__ ((mode (DI)));
#if !defined(NO_OLDNAMES) || defined(_POSIX)
  typedef int off64_t __attribute__ ((mode (DI)));
#endif
#else
  typedef long long _off64_t;
#if !defined(NO_OLDNAMES) || defined(_POSIX)
  typedef long long off64_t;
#endif
#endif
#endif

  /* Some defines for _access nAccessMode (MS doesn't define them, but
  * it doesn't seem to hurt to add them). */
#define	F_OK	0	/* Check for file existence */
#define	X_OK	1	/* Check for execute permission. */
#define	W_OK	2	/* Check for write permission */
#define	R_OK	4	/* Check for read permission */

  _CRTIMP int __cdecl _access(const char *_Filename,int _AccessMode);
  _CRTIMP int __cdecl _chmod(const char *_Filename,int _Mode);
  _CRTIMP int __cdecl _chsize(int _FileHandle,long _Size);
  _CRTIMP int __cdecl _close(int _FileHandle);
  _CRTIMP int __cdecl _commit(int _FileHandle);
  _CRTIMP int __cdecl _creat(const char *_Filename,int _PermissionMode);
  _CRTIMP int __cdecl _dup(int _FileHandle);
  _CRTIMP int __cdecl _dup2(int _FileHandleSrc,int _FileHandleDst);
  _CRTIMP int __cdecl _eof(int _FileHandle);
  _CRTIMP long __cdecl _filelength(int _FileHandle);
  _CRTIMP intptr_t __cdecl _findfirst32(const char *_Filename,struct _finddata32_t *_FindData);
  _CRTIMP int __cdecl _findnext32(intptr_t _FindHandle,struct _finddata32_t *_FindData);
  _CRTIMP int __cdecl _findclose(intptr_t _FindHandle);
  _CRTIMP int __cdecl _isatty(int _FileHandle);
  _CRTIMP int __cdecl _locking(int _FileHandle,int _LockMode,long _NumOfBytes);
  _CRTIMP long __cdecl _lseek(int _FileHandle,long _Offset,int _Origin);
  _off64_t lseek64(int fd,_off64_t offset, int whence);
  _CRTIMP char *__cdecl _mktemp(char *_TemplateName);
  _CRTIMP int __cdecl _pipe(int *_PtHandles,unsigned int _PipeSize,int _TextMode);
  _CRTIMP int __cdecl _read(int _FileHandle,void *_DstBuf,unsigned int _MaxCharCount);

#ifndef _CRT_DIRECTORY_DEFINED
#define _CRT_DIRECTORY_DEFINED
  int __cdecl remove(const char *_Filename);
  int __cdecl rename(const char *_OldFilename,const char *_NewFilename);
  _CRTIMP int __cdecl _unlink(const char *_Filename);
#ifndef	NO_OLDNAMES
  int __cdecl unlink(const char *_Filename);
#endif
#endif

  _CRTIMP int __cdecl _setmode(int _FileHandle,int _Mode);
  _CRTIMP long __cdecl _tell(int _FileHandle);
  _CRTIMP int __cdecl _umask(int _Mode);
  _CRTIMP int __cdecl _write(int _FileHandle,const void *_Buf,unsigned int _MaxCharCount);

#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP __int64 __cdecl _filelengthi64(int _FileHandle);
  _CRTIMP intptr_t __cdecl _findfirst32i64(con