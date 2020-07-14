/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _BASETSD_H_
#define _BASETSD_H_

#if (defined(__x86_64) || defined(__ia64__)) && !defined(RC_INVOKED)
typedef unsigned __int64 POINTER_64_INT;
#else
typedef unsigned long POINTER_64_INT;
#endif

#define POINTER_32
#define POINTER_64
#define FIRMWARE_PTR

#ifdef __cplusplus
extern "C" {
#endif

  typedef signed char INT8,*PINT8;
  typedef signed short INT16,*PINT16;
  typedef signed int INT32,*PINT32;
  typedef signed __int64 INT64,*PINT64;
  typedef unsigned char UINT8,*PUINT8;
  typedef unsigned short UINT16,*PUINT16;
  typedef unsigned int UINT32,*PUINT32;
  typedef unsigned __int64 UINT64,*PUINT64;
  typedef signed int LONG32,*PLONG32;
  typedef unsigned int ULONG32,*PULONG32;
  typedef unsigned int DWORD32,*PDWORD32;

#ifndef _W64
#define _W64
#endif

#ifdef _WIN64
  typedef __int64 INT_PTR,*PINT_PTR;
  typedef unsigned __int64 UINT_PTR,*PUINT_PTR;
  typedef __int64 LONG_PTR,*PLONG_PTR;
  typedef unsigned __int64 ULONG_PTR,*PULONG_PTR;
#define __int3264 __int64
#else
  typedef int INT_PTR,*PINT_PTR;
  typedef unsigned int UINT_PTR,*PUINT_PTR;
  typedef long LONG_PTR,*PLONG_PTR;
  typedef unsigned long ULONG_PTR,*PULONG_PTR;
#define __int3264 __int32
#endif

#ifdef _WIN64
#define ADDRESS_TAG_BIT 0x40000000000ULL
  typedef __int64 SHANDLE_PTR;
  typedef unsigned __int64 HANDLE_PTR;
  typedef unsigned int UHALF_PTR,*PUHALF_PTR;
  typedef int HALF_PTR,*PHALF_PTR;

  static __inline unsigned long HandleToULong(const void *h) { return((unsigned long) (ULONG_PTR) h); }
  static __inline long HandleToLong(const void *h) { return((long) (LONG_PTR) h); }
  static __inline void *ULongToHandle(const unsigned long h) { return((void *) (UINT_PTR) h); }
  static __inline void *LongToHandle(const long h) { return((void *) (INT_PTR) h); }
  static __inline un