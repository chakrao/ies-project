/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef GUID_DEFINED
#define GUID_DEFINED
typedef struct _GUID {
  unsigned long Data1;
  unsigned short Data2;
  unsigned short Data3;
  unsigned char Data4[8 ];
} GUID;
#endif

#ifndef UUID_DEFINED
#define UUID_DEFINED
typedef GUID UUID;
#endif

#ifndef FAR
#define FAR
#endif

#ifndef DECLSPEC_SELECTANY
#define DECLSPEC_SELECTANY __declspec(selectany)
#endif

#ifndef EXTERN_C
#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif
#endif

#ifdef DEFINE_GUID
#undef DEFINE_GUID
#endif

#ifdef INITGUID
#ifdef __cplusplus
#define DEFINE_GUID(name,l,w1,w2,b1,b2,b3,b4,b5,b6,b7,b8) EXTERN_C const GUID DECLSPEC_SELECTANY name = { l,w1,w2,{ b1,b2,b3,b4,b5,b6,b7,b8 } }
#else