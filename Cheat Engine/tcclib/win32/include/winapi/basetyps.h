/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#if !defined(_BASETYPS_H_)
#define _BASETYPS_H_

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#define STDMETHODCALLTYPE WINAPI
#define STDMETHODVCALLTYPE __cdecl

#define STDAPICALLTYPE WINAPI
#define STDAPIVCALLTYPE __cdecl

#define STDAPI EXTERN_