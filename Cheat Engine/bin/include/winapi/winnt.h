
/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _WINNT_
#define _WINNT_

#ifdef __cplusplus
extern "C" {
#endif

#include <ctype.h>
#define ANYSIZE_ARRAY 1

//gr #include <specstrings.h>

#define RESTRICTED_POINTER

#ifndef __CRT_UNALIGNED
#define __CRT_UNALIGNED
#endif

#if defined(__ia64__) || defined(__x86_64)
#define UNALIGNED __CRT_UNALIGNED
#ifdef _WIN64
#define UNALIGNED64 __CRT_UNALIGNED
#else
#define UNALIGNED64
#endif
#else
#define UNALIGNED
#define UNALIGNED64
#endif

#if !defined(I_X86_) && !defined(_IA64_) && !defined(_AMD64_) && (defined(_X86_) && !defined(__x86_64))
#define I_X86_
#endif

#if !defined(I_X86_) && !defined(_IA64_) && !defined(_AMD64_) && defined(__x86_64)
#define _AMD64_
#endif

#if !defined(I_X86_) && !(defined(_X86_) && !defined(__x86_64)) && !defined(_AMD64_) && defined(__ia64__)
#if !defined(_IA64_)
#define _IA64_
#endif
#endif


#ifdef _WIN64
#define MAX_NATURAL_ALIGNMENT sizeof(ULONGLONG)
#define MEMORY_ALLOCATION_ALIGNMENT 16
#else
#define MAX_NATURAL_ALIGNMENT sizeof(DWORD)
#define MEMORY_ALLOCATION_ALIGNMENT 8
#endif

#ifdef __cplusplus
#define TYPE_ALIGNMENT(t) __alignof__ (t)
#else
#define TYPE_ALIGNMENT(t) FIELD_OFFSET(struct { char x; t test; },test)
#endif

#ifdef _WIN64
#ifdef _AMD64_
#define PROBE_ALIGNMENT(_s) TYPE_ALIGNMENT(DWORD)
#elif defined(_IA64_)
#define PROBE_ALIGNMENT(_s) (TYPE_ALIGNMENT(_s) > TYPE_ALIGNMENT(DWORD) ? TYPE_ALIGNMENT(_s) : TYPE_ALIGNMENT(DWORD))
#else
#error No Target Architecture
#endif
#define PROBE_ALIGNMENT32(_s) TYPE_ALIGNMENT(DWORD)
#else
#define PROBE_ALIGNMENT(_s) TYPE_ALIGNMENT(DWORD)
#endif

#define C_ASSERT(e) typedef char __C_ASSERT__[(e)?1:-1]

#include <basetsd.h>

#if defined(_X86_) || defined(__ia64__) || defined(__x86_64)
#define DECLSPEC_IMPORT __declspec(dllimport)
#else
#define DECLSPEC_IMPORT
#endif

#ifndef DECLSPEC_NORETURN
#define DECLSPEC_NORETURN __declspec(noreturn)
#endif

#ifndef DECLSPEC_ALIGN
#define DECLSPEC_ALIGN(x) __attribute__ ((aligned(x)))
#endif

#ifndef SYSTEM_CACHE_ALIGNMENT_SIZE
#if defined(_AMD64_) || defined(I_X86_)
#define SYSTEM_CACHE_ALIGNMENT_SIZE 64
#else
#define SYSTEM_CACHE_ALIGNMENT_SIZE 128
#endif
#endif

#ifndef DECLSPEC_CACHEALIGN
#define DECLSPEC_CACHEALIGN DECLSPEC_ALIGN(SYSTEM_CACHE_ALIGNMENT_SIZE)
#endif

#ifndef DECLSPEC_UUID
#define DECLSPEC_UUID(x)
#endif

#ifndef DECLSPEC_NOVTABLE
#define DECLSPEC_NOVTABLE
#endif

#ifndef DECLSPEC_SELECTANY
#define DECLSPEC_SELECTANY __declspec(selectany)
#endif

#ifndef NOP_FUNCTION
#define NOP_FUNCTION (void)0
#endif

#ifndef DECLSPEC_NOINLINE
#define DECLSPEC_NOINLINE
#endif

#ifndef FORCEINLINE
#define FORCEINLINE static __inline__
#endif

#ifndef DECLSPEC_DEPRECATED
#define DECLSPEC_DEPRECATED __declspec(deprecated)
#define DEPRECATE_SUPPORTED
#endif

#define DECLSPEC_DEPRECATED_DDK
#define PRAGMA_DEPRECATED_DDK 0

  typedef void *PVOID;
  typedef void *PVOID64;

#define NTAPI __stdcall
#define NTSYSAPI DECLSPEC_IMPORT
#define NTSYSCALLAPI DECLSPEC_IMPORT

#ifndef VOID
#define VOID void
  typedef char CHAR;
  typedef short SHORT;
  typedef long LONG;
#endif

  typedef wchar_t WCHAR;
  typedef WCHAR *PWCHAR,*LPWCH,*PWCH;
  typedef CONST WCHAR *LPCWCH,*PCWCH;
  typedef WCHAR *NWPSTR,*LPWSTR,*PWSTR;
  typedef PWSTR *PZPWSTR;
  typedef CONST PWSTR *PCZPWSTR;
  typedef WCHAR UNALIGNED *LPUWSTR,*PUWSTR;
  typedef CONST WCHAR *LPCWSTR,*PCWSTR;
  typedef PCWSTR *PZPCWSTR;
  typedef CONST WCHAR UNALIGNED *LPCUWSTR,*PCUWSTR;
  typedef CHAR *PCHAR,*LPCH,*PCH;
  typedef CONST CHAR *LPCCH,*PCCH;
  typedef CHAR *NPSTR,*LPSTR,*PSTR;
  typedef PSTR *PZPSTR;
  typedef CONST PSTR *PCZPSTR;
  typedef CONST CHAR *LPCSTR,*PCSTR;
  typedef PCSTR *PZPCSTR;

#ifdef UNICODE
#ifndef _TCHAR_DEFINED
#define _TCHAR_DEFINED
  typedef WCHAR TCHAR,*PTCHAR;
  typedef WCHAR TBYTE ,*PTBYTE;
#endif

  typedef LPWSTR LPTCH,PTCH;
  typedef LPWSTR PTSTR,LPTSTR;
  typedef LPCWSTR PCTSTR,LPCTSTR;
  typedef LPUWSTR PUTSTR,LPUTSTR;
  typedef LPCUWSTR PCUTSTR,LPCUTSTR;
  typedef LPWSTR LP;
#define __TEXT(quote) L##quote
#else
#ifndef _TCHAR_DEFINED
#define _TCHAR_DEFINED
  typedef char TCHAR,*PTCHAR;
  typedef unsigned char TBYTE ,*PTBYTE;
#endif

  typedef LPSTR LPTCH,PTCH;
  typedef LPSTR PTSTR,LPTSTR,PUTSTR,LPUTSTR;
  typedef LPCSTR PCTSTR,LPCTSTR,PCUTSTR,LPCUTSTR;
#define __TEXT(quote) quote
#endif

#define TEXT(quote) __TEXT(quote)

  typedef SHORT *PSHORT;
  typedef LONG *PLONG;

  typedef void *HANDLE;
#define DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name
  typedef HANDLE *PHANDLE;

  typedef BYTE FCHAR;
  typedef WORD FSHORT;
  typedef DWORD FLONG;

#ifndef _HRESULT_DEFINED
#define _HRESULT_DEFINED
  typedef LONG HRESULT;
#endif

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#define STDMETHODCALLTYPE WINAPI
#define STDMETHODVCALLTYPE __cdecl
#define STDAPICALLTYPE WINAPI
#define STDAPIVCALLTYPE __cdecl
#define STDAPI EXTERN_C HRESULT WINAPI
#define STDAPI_(type) EXTERN_C type WINAPI
#define STDMETHODIMP HRESULT WINAPI
#define STDMETHODIMP_(type) type WINAPI
#define STDAPIV EXTERN_C HRESULT STDAPIVCALLTYPE
#define STDAPIV_(type) EXTERN_C type STDAPIVCALLTYPE
#define STDMETHODIMPV HRESULT STDMETHODVCALLTYPE
#define STDMETHODIMPV_(type) type STDMETHODVCALLTYPE

  typedef char CCHAR;
#ifndef _LCID_DEFINED
#define _LCID_DEFINED
typedef DWORD LCID;
#endif
  typedef PDWORD PLCID;
#ifndef _LANGID_DEFINED
#define _LANGID_DEFINED
  typedef WORD LANGID;
#endif
#define APPLICATION_ERROR_MASK 0x20000000
#define ERROR_SEVERITY_SUCCESS 0x00000000
#define ERROR_SEVERITY_INFORMATIONAL 0x40000000
#define ERROR_SEVERITY_WARNING 0x80000000
#define ERROR_SEVERITY_ERROR 0xC0000000

#ifdef __ia64__
  __declspec(align(16))
#endif
    typedef struct _FLOAT128 {
      __int64 LowPart;
      __int64 HighPart;
  } FLOAT128;

  typedef FLOAT128 *PFLOAT128;

#define _ULONGLONG_
#if((!(defined(_X86_) && !defined(__x86_64)) || (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 64)))
  typedef __int64 LONGLONG;
  typedef unsigned __int64 ULONGLONG;

#define MAXLONGLONG (0x7fffffffffffffff)
#else

  typedef double LONGLONG;
  typedef double ULONGLONG;
#endif

  typedef LONGLONG *PLONGLONG;
  typedef ULONGLONG *PULONGLONG;

  typedef LONGLONG USN;

  typedef union _LARGE_INTEGER {
    struct {
      DWORD LowPart;
      LONG HighPart;
    };
    struct {
      DWORD LowPart;
      LONG HighPart;
    } u;
    LONGLONG QuadPart;
  } LARGE_INTEGER;

  typedef LARGE_INTEGER *PLARGE_INTEGER;

  typedef union _ULARGE_INTEGER {
    struct {
      DWORD LowPart;
      DWORD HighPart;
    };
    struct {
      DWORD LowPart;
      DWORD HighPart;
    } u;
    ULONGLONG QuadPart;
  } ULARGE_INTEGER;

  typedef ULARGE_INTEGER *PULARGE_INTEGER;

  typedef struct _LUID {
    DWORD LowPart;
    LONG HighPart;
  } LUID,*PLUID;

#define _DWORDLONG_
  typedef ULONGLONG DWORDLONG;
  typedef DWORDLONG *PDWORDLONG;

#ifdef RC_INVOKED
#define Int32x32To64(a,b) ((LONGLONG)((LONG)(a)) *(LONGLONG)((LONG)(b)))
#define UInt32x32To64(a,b) ((ULONGLONG)((DWORD)(a)) *(ULONGLONG)((DWORD)(b)))
#define Int64ShrlMod32(a,b) ((ULONGLONG)(a) >> (b))
#elif (defined(_X86_) && !defined(__x86_64))
#define Int32x32To64(a,b) (LONGLONG)((LONGLONG)(LONG)(a) *(LONG)(b))
#define UInt32x32To64(a,b) (ULONGLONG)((ULONGLONG)(DWORD)(a) *(DWORD)(b))
#define Int64ShrlMod32(a,b) ((DWORDLONG)(a)>>(b))
#elif defined(__ia64__) || defined(__x86_64)
#define Int32x32To64(a,b) ((LONGLONG)((LONG)(a)) *(LONGLONG)((LONG)(b)))
#define UInt32x32To64(a,b) ((ULONGLONG)((DWORD)(a)) *(ULONGLONG)((DWORD)(b)))
#define Int64ShrlMod32(a,b) ((ULONGLONG)(a) >> (b))
#else
#error Must define a target architecture.
#endif

#define Int64ShraMod32(a,b) ((LONGLONG)(a) >> (b))
#define Int64ShllMod32(a,b) ((ULONGLONG)(a) << (b))

#ifdef __cplusplus
  extern "C" {
#endif

#ifdef __x86_64

#define RotateLeft8 _rotl8
#define RotateLeft16 _rotl16
#define RotateRight8 _rotr8
#define RotateRight16 _rotr16

    unsigned char __cdecl _rotl8(unsigned char Value,unsigned char Shift);
    unsigned short __cdecl _rotl16(unsigned short Value,unsigned char Shift);
    unsigned char __cdecl _rotr8(unsigned char Value,unsigned char Shift);
    unsigned short __cdecl _rotr16(unsigned short Value,unsigned char Shift);
#endif

#define RotateLeft32 _rotl
#define RotateLeft64 _rotl64
#define RotateRight32 _rotr
#define RotateRight64 _rotr64

    unsigned int __cdecl _rotl(unsigned int Value,int Shift);
    unsigned __int64 __cdecl _rotl64(unsigned __int64 Value,int Shift);
    unsigned int __cdecl _rotr(unsigned int Value,int Shift);
    unsigned __int64 __cdecl _rotr64(unsigned __int64 Value,int Shift);
#ifdef __cplusplus
  }
#endif

#define ANSI_NULL ((CHAR)0)
#define UNICODE_NULL ((WCHAR)0)
#define UNICODE_STRING_MAX_BYTES ((WORD) 65534)
#define UNICODE_STRING_MAX_CHARS (32767)

#ifndef _BOOLEAN_
#define _BOOLEAN_
  typedef BYTE BOOLEAN;
#endif
  typedef BOOLEAN *PBOOLEAN;

  typedef struct _LIST_ENTRY {
    struct _LIST_ENTRY *Flink;
    struct _LIST_ENTRY *Blink;
  } LIST_ENTRY,*PLIST_ENTRY,*RESTRICTED_POINTER PRLIST_ENTRY;

  typedef struct _SINGLE_LIST_ENTRY {
    struct _SINGLE_LIST_ENTRY *Next;
  } SINGLE_LIST_ENTRY,*PSINGLE_LIST_ENTRY;

  typedef struct LIST_ENTRY32 {
    DWORD Flink;
    DWORD Blink;
  } LIST_ENTRY32;
  typedef LIST_ENTRY32 *PLIST_ENTRY32;

  typedef struct LIST_ENTRY64 {
    ULONGLONG Flink;
    ULONGLONG Blink;
  } LIST_ENTRY64;
  typedef LIST_ENTRY64 *PLIST_ENTRY64;

#include <guiddef.h>

#ifndef __OBJECTID_DEFINED
#define __OBJECTID_DEFINED
  typedef struct _OBJECTID {
    GUID Lineage;
    DWORD Uniquifier;
  } OBJECTID;
#endif

#define MINCHAR 0x80
#define MAXCHAR 0x7f
#define MINSHORT 0x8000
#define MAXSHORT 0x7fff
#define MINLONG 0x80000000
#define MAXLONG 0x7fffffff
#define MAXBYTE 0xff
#define MAXWORD 0xffff
#define MAXDWORD 0xffffffff

#define FIELD_OFFSET(type,field) ((LONG)(LONG_PTR)&(((type *)0)->field))
#define RTL_FIELD_SIZE(type,field) (sizeof(((type *)0)->field))
#define RTL_SIZEOF_THROUGH_FIELD(type,field) (FIELD_OFFSET(type,field) + RTL_FIELD_SIZE(type,field))
#define RTL_CONTAINS_FIELD(Struct,Size,Field) ((((PCHAR)(&(Struct)->Field)) + sizeof((Struct)->Field)) <= (((PCHAR)(Struct))+(Size)))
#define RTL_NUMBER_OF_V1(A) (sizeof(A)/sizeof((A)[0]))
#define RTL_NUMBER_OF_V2(A) RTL_NUMBER_OF_V1(A)

#ifdef ENABLE_RTL_NUMBER_OF_V2
#define RTL_NUMBER_OF(A) RTL_NUMBER_OF_V2(A)
#else
#define RTL_NUMBER_OF(A) RTL_NUMBER_OF_V1(A)
#endif

#define ARRAYSIZE(A) RTL_NUMBER_OF_V2(A)
#define _ARRAYSIZE(A) RTL_NUMBER_OF_V1(A)

#define RTL_FIELD_TYPE(type,field) (((type*)0)->field)
#define RTL_NUMBER_OF_FIELD(type,field) (RTL_NUMBER_OF(RTL_FIELD_TYPE(type,field)))
#define RTL_PADDING_BETWEEN_FIELDS(T,F1,F2) ((FIELD_OFFSET(T,F2) > FIELD_OFFSET(T,F1)) ? (FIELD_OFFSET(T,F2) - FIELD_OFFSET(T,F1) - RTL_FIELD_SIZE(T,F1)) : (FIELD_OFFSET(T,F1) - FIELD_OFFSET(T,F2) - RTL_FIELD_SIZE(T,F2)))

#ifdef __cplusplus
#define RTL_CONST_CAST(type) const_cast<type>
#else
#define RTL_CONST_CAST(type) (type)
#endif

#define RTL_BITS_OF(sizeOfArg) (sizeof(sizeOfArg) *8)
#define RTL_BITS_OF_FIELD(type,field) (RTL_BITS_OF(RTL_FIELD_TYPE(type,field)))
#define CONTAINING_RECORD(address,type,field) ((type *)((PCHAR)(address) - (ULONG_PTR)(&((type *)0)->field)))

#define VER_SERVER_NT 0x80000000
#define VER_WORKSTATION_NT 0x40000000
#define VER_SUITE_SMALLBUSINESS 0x00000001
#define VER_SUITE_ENTERPRISE 0x00000002
#define VER_SUITE_BACKOFFICE 0x00000004
#define VER_SUITE_COMMUNICATIONS 0x00000008
#define VER_SUITE_TERMINAL 0x00000010
#define VER_SUITE_SMALLBUSINESS_RESTRICTED 0x00000020
#define VER_SUITE_EMBEDDEDNT 0x00000040
#define VER_SUITE_DATACENTER 0x00000080
#define VER_SUITE_SINGLEUSERTS 0x00000100
#define VER_SUITE_PERSONAL 0x00000200
#define VER_SUITE_BLADE 0x00000400
#define VER_SUITE_EMBEDDED_RESTRICTED 0x00000800
#define VER_SUITE_SECURITY_APPLIANCE 0x00001000
#define VER_SUITE_STORAGE_SERVER 0x00002000
#define VER_SUITE_COMPUTE_SERVER 0x00004000

#define PRODUCT_UNDEFINED                       0x0

#define PRODUCT_ULTIMATE                        0x1
#define PRODUCT_HOME_BASIC                      0x2
#define PRODUCT_HOME_PREMIUM                    0x3
#define PRODUCT_ENTERPRISE                      0x4
#define PRODUCT_HOME_BASIC_N                    0x5
#define PRODUCT_BUSINESS                        0x6
#define PRODUCT_STANDARD_SERVER                 0x7
#define PRODUCT_DATACENTER_SERVER               0x8
#define PRODUCT_SMALLBUSINESS_SERVER            0x9
#define PRODUCT_ENTERPRISE_SERVER               0xa
#define PRODUCT_STARTER                         0xb
#define PRODUCT_DATACENTER_SERVER_CORE          0xc
#define PRODUCT_STANDARD_SERVER_CORE            0xd
#define PRODUCT_ENTERPRISE_SERVER_CORE          0xe
#define PRODUCT_ENTERPRISE_SERVER_IA64          0xf
#define PRODUCT_BUSINESS_N                      0x10
#define PRODUCT_WEB_SERVER                      0x11
#define PRODUCT_CLUSTER_SERVER                  0x12
#define PRODUCT_HOME_SERVER                     0x13
#define PRODUCT_STORAGE_EXPRESS_SERVER          0x14
#define PRODUCT_STORAGE_STANDARD_SERVER         0x15
#define PRODUCT_STORAGE_WORKGROUP_SERVER        0x16
#define PRODUCT_STORAGE_ENTERPRISE_SERVER       0x17
#define PRODUCT_SERVER_FOR_SMALLBUSINESS        0x18
#define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM    0x19

#define PRODUCT_UNLICENSED                      0xabcdabcd

#define LANG_NEUTRAL 0x00
#define LANG_INVARIANT 0x7f

#define LANG_AFRIKAANS 0x36
#define LANG_ALBANIAN 0x1c
#define LANG_ALSATIAN 0x84
#define LANG_AMHARIC 0x5e
#define LANG_ARABIC 0x01
#define LANG_ARMENIAN 0x2b
#define LANG_ASSAMESE 0x4d
#define LANG_AZERI 0x2c
#define LANG_BASHKIR 0x6d
#define LANG_BASQUE 0x2d
#define LANG_BELARUSIAN 0x23
#define LANG_BENGALI 0x45
#define LANG_BRETON 0x7e
#define LANG_BOSNIAN 0x1a
#define LANG_BOSNIAN_NEUTRAL 0x781a
#define LANG_BULGARIAN 0x02
#define LANG_CATALAN 0x03
#define LANG_CHINESE 0x04
#define LANG_CHINESE_SIMPLIFIED 0x04
#define LANG_CHINESE_TRADITIONAL 0x7c04
#define LANG_CORSICAN 0x83
#define LANG_CROATIAN 0x1a
#define LANG_CZECH 0x05
#define LANG_DANISH 0x06
#define LANG_DARI 0x8c
#define LANG_DIVEHI 0x65
#define LANG_DUTCH 0x13
#define LANG_ENGLISH 0x09
#define LANG_ESTONIAN 0x25
#define LANG_FAEROESE 0x38
#define LANG_FARSI 0x29
#define LANG_FILIPINO 0x64
#define LANG_FINNISH 0x0b
#define LANG_FRENCH 0x0c
#define LANG_FRISIAN 0x62
#define LANG_GALICIAN 0x56
#define LANG_GEORGIAN 0x37
#define LANG_GERMAN 0x07
#define LANG_GREEK 0x08
#define LANG_GREENLANDIC 0x6f
#define LANG_GUJARATI 0x47
#define LANG_HAUSA 0x68
#define LANG_HEBREW 0x0d
#define LANG_HINDI 0x39
#define LANG_HUNGARIAN 0x0e
#define LANG_ICELANDIC 0x0f
#define LANG_IGBO 0x70
#define LANG_INDONESIAN 0x21
#define LANG_INUKTITUT 0x5d
#define LANG_IRISH 0x3c
#define LANG_ITALIAN 0x10
#define LANG_JAPANESE 0x11
#define LANG_KANNADA 0x4b
#define LANG_KASHMIRI 0x60
#define LANG_KAZAK 0x3f
#define LANG_KHMER 0x53
#define LANG_KICHE 0x86
#define LANG_KINYARWANDA 0x87
#define LANG_KONKANI 0x57
#define LANG_KOREAN 0x12
#define LANG_KYRGYZ 0x40
#define LANG_LAO 0x54
#define LANG_LATVIAN 0x26
#define LANG_LITHUANIAN 0x27
#define LANG_LOWER_SORBIAN 0x2e
#define LANG_LUXEMBOURGISH 0x6e
#define LANG_MACEDONIAN 0x2f
#define LANG_MALAY 0x3e
#define LANG_MALAYALAM 0x4c
#define LANG_MALTESE 0x3a
#define LANG_MANIPURI 0x58
#define LANG_MAORI 0x81
#define LANG_MAPUDUNGUN 0x7a
#define LANG_MARATHI 0x4e
#define LANG_MOHAWK 0x7c
#define LANG_MONGOLIAN 0x50
#define LANG_NEPALI 0x61
#define LANG_NORWEGIAN 0x14
#define LANG_OCCITAN 0x82
#define LANG_ORIYA 0x48
#define LANG_PASHTO 0x63
#define LANG_PERSIAN 0x29
#define LANG_POLISH 0x15
#define LANG_PORTUGUESE 0x16
#define LANG_PUNJABI 0x46
#define LANG_QUECHUA 0x6b
#define LANG_ROMANIAN 0x18
#define LANG_RUSSIAN 0x19
#define LANG_SAMI 0x3b
#define LANG_ROMANSH 0x17
#define LANG_SANSKRIT 0x4f
#define LANG_SERBIAN 0x1a
#define LANG_SERBIAN_NEUTRAL 0x7c1a
#define LANG_SINDHI 0x59
#define LANG_SINHALESE 0x5b
#define LANG_SLOVAK 0x1b
#define LANG_SLOVENIAN 0x24
#define LANG_SOTHO 0x6c
#define LANG_SPANISH 0x0a
#define LANG_SWAHILI 0x41
#define LANG_SWEDISH 0x1d
#define LANG_SYRIAC 0x5a
#define LANG_TAJIK 0x28
#define LANG_TAMAZIGHT 0x5f
#define LANG_TAMIL 0x49
#define LANG_TATAR 0x44
#define LANG_TELUGU 0x4a
#define LANG_THAI 0x1e
#define LANG_TIBETAN 0x51
#define LANG_TIGRIGNA 0x73
#define LANG_TSWANA 0x32
#define LANG_TURKISH 0x1f
#define LANG_TURKMEN 0x42
#define LANG_UIGHUR 0x80
#define LANG_UKRAINIAN 0x22
#define LANG_UPPER_SORBIAN 0x2e
#define LANG_URDU 0x20
#define LANG_UZBEK 0x43
#define LANG_VIETNAMESE 0x2a
#define LANG_WELSH 0x52
#define LANG_WOLOF 0x88
#define LANG_XHOSA 0x34
#define LANG_YAKUT 0x85
#define LANG_YI 0x78
#define LANG_YORUBA 0x6a
#define LANG_ZULU 0x35

#define SUBLANG_NEUTRAL 0x0
#define SUBLANG_DEFAULT 0x1
#define SUBLANG_SYS_DEFAULT 0x2
#define SUBLANG_CUSTOM_DEFAULT 0x3
#define SUBLANG_CUSTOM_UNSPECIFIED 0x4
#define SUBLANG_UI_CUSTOM_DEFAULT 0x5

#define SUBLANG_ARABIC_SAUDI_ARABIA 0x01
#define SUBLANG_ARABIC_IRAQ 0x02
#define SUBLANG_ARABIC_EGYPT 0x03
#define SUBLANG_ARABIC_LIBYA 0x04
#define SUBLANG_ARABIC_ALGERIA 0x05
#define SUBLANG_ARABIC_MOROCCO 0x06
#define SUBLANG_ARABIC_TUNISIA 0x07
#define SUBLANG_ARABIC_OMAN 0x08
#define SUBLANG_ARABIC_YEMEN 0x09
#define SUBLANG_ARABIC_SYRIA 0x0a
#define SUBLANG_ARABIC_JORDAN 0x0b
#define SUBLANG_ARABIC_LEBANON 0x0c
#define SUBLANG_ARABIC_KUWAIT 0x0d
#define SUBLANG_ARABIC_UAE 0x0e
#define SUBLANG_ARABIC_BAHRAIN 0x0f
#define SUBLANG_ARABIC_QATAR 0x10
#define SUBLANG_AZERI_LATIN 0x01
#define SUBLANG_AZERI_CYRILLIC 0x02
#define SUBLANG_CHINESE_TRADITIONAL 0x01
#define SUBLANG_CHINESE_SIMPLIFIED 0x02
#define SUBLANG_CHINESE_HONGKONG 0x03
#define SUBLANG_CHINESE_SINGAPORE 0x04
#define SUBLANG_CHINESE_MACAU 0x05
#define SUBLANG_DUTCH 0x01
#define SUBLANG_DUTCH_BELGIAN 0x02
#define SUBLANG_ENGLISH_US 0x01
#define SUBLANG_ENGLISH_UK 0x02
#define SUBLANG_ENGLISH_AUS 0x03
#define SUBLANG_ENGLISH_CAN 0x04
#define SUBLANG_ENGLISH_NZ 0x05
#define SUBLANG_ENGLISH_EIRE 0x06
#define SUBLANG_ENGLISH_SOUTH_AFRICA 0x07
#define SUBLANG_ENGLISH_JAMAICA 0x08
#define SUBLANG_ENGLISH_CARIBBEAN 0x09
#define SUBLANG_ENGLISH_BELIZE 0x0a
#define SUBLANG_ENGLISH_TRINIDAD 0x0b
#define SUBLANG_ENGLISH_ZIMBABWE 0x0c
#define SUBLANG_ENGLISH_PHILIPPINES 0x0d
#define SUBLANG_FRENCH 0x01
#define SUBLANG_FRENCH_BELGIAN 0x02
#define SUBLANG_FRENCH_CANADIAN 0x03
#define SUBLANG_FRENCH_SWISS 0x04
#define SUBLANG_FRENCH_LUXEMBOURG 0x05
#define SUBLANG_FRENCH_MONACO 0x06
#define SUBLANG_GERMAN 0x01
#define SUBLANG_GERMAN_SWISS 0x02
#define SUBLANG_GERMAN_AUSTRIAN 0x03
#define SUBLANG_GERMAN_LUXEMBOURG 0x04
#define SUBLANG_GERMAN_LIECHTENSTEIN 0x05
#define SUBLANG_ITALIAN 0x01
#define SUBLANG_ITALIAN_SWISS 0x02
#define SUBLANG_KASHMIRI_SASIA 0x02
#define SUBLANG_KASHMIRI_INDIA 0x02
#define SUBLANG_KOREAN 0x01
#define SUBLANG_LITHUANIAN 0x01
#define SUBLANG_MALAY_MALAYSIA 0x01
#define SUBLANG_MALAY_BRUNEI_DARUSSALAM 0x02
#define SUBLANG_NEPALI_INDIA 0x02
#define SUBLANG_NORWEGIAN_BOKMAL 0x01
#define SUBLANG_NORWEGIAN_NYNORSK 0x02
#define SUBLANG_PORTUGUESE 0x02
#define SUBLANG_PORTUGUESE_BRAZILIAN 0x01
#define SUBLANG_SERBIAN_LATIN 0x02
#define SUBLANG_SERBIAN_CYRILLIC 0x03
#define SUBLANG_SPANISH 0x01
#define SUBLANG_SPANISH_MEXICAN 0x02
#define SUBLANG_SPANISH_MODERN 0x03
#define SUBLANG_SPANISH_GUATEMALA 0x04
#define SUBLANG_SPANISH_COSTA_RICA 0x05
#define SUBLANG_SPANISH_PANAMA 0x06
#define SUBLANG_SPANISH_DOMINICAN_REPUBLIC 0x07
#define SUBLANG_SPANISH_VENEZUELA 0x08
#define SUBLANG_SPANISH_COLOMBIA 0x09
#define SUBLANG_SPANISH_PERU 0x0a
#define SUBLANG_SPANISH_ARGENTINA 0x0b
#define SUBLANG_SPANISH_ECUADOR 0x0c
#define SUBLANG_SPANISH_CHILE 0x0d
#define SUBLANG_SPANISH_URUGUAY 0x0e
#define SUBLANG_SPANISH_PARAGUAY 0x0f
#define SUBLANG_SPANISH_BOLIVIA 0x10
#define SUBLANG_SPANISH_EL_SALVADOR 0x11
#define SUBLANG_SPANISH_HONDURAS 0x12
#define SUBLANG_SPANISH_NICARAGUA 0x13
#define SUBLANG_SPANISH_PUERTO_RICO 0x14
#define SUBLANG_SWEDISH 0x01
#define SUBLANG_SWEDISH_FINLAND 0x02
#define SUBLANG_URDU_PAKISTAN 0x01
#define SUBLANG_URDU_INDIA 0x02
#define SUBLANG_UZBEK_LATIN 0x01
#define SUBLANG_UZBEK_CYRILLIC 0x02

#define SORT_DEFAULT 0x0
#define SORT_INVARIANT_MATH 0x1

#define SORT_JAPANESE_XJIS 0x0
#define SORT_JAPANESE_UNICODE 0x1
#define SORT_JAPANESE_RADICALSTROKE 0x4

#define SORT_CHINESE_BIG5 0x0
#define SORT_CHINESE_PRCP 0x0
#define SORT_CHINESE_UNICODE 0x1
#define SORT_CHINESE_PRC 0x2
#define SORT_CHINESE_BOPOMOFO 0x3

#define SORT_KOREAN_KSC 0x0
#define SORT_KOREAN_UNICODE 0x1

#define SORT_GERMAN_PHONE_BOOK 0x1

#define SORT_HUNGARIAN_DEFAULT 0x0
#define SORT_HUNGARIAN_TECHNICAL 0x1

#define SORT_GEORGIAN_TRADITIONAL 0x0
#define SORT_GEORGIAN_MODERN 0x1

#define MAKELANGID(p,s) ((((WORD)(s)) << 10) | (WORD)(p))
#define PRIMARYLANGID(lgid) ((WORD)(lgid) & 0x3ff)
#define SUBLANGID(lgid) ((WORD)(lgid) >> 10)

#define NLS_VALID_LOCALE_MASK 0x000fffff

#define MAKELCID(lgid,srtid) ((DWORD)((((DWORD)((WORD)(srtid))) << 16) | ((DWORD)((WORD)(lgid)))))
#define MAKESORTLCID(lgid,srtid,ver) ((DWORD)((MAKELCID(lgid,srtid)) | (((DWORD)((WORD)(ver))) << 20)))
#define LANGIDFROMLCID(lcid) ((WORD)(lcid))
#define SORTIDFROMLCID(lcid) ((WORD)((((DWORD)(lcid)) >> 16) & 0xf))
#define SORTVERSIONFROMLCID(lcid) ((WORD)((((DWORD)(lcid)) >> 20) & 0xf))

#define LOCALE_NAME_MAX_LENGTH 85
#define LANG_SYSTEM_DEFAULT (MAKELANGID(LANG_NEUTRAL,SUBLANG_SYS_DEFAULT))
#define LANG_USER_DEFAULT (MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT))

#define LOCALE_SYSTEM_DEFAULT (MAKELCID(LANG_SYSTEM_DEFAULT,SORT_DEFAULT))
#define LOCALE_USER_DEFAULT (MAKELCID(LANG_USER_DEFAULT,SORT_DEFAULT))

#define LOCALE_NEUTRAL (MAKELCID(MAKELANGID(LANG_NEUTRAL,SUBLANG_NEUTRAL),SORT_DEFAULT))

#define LOCALE_CUSTOM_DEFAULT (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_CUSTOM_DEFAULT), SORT_DEFAULT))
#define LOCALE_CUSTOM_UNSPECIFIED (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_CUSTOM_UNSPECIFIED), SORT_DEFAULT))
#define LOCALE_CUSTOM_UI_DEFAULT (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_UI_CUSTOM_DEFAULT), SORT_DEFAULT))

#define LOCALE_INVARIANT (MAKELCID(MAKELANGID(LANG_INVARIANT,SUBLANG_NEUTRAL),SORT_DEFAULT))

#define UNREFERENCED_PARAMETER(P) (P)
#define DBG_UNREFERENCED_PARAMETER(P) (P)
#define DBG_UNREFERENCED_LOCAL_VARIABLE(V) (V)

#define DEFAULT_UNREACHABLE

#ifndef WIN32_NO_STATUS
#define STATUS_WAIT_0 ((DWORD)0x00000000L)
#define STATUS_ABANDONED_WAIT_0 ((DWORD)0x00000080L)
#define STATUS_USER_APC ((DWORD)0x000000C0L)
#define STATUS_TIMEOUT ((DWORD)0x00000102L)
#define STATUS_PENDING ((DWORD)0x00000103L)
#define DBG_EXCEPTION_HANDLED ((DWORD)0x00010001L)
#define DBG_CONTINUE ((DWORD)0x00010002L)
#define STATUS_SEGMENT_NOTIFICATION ((DWORD)0x40000005L)
#define DBG_TERMINATE_THREAD ((DWORD)0x40010003L)
#define DBG_TERMINATE_PROCESS ((DWORD)0x40010004L)
#define DBG_CONTROL_C ((DWORD)0x40010005L)
#define DBG_CONTROL_BREAK ((DWORD)0x40010008L)
#define DBG_COMMAND_EXCEPTION ((DWORD)0x40010009L)
#define STATUS_GUARD_PAGE_VIOLATION ((DWORD)0x80000001L)
#define STATUS_DATATYPE_MISALIGNMENT ((DWORD)0x80000002L)
#define STATUS_BREAKPOINT ((DWORD)0x80000003L)
#define STATUS_SINGLE_STEP ((DWORD)0x80000004L)
#define DBG_EXCEPTION_NOT_HANDLED ((DWORD)0x80010001L)
#define STATUS_ACCESS_VIOLATION ((DWORD)0xC0000005L)
#define STATUS_IN_PAGE_ERROR ((DWORD)0xC0000006L)
#define STATUS_INVALID_HANDLE ((DWORD)0xC0000008L)
#define STATUS_NO_MEMORY ((DWORD)0xC0000017L)
#define STATUS_ILLEGAL_INSTRUCTION ((DWORD)0xC000001DL)
#define STATUS_NONCONTINUABLE_EXCEPTION ((DWORD)0xC0000025L)
#define STATUS_INVALID_DISPOSITION ((DWORD)0xC0000026L)
#define STATUS_ARRAY_BOUNDS_EXCEEDED ((DWORD)0xC000008CL)
#define STATUS_FLOAT_DENORMAL_OPERAND ((DWORD)0xC000008DL)
#define STATUS_FLOAT_DIVIDE_BY_ZERO ((DWORD)0xC000008EL)
#define STATUS_FLOAT_INEXACT_RESULT ((DWORD)0xC000008FL)
#define STATUS_FLOAT_INVALID_OPERATION ((DWORD)0xC0000090L)
#define STATUS_FLOAT_OVERFLOW ((DWORD)0xC0000091L)
#define STATUS_FLOAT_STACK_CHECK ((DWORD)0xC0000092L)
#define STATUS_FLOAT_UNDERFLOW ((DWORD)0xC0000093L)
#define STATUS_INTEGER_DIVIDE_BY_ZERO ((DWORD)0xC0000094L)
#define STATUS_INTEGER_OVERFLOW ((DWORD)0xC0000095L)
#define STATUS_PRIVILEGED_INSTRUCTION ((DWORD)0xC0000096L)
#define STATUS_STACK_OVERFLOW ((DWORD)0xC00000FDL)
#define STATUS_CONTROL_C_EXIT ((DWORD)0xC000013AL)
#define STATUS_FLOAT_MULTIPLE_FAULTS ((DWORD)0xC00002B4L)
#define STATUS_FLOAT_MULTIPLE_TRAPS ((DWORD)0xC00002B5L)
#define STATUS_REG_NAT_CONSUMPTION ((DWORD)0xC00002C9L)
#define STATUS_SXS_EARLY_DEACTIVATION ((DWORD)0xC015000FL)
#define STATUS_SXS_INVALID_DEACTIVATION ((DWORD)0xC0150010L)
#endif

#define MAXIMUM_WAIT_OBJECTS 64
#define MAXIMUM_SUSPEND_COUNT MAXCHAR

  typedef ULONG_PTR KSPIN_LOCK;
  typedef KSPIN_LOCK *PKSPIN_LOCK;

#ifdef _AMD64_

#if defined(__x86_64) && !defined(RC_INVOKED)

#ifdef __cplusplus
  extern "C" {
#endif

#define BitTest _bittest
#define BitTestAndComplement _bittestandcomplement
#define BitTestAndSet _bittestandset
#define BitTestAndReset _bittestandreset
#define InterlockedBitTestAndSet _interlockedbittestandset
#define InterlockedBitTestAndReset _interlockedbittestandreset
#define BitTest64 _bittest64
#define BitTestAndComplement64 _bittestandcomplement64
#define BitTestAndSet64 _bittestandset64
#define BitTestAndReset64 _bittestandreset64
#define InterlockedBitTestAndSet64 _interlockedbittestandset64
#define InterlockedBitTestAndReset64 _interlockedbittestandreset64

    __CRT_INLINE BOOLEAN _bittest(LONG const *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandcomplement(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btcl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN InterlockedBitTestAndComplement(LONG *Base,LONG Bit) {
      int old = 0;
      __asm__ __volatile__("lock ; btcl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Bit));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btsl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandreset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btrl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btsl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandreset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btrl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittest64(LONG64 const *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandcomplement64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btcq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btsq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandreset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btrq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btsq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandreset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btrq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
#define BitScanForward _BitScanForward
#define BitScanReverse _BitScanReverse
#define BitScanForward64 _BitScanForward64
#define BitScanReverse64 _BitScanReverse64

    __CRT_INLINE BOOLEAN _BitScanForward(DWORD *Index,DWORD Mask) {
      __asm__ __volatile__("bsfl %1,%0" : "=r" (Mask),"=m" ((*(volatile long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanReverse(DWORD *Index,DWORD Mask) {
      __asm__ __volatile__("bsrl %1,%0" : "=r" (Mask),"=m" ((*(volatile long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanForward64(DWORD *Index,DWORD64 Mask) {
      __asm__ __volatile__("bsfq %1,%0" : "=r" (Mask),"=m" ((*(volatile long long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanReverse64(DWORD *Index,DWORD64 Mask) {
      __asm__ __volatile__("bsrq %1,%0" : "=r" (Mask),"=m" ((*(volatile long long *)Index)));
      return Mask!=0;
    }

#define InterlockedIncrement16 _InterlockedIncrement16
#define InterlockedDecrement16 _InterlockedDecrement16
#define InterlockedCompareExchange16 _InterlockedCompareExchange16

#define InterlockedAnd _InterlockedAnd
#define InterlockedOr _InterlockedOr
#define InterlockedXor _InterlockedXor
#define InterlockedIncrement _InterlockedIncrement
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedDecrement _InterlockedDecrement
#define InterlockedDecrementAcquire InterlockedDecrement
#define InterlockedDecrementRelease InterlockedDecrement
#define InterlockedAdd _InterlockedAdd
#define InterlockedExchange _InterlockedExchange
#define InterlockedExchangeAdd _InterlockedExchangeAdd
#define InterlockedCompareExchange _InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire InterlockedCompareExchange
#define InterlockedCompareExchangeRelease InterlockedCompareExchange

#define InterlockedAnd64 _InterlockedAnd64
#define InterlockedAndAffinity InterlockedAnd64
#define InterlockedOr64 _InterlockedOr64
#define InterlockedOrAffinity InterlockedOr64
#define InterlockedXor64 _InterlockedXor64
#define InterlockedIncrement64 _InterlockedIncrement64
#define InterlockedDecrement64 _InterlockedDecrement64
#define InterlockedAdd64 _InterlockedAdd64
#define InterlockedExchange64 _InterlockedExchange64
#define InterlockedExchangeAcquire64 InterlockedExchange64
#define InterlockedExchangeAdd64 _InterlockedExchangeAdd64
#define InterlockedCompareExchange64 _InterlockedCompareExchange64
#define InterlockedCompareExchangeAcquire64 InterlockedCompareExchange64
#define InterlockedCompareExchangeRelease64 InterlockedCompareExchange64

#define InterlockedExchangePointer _InterlockedExchangePointer
#define InterlockedCompareExchangePointer _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerAcquire _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerRelease _InterlockedCompareExchangePointer

#define InterlockedExchangeAddSizeT(a,b) InterlockedExchangeAdd64((LONG64 *)a,b)
#define InterlockedIncrementSizeT(a) InterlockedIncrement64((LONG64 *)a)
#define InterlockedDecrementSizeT(a) InterlockedDecrement64((LONG64 *)a)

    __CRT_INLINE SHORT InterlockedIncrement16(SHORT volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addw $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE SHORT InterlockedDecrement16(SHORT volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subw $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE SHORT InterlockedCompareExchange16(SHORT volatile *Destination,SHORT ExChange,SHORT Comperand) {
      SHORT prev;
      __asm__ __volatile__("lock ; cmpxchgw %w1,%2"
	:"=a"(prev)
	:"q"(ExChange), "m"(*Destination), "0"(Comperand)
	: "memory");
      return prev;
    }
    __CRT_INLINE LONG InterlockedAnd(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; andl %0,%1"
	: :"r"(Value),"m"(*Destination)
	: "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedOr(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; orl %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedXor(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; xorl %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    //		$$$$
    __CRT_INLINE LONG64 InterlockedAnd64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; andq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG64 InterlockedOr64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; orq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG64 InterlockedXor64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; xorq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedIncrement(LONG volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addl $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG InterlockedDecrement(LONG volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subl $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG InterlockedExchange(LONG volatile *Target,LONG Value) {
      __asm__ __volatile("lock ; xchgl %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }
    

    __CRT_INLINE LONG InterlockedCompareExchange(LONG volatile *Destination,LONG ExChange,LONG Comperand) {
      LONG prev;
      __asm__ __volatile__("lock ; cmpxchgl %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE LONG64 InterlockedIncrement64(LONG64 volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addq $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG64 InterlockedDecrement64(LONG64 volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subq $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG64 InterlockedExchange64(LONG64 volatile *Target,LONG64 Value) {
      __asm__ __volatile("lock ; xchgq %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }
    
  
      //LONG InterlockedExchangeAdd(LONG volatile *Addend,LONG Value);
    
    //cheat engine modification start
    LONG InterlockedExchangeAdd(LONG volatile *Addend,LONG Value)
    {
      LONG Old;

          do {
              Old = *Addend;
          } while (InterlockedCompareExchange(Addend,
                                                Old + Value,
                                                Old) != Old);

          return Old;      
    };


#ifndef _X86AMD64_
    __CRT_INLINE LONG InterlockedAdd(LONG volatile *Addend,LONG Value) { return InterlockedExchangeAdd(Addend,Value) + Value; }
#endif
    


    __CRT_INLINE LONG64 InterlockedCompareExchange64(LONG64 volatile *Destination,LONG64 ExChange,LONG64 Comperand) {
      LONG64 prev;
      __asm__ __volatile__("lock ; cmpxchgq %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE PVOID InterlockedCompareExchangePointer(PVOID volatile *Destination,PVOID ExChange,PVOID Comperand) {
      PVOID prev;
      __asm__ __volatile__("lock ; cmpxchgq %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE PVOID InterlockedExchangePointer(PVOID volatile *Target,PVOID Value) {
      __asm__ __volatile("lock ; xchgq %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }
    
    //cheat engine modification start
    LONG64 InterlockedExchangeAdd64(LONG64 volatile *Addend,LONG64 Value)
    {
      LONGLONG Old;

          do {
              Old = *Addend;
          } while (InterlockedCompareExchange64(Addend,
                                                Old + Value,
                                                Old) != Old);

          return Old;      
    };

#ifndef _X86AMD64_
    __CRT_INLINE LONG64 InterlockedAdd64(LONG64 volatile *Addend,LONG64 Value) { return InterlockedExchangeAdd64(Addend,Value) + Value; }
#endif 
   //cheat engine modification stop

#define CacheLineFlush(Address) _mm_clflush(Address)

    VOID _ReadWriteBarrier(VOID);

#define FastFence __faststorefence
#define LoadFence _mm_lfence
#define MemoryFence _mm_mfence
#define StoreFence _mm_sfence

    VOID __faststorefence(VOID);
    VOID _m_prefetchw(volatile CONST VOID *Source);

//!__TINYC__: #include <intrin.h>

#define YieldProcessor _mm_pause
#define MemoryBarrier __faststorefence
#define PreFetchCacheLine(l,a) _mm_prefetch((CHAR CONST *) a,l)
#define PrefetchForWrite(p) _m_prefetchw(p)
#define ReadForWriteAccess(p) (_m_prefetchw(p),*(p))

#define PF_TEMPORAL_LEVEL_1 _MM_HINT_T0
#define PF_TEMPORAL_LEVEL_2 _MM_HINT_T1
#define PF_TEMPORAL_LEVEL_3 _MM_HINT_T2
#define PF_NON_TEMPORAL_LEVEL_ALL _MM_HINT_NTA

#define ReadMxCsr _mm_getcsr
#define WriteMxCsr _mm_setcsr

    VOID __int2c(VOID);

#define DbgRaiseAssertionFailure() __int2c()
#define GetCallersEflags() __getcallerseflags()

    unsigned __int32 __getcallerseflags(VOID);

#define GetSegmentLimit __segmentlimit

    DWORD __segmentlimit(DWORD Selector);

#define ReadTimeStampCounter() __rdtsc()

    DWORD64 __rdtsc(VOID);
    VOID __movsb(PBYTE Destination,BYTE const *Source,SIZE_T Count);
    VOID __movsw(PWORD Destination,WORD const *Source,SIZE_T Count);
    VOID __movsd(PDWORD Destination,DWORD const *Source,SIZE_T Count);
    VOID __movsq(PDWORD64 Destination,DWORD64 const *Source,SIZE_T Count);
    VOID __stosb(PBYTE Destination,BYTE Value,SIZE_T Count);
    VOID __stosw(PWORD Destination,WORD Value,SIZE_T Count);
    VOID __stosd(PDWORD Destination,DWORD Value,SIZE_T Count);
    VOID __stosq(PDWORD64 Destination,DWORD64 Value,SIZE_T Count);

#define MultiplyHigh __mulh
#define UnsignedMultiplyHigh __umulh

    LONGLONG MultiplyHigh(LONGLONG Multiplier,LONGLONG Multiplicand);
    ULONGLONG UnsignedMultiplyHigh(ULONGLONG Multiplier,ULONGLONG Multiplicand);

#define ShiftLeft128 __shiftleft128
#define ShiftRight128 __shiftright128

    DWORD64 ShiftLeft128(DWORD64 LowPart,DWORD64 HighPart,BYTE Shift);
    DWORD64 ShiftRight128(DWORD64 LowPart,DWORD64 HighPart,BYTE Shift);

#ifndef cheatengine
#define Multiply128 _mul128

    LONG64 Multiply128(LONG64 Multiplier,LONG64 Multiplicand,LONG64 *HighProduct);

#define UnsignedMultiply128 _umul128

    DWORD64 UnsignedMultiply128(DWORD64 Multiplier,DWORD64 Multiplicand,DWORD64 *HighProduct);

    __CRT_INLINE LONG64 MultiplyExtract128(LONG64 Multiplier,LONG64 Multiplicand,BYTE Shift) {
      LONG64 extractedProduct;
      LONG64 highProduct;
      LONG64 lowProduct;
      lowProduct = Multiply128(Multiplier,Multiplicand,&highProduct);
      extractedProduct = (LONG64)ShiftRight128((LONG64)lowProduct,(LONG64)highProduct,Shift);
      return extractedProduct;
    }

    __CRT_INLINE DWORD64 UnsignedMultiplyExtract128(DWORD64 Multiplier,DWORD64 Multiplicand,BYTE Shift) {
      DWORD64 extractedProduct;
      DWORD64 highProduct;
      DWORD64 lowProduct;
      lowProduct = UnsignedMultiply128(Multiplier,Multiplicand,&highProduct);
      extractedProduct = ShiftRight128(lowProduct,highProduct,Shift);
      return extractedProduct;
    }

#endif //cheatengine  (Cheat Engine Modification)

    __CRT_INLINE BYTE __readgsbyte(DWORD Offset) {
      BYTE ret;
      __asm__ volatile ("movb	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE WORD __readgsword(DWORD Offset) {
      WORD ret;
      __asm__ volatile ("movw	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE DWORD __readgsdword(DWORD Offset) {
      DWORD ret;
      __asm__ volatile ("movl	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE DWORD64 __readgsqword(DWORD Offset) {
      void *ret;
      __asm__ volatile ("movq	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return (DWORD64) ret;
    }
    __CRT_INLINE VOID __writegsbyte(DWORD Offset,BYTE Data) {
      __asm__ volatile ("movb	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsword(DWORD Offset,WORD Data) {
      __asm__ volatile ("movw	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsdword(DWORD Offset,DWORD Data) {
      __asm__ volatile ("movl	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsqword(DWORD Offset,DWORD64 Data) {
      __asm__ volatile ("movq	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }

#ifdef __cplusplus
  }
#endif
#endif

#define EXCEPTION_READ_FAULT 0
#define EXCEPTION_WRITE_FAULT 1
#define EXCEPTION_EXECUTE_FAULT 8

#if !defined(RC_INVOKED)

#define CONTEXT_AMD64 0x100000

#define CONTEXT_CONTROL (CONTEXT_AMD64 | 0x1L)