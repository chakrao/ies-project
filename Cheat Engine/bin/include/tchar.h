
/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#include <_mingw.h>

#ifndef _INC_TCHAR
#define _INC_TCHAR

#ifdef _STRSAFE_H_INCLUDED_
#error Need to include strsafe.h after tchar.h
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define _ftcscat _tcscat
#define _ftcschr _tcschr
#define _ftcscpy _tcscpy
#define _ftcscspn _tcscspn
#define _ftcslen _tcslen
#define _ftcsncat _tcsncat
#define _ftcsncpy _tcsncpy
#define _ftcspbrk _tcspbrk
#define _ftcsrchr _tcsrchr
#define _ftcsspn _tcsspn
#define _ftcsstr _tcsstr
#define _ftcstok _tcstok

#define _ftcsdup _tcsdup
#define _ftcsnset _tcsnset
#define _ftcsrev _tcsrev
#define _ftcsset _tcsset

#define _ftcscmp _tcscmp
#define _ftcsicmp _tcsicmp
#define _ftcsnccmp _tcsnccmp
#define _ftcsncmp _tcsncmp
#define _ftcsncicmp _tcsncicmp
#define _ftcsnicmp _tcsnicmp

#define _ftcscoll _tcscoll
#define _ftcsicoll _tcsicoll
#define _ftcsnccoll _tcsnccoll
#define _ftcsncoll _tcsncoll
#define _ftcsncicoll _tcsncicoll
#define _ftcsnicoll _tcsnicoll

#define _ftcsclen _tcsclen
#define _ftcsnccat _tcsnccat
#define _ftcsnccpy _tcsnccpy
#define _ftcsncset _tcsncset

#define _ftcsdec _tcsdec
#define _ftcsinc _tcsinc
#define _ftcsnbcnt _tcsnbcnt
#define _ftcsnccnt _tcsnccnt
#define _ftcsnextc _tcsnextc
#define _ftcsninc _tcsninc
#define _ftcsspnp _tcsspnp

#define _ftcslwr _tcslwr
#define _ftcsupr _tcsupr

#define _ftclen _tclen
#define _ftccpy _tccpy
#define _ftccmp _tccmp

#ifndef _CONST_RETURN
#ifdef __cplusplus
#define _CONST_RETURN const
#define _CRT_CONST_CORRECT_OVERLOADS
#else
#define _CONST_RETURN
#endif
#endif

#define _WConst_return _CONST_RETURN

#ifdef _UNICODE

#ifdef __cplusplus
}
#endif

#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WCTYPE_T_DEFINED
#define _WCTYPE_T_DEFINED
  typedef unsigned short wint_t;
  typedef unsigned short wctype_t;
#endif

#ifndef __TCHAR_DEFINED
#define __TCHAR_DEFINED
  typedef wchar_t _TCHAR;
  typedef wchar_t _TSCHAR;
  typedef wchar_t _TUCHAR;
  typedef wchar_t _TXCHAR;
  typedef wint_t _TINT;
#endif

#ifndef _TCHAR_DEFINED
#define _TCHAR_DEFINED
#ifndef	NO_OLDNAMES
  typedef wchar_t TCHAR;
#endif
#endif

#define _TEOF WEOF

#define __T(x) L##x

#define _tmain wmain
#define _tWinMain wWinMain
#define _tenviron _wenviron
#define __targv __wargv

#define _tprintf wprintf
#define _tprintf_l _wprintf_l
#define _tprintf_p _wprintf_p
#define _tprintf_p_l _wprintf_p_l
#define _tcprintf _cwprintf
#define _tcprintf_l _cwprintf_l
#define _tcprintf_p _cwprintf_p
#define _tcprintf_p_l _cwprintf_p_l
#define _vtcprintf _vcwprintf
#define _vtcprintf_l _vcwprintf_l
#define _vtcprintf_p _vcwprintf_p
#define _vtcprintf_p_l _vcwprintf_p_l
#define _ftprintf fwprintf
#define _ftprintf_l _fwprintf_l
#define _ftprintf_p _fwprintf_p
#define _ftprintf_p_l _fwprintf_p_l
#define _stprintf swprintf
#define _stprintf_l __swprintf_l
#define _stprintf_p _swprintf_p
#define _stprintf_p_l _swprintf_p_l
#define _sctprintf _scwprintf
#define _sctprintf_l _scwprintf_l
#define _sctprintf_p _scwprintf_p
#define _sctprintf_p_l _scwprintf_p_l
#define _sntprintf _snwprintf
#define _sntprintf_l _snwprintf_l
#define _vtprintf vwprintf
#define _vtprintf_l _vwprintf_l
#define _vtprintf_p _vwprintf_p
#define _vtprintf_p_l _vwprintf_p_l
#define _vftprintf vfwprintf
#define _vftprintf_l _vfwprintf_l
#define _vftprintf_p _vfwprintf_p
#define _vftprintf_p_l _vfwprintf_p_l
#define _vstprintf vswprintf
#define _vstprintf_l _vswprintf_l
#define _vstprintf_p _vswprintf_p
#define _vstprintf_p_l _vswprintf_p_l
#define _vsctprintf _vscwprintf
#define _vsctprintf_l _vscwprintf_l
#define _vsctprintf_p _vscwprintf_p
#define _vsctprintf_p_l _vscwprintf_p_l
#define _vsntprintf _vsnwprintf
#define _vsntprintf_l _vsnwprintf_l

#define _tscanf wscanf
#define _tscanf_l _wscanf_l
#define _tcscanf _cwscanf
#define _tcscanf_l _cwscanf_l
#define _ftscanf fwscanf
#define _ftscanf_l _fwscanf_l
#define _stscanf swscanf
#define _stscanf_l _swscanf_l
#define _sntscanf _snwscanf
#define _sntscanf_l _snwscanf_l

#define _fgettc fgetwc
#define _fgettc_nolock _fgetwc_nolock
#define _fgettchar _fgetwchar
#define _fgetts fgetws
#define _fputtc fputwc
#define _fputtc_nolock _fputwc_nolock
#define _fputtchar _fputwchar
#define _fputts fputws
#define _cputts _cputws
#define _cgetts _cgetws
#define _gettc getwc
#define _gettc_nolock _getwc_nolock
#define _gettch _getwch
#define _gettch_nolock _getwch_nolock
#define _gettche _getwche
#define _gettche_nolock _getwche_nolock
#define _gettchar getwchar
#define _gettchar_nolock _getwchar_nolock
#define _getts _getws
#define _puttc putwc
#define _puttc_nolock _putwc_nolock
#define _puttchar putwchar
#define _puttchar_nolock _putwchar_nolock
#define _puttch _putwch
#define _puttch_nolock _putwch_nolock
#define _putts _putws
#define _ungettc ungetwc
#define _ungettc_nolock _ungetwc_nolock
#define _ungettch _ungetwch
#define _ungettch_nolock _ungetwch_nolock

#define _tcstod wcstod
#define _tcstol wcstol
#define _tcstoul wcstoul
#define _tcstoi64 _wcstoi64
#define _tcstoui64 _wcstoui64
#define _tstof _wtof
#define _tstol _wtol
#define _tstoi _wtoi
#define _tstoi64 _wtoi64
#define _tcstod_l _wcstod_l
#define _tcstol_l _wcstol_l
#define _tcstoul_l _wcstoul_l
#define _tcstoi64_l _wcstoi64_l
#define _tcstoui64_l _wcstoui64_l
#define _tstof_l _wtof_l
#define _tstol_l _wtol_l
#define _tstoi_l _wtoi_l
#define _tstoi64_l _wtoi64_l

#define _itot _itow
#define _ltot _ltow
#define _ultot _ultow
#define _ttoi _wtoi
#define _ttol _wtol

#define _ttoi64 _wtoi64
#define _i64tot _i64tow
#define _ui64tot _ui64tow

#define _tcscat wcscat
#define _tcschr wcschr
#define _tcscpy wcscpy
#define _tcscspn wcscspn
#define _tcslen wcslen
#define _tcsnlen wcsnlen
#define _tcsncat wcsncat
#define _tcsncat_l _wcsncat_l
#define _tcsncpy wcsncpy
#define _tcsncpy_l _wcsncpy_l
#define _tcspbrk wcspbrk
#define _tcsrchr wcsrchr
#define _tcsspn wcsspn
#define _tcsstr wcsstr
#define _tcstok wcstok
#define _tcstok_l _wcstok_l
#define _tcserror _wcserror
#define __tcserror __wcserror

#define _tcsdup _wcsdup
#define _tcsnset _wcsnset
#define _tcsnset_l _wcsnset_l
#define _tcsrev _wcsrev
#define _tcsset _wcsset
#define _tcsset_l _wcsset_l

#define _tcscmp wcscmp
#define _tcsicmp _wcsicmp
#define _tcsicmp_l _wcsicmp_l
#define _tcsnccmp wcsncmp
#define _tcsncmp wcsncmp
#define _tcsncicmp _wcsnicmp
#define _tcsncicmp_l _wcsnicmp_l
#define _tcsnicmp _wcsnicmp
#define _tcsnicmp_l _wcsnicmp_l

#define _tcscoll wcscoll
#define _tcscoll_l _wcscoll_l
#define _tcsicoll _wcsicoll
#define _tcsicoll_l _wcsicoll_l
#define _tcsnccoll _wcsncoll
#define _tcsnccoll_l _wcsncoll_l
#define _tcsncoll _wcsncoll
#define _tcsncoll_l _wcsncoll_l
#define _tcsncicoll _wcsnicoll
#define _tcsncicoll_l _wcsnicoll_l
#define _tcsnicoll _wcsnicoll
#define _tcsnicoll_l _wcsnicoll_l

#define _texecl _wexecl
#define _texecle _wexecle
#define _texeclp _wexeclp
#define _texeclpe _wexeclpe
#define _texecv _wexecv
#define _texecve _wexecve
#define _texecvp _wexecvp
#define _texecvpe _wexecvpe

#define _tspawnl _wspawnl
#define _tspawnle _wspawnle
#define _tspawnlp _wspawnlp
#define _tspawnlpe _wspawnlpe
#define _tspawnv _wspawnv
#define _tspawnve _wspawnve
#define _tspawnvp _wspawnvp
#define _tspawnvp _wspawnvp
#define _tspawnvpe _wspawnvpe

#define _tsystem _wsystem

#define _tasctime _wasctime
#define _tctime _wctime
#define _tctime32 _wctime32
#define _tctime64 _wctime64
#define _tstrdate _wstrdate
#define _tstrtime _wstrtime
#define _tutime _wutime
#define _tutime32 _wutime32
#define _tutime64 _wutime64
#define _tcsftime wcsftime
#define _tcsftime_l _wcsftime_l

#define _tchdir _wchdir
#define _tgetcwd _wgetcwd
#define _tgetdcwd _wgetdcwd
#define _tgetdcwd_nolock _wgetdcwd_nolock
#define _tmkdir _wmkdir
#define _trmdir _wrmdir

#define _tfullpath _wfullpath
#define _tgetenv _wgetenv
#define _tmakepath _wmakepath
#define _tpgmptr _wpgmptr
#define _get_tpgmptr _get_wpgmptr
#define _tputenv _wputenv
#define _tsearchenv _wsearchenv
#define _tsplitpath _wsplitpath

#define _tfdopen _wfdopen
#define _tfsopen _wfsopen
#define _tfopen _wfopen
#define _tfreopen _wfreopen
#define _tperror _wperror
#define _tpopen _wpopen
#define _ttempnam _wtempnam
#define _ttmpnam _wtmpnam

#define _taccess _waccess
#define _tchmod _wchmod
#define _tcreat _wcreat
#define _tfindfirst _wfindfirst
#define _tfindfirst32 _wfindfirst32
#define _tfindfirst64 _wfindfirst64
#define _tfindfirsti64 _wfindfirsti64
#define _tfindfirst32i64 _wfindfirst32i64
#define _tfindfirst64i32 _wfindfirst64i32
#define _tfindnext _wfindnext