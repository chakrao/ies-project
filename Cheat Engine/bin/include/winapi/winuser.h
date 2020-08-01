/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _WINUSER_
#define _WINUSER_

#define WINUSERAPI DECLSPEC_IMPORT

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WINVER
#define WINVER 0x0502
#endif

#include <stdarg.h>

#ifndef NOUSER
  typedef HANDLE HDWP;
  typedef VOID MENUTEMPLATEA;
  typedef VOID MENUTEMPLATEW;
  typedef PVOID LPMENUTEMPLATEA;
  typedef PVOID LPMENUTEMPLATEW;

#ifdef UNICODE
  typedef MENUTEMPLATEW MENUTEMPLATE;
  typedef LPMENUTEMPLATEW LPMENUTEMPLATE;
#else
  typedef MENUTEMPLATEA MENUTEMPLATE;
  typedef LPMENUTEMPLATEA LPMENUTEMPLATE;
#endif

  typedef LRESULT (CALLBACK *WNDPROC)(HWND,UINT,WPARAM,LPARAM);
  typedef INT_PTR (CALLBACK *DLGPROC)(HWND,UINT,WPARAM,LPARAM);
  typedef VOID (CALLBACK *TIMERPROC)(HWND,UINT,UINT_PTR,DWORD);
  typedef WINBOOL (CALLBACK *GRAYSTRINGPROC)(HDC,LPARAM,int);
  typedef WINBOOL (CALLBACK *WNDENUMPROC)(HWND,LPARAM);
  typedef LRESULT (CALLBACK *HOOKPROC)(int code,WPARAM wParam,LPARAM lParam);
  typedef VOID (CALLBACK *SENDASYNCPROC)(HWND,UINT,ULONG_PTR,LRESULT);
  typedef WINBOOL (CALLBACK *PROPENUMPROCA)(HWND,LPCSTR,HANDLE);
  typedef WINBOOL (CALLBACK *PROPENUMPROCW)(HWND,LPCWSTR,HANDLE);
  typedef WINBOOL (CALLBACK *PROPENUMPROCEXA)(HWND,LPSTR,HANDLE,ULONG_PTR);
  typedef WINBOOL (CALLBACK *PROPENUMPROCEXW)(HWND,LPWSTR,HANDLE,ULONG_PTR);
  typedef int (CALLBACK *EDITWORDBREAKPROCA)(LPSTR lpch,int ichCurrent,int cch,int code);
  typedef int (CALLBACK *EDITWORDBREAKPROCW)(LPWSTR lpch,int ichCurrent,int cch,int code);
  typedef WINBOOL (CALLBACK *DRAWSTATEPROC)(HDC hdc,LPARAM lData,WPARAM wData,int cx,int cy);

#ifdef UNICODE
  typedef PROPENUMPROCW PROPENUMPROC;
  typedef PROPENUMPROCEXW PROPENUMPROCEX;
  typedef EDITWORDBREAKPROCW EDITWORDBREAKPROC;
#else
  typedef PROPENUMPROCA PROPENUMPROC;
  typedef PROPENUMPROCEXA PROPENUMPROCEX;
  typedef EDITWORDBREAKPROCA EDITWORDBREAKPROC;
#endif

  typedef WINBOOL (CALLBACK *NAMEENUMPROCA)(LPSTR,LPARAM);
  typedef WINBOOL (CALLBACK *NAMEENUMPROCW)(LPWSTR,LPARAM);
  typedef NAMEENUMPROCA WINSTAENUMPROCA;
  typedef NAMEENUMPROCA DESKTOPENUMPROCA;
  typedef NAMEENUMPROCW WINSTAENUMPROCW;
  typedef NAMEENUMPROCW DESKTOPENUMPROCW;

#ifdef UNICODE
  typedef WINSTAENUMPROCW WINSTAENUMPROC;
  typedef DESKTOPENUMPROCW DESKTOPENUMPROC;
#else
  typedef WINSTAENUMPROCA WINSTAENUMPROC;
  typedef DESKTOPENUMPROCA DESKTOPENUMPROC;
#endif

#define IS_INTRESOURCE(_r) ((((ULONG_PTR)(_r)) >> 16)==0)
#define MAKEINTRESOURCEA(i) ((LPSTR)((ULONG_PTR)((WORD)(i))))
#define MAKEINTRESOURCEW(i) ((LPWSTR)((ULONG_PTR)((WORD)(i))))
#ifdef UNICODE
#define MAKEINTRESOURCE MAKEINTRESOURCEW
#else
#define MAKEINTRESOURCE MAKEINTRESOURCEA
#endif

#ifndef NORESOURCE

#define RT_CURSOR MAKEINTRESOURCE(1)
#define RT_BITMAP MAKEINTRESOURCE(2)
#define RT_ICON MAKEINTRESOURCE(3)
#define RT_MENU MAKEINTRESOURCE(4)
#define RT_DIALOG MAKEINTRESOURCE(5)
#define RT_STRING MAKEINTRESOURCE(6)
#define RT_FONTDIR MAKEINTRESOURCE(7)
#define RT_FONT MAKEINTRESOURCE(8)
#define RT_ACCELERATOR MAKEINTRESOURCE(9)
#define RT_RCDATA MAKEINTRESOURCE(10)
#define RT_MESSAGETABLE MAKEINTRESOURCE(11)

#define DIFFERENCE 11
#define RT_GROUP_CURSOR MAKEINTRESOURCE((ULONG_PTR)RT_CURSOR + DIFFERENCE)
#define RT_GROUP_ICON MAKEINTRESOURCE((ULONG_PTR)RT_ICON + DIFFERENCE)
#define RT_VERSION MAKEINTRESOURCE(16)
#define RT_DLGINCLUDE MAKEINTRESOURCE(17)
#define RT_PLUGPLAY MAKEINTRESOURCE(19)
#define RT_VXD MAKEINTRESOURCE(20)
#define RT_ANICURSOR MAKEINTRESOURCE(21)
#define RT_ANIICON MAKEINTRESOURCE(22)
#define RT_HTML MAKEINTRESOURCE(23)
#ifdef RC_INVOKED
#define RT_MANIFEST 24
#define CREATEPROCESS_MANIFEST_RESOURCE_ID 1
#define ISOLATIONAWARE_MANIFEST_RESOURCE_ID 2
#define ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID 3
#define MINIMUM_RESERVED_MANIFEST_RESOURCE_ID 1
#define MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID 16
#else
#define RT_MANIFEST MAKEINTRESOURCE(24)
#define CREATEPROCESS_MANIFEST_RESOURCE_ID MAKEINTRESOURCE(1)
#define ISOLATIONAWARE_MANIFEST_RESOURCE_ID MAKEINTRESOURCE(2)
#define ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID MAKEINTRESOURCE(3)
#define MINIMUM_RESERVED_MANIFEST_RESOURCE_ID MAKEINTRESOURCE(1)
#define MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID MAKEINTRESOURCE(16)
#endif
#endif

#ifdef UNICODE
#define wvsprintf wvsprintfW
#define wsprintf wsprintfW
#else
#define wvsprintf wvsprintfA
#define wsprintf wsprintfA
#endif

  WINUSERAPI int WINAPI wvsprintfA(LPSTR,LPCSTR,va_list arglist);
  WINUSERAPI int WINAPI wvsprintfW(LPWSTR,LPCWSTR,va_list arglist);
  WINUSERAPI int WINAPIV wsprintfA(LPSTR,LPCSTR,...);
  WINUSERAPI int WINAPIV wsprintfW(LPWSTR,LPCWSTR,...);

#define SETWALLPAPER_DEFAULT ((LPWSTR)-1)

#ifndef NOSCROLL
#define SB_HORZ 0
#define SB_VERT 1
#define SB_CTL 2
#define SB_BOTH 3

#define SB_LINEUP 0
#define SB_LINELEFT 0
#define SB_LINEDOWN 1
#define SB_LINERIGHT 1
#define SB_PAGEUP 2
#define SB_PAGELEFT 2
#define SB_PAGEDOWN 3
#define SB_PAGERIGHT 3
#define SB_THUMBPOSITION 4
#define SB_THUMBTRACK 5
#define SB_TOP 6
#define SB_LEFT 6
#define SB_BOTTOM 7
#define SB_RIGHT 7
#define SB_ENDSCROLL 8
#endif

#ifndef NOSHOWWINDOW
#define SW_HIDE 0
#define SW_SHOWNORMAL 1
#define SW_NORMAL 1
#define SW_SHOWMINIMIZED 2
#define SW_SHOWMAXIMIZED 3
#define SW_MAXIMIZE 3
#define SW_SHOWNOACTIVATE 4
#define SW_SHOW 5
#define SW_MINIMIZE 6
#define SW_SHOWMINNOACTIVE 7
#define SW_SHOWNA 8
#define SW_RESTORE 9
#define SW_SHOWDEFAULT 10
#define SW_FORCEMINIMIZE 11
#define SW_MAX 11

#define HIDE_WINDOW 0
#define SHOW_OPENWINDOW 1
#define SHOW_ICONWINDOW 2
#define SHOW_FULLSCREEN 3
#define SHOW_OPENNOACTIVATE 4

#define SW_PARENTCLOSING 1
#define SW_OTHERZOOM 2
#define SW_PARENTOPENING 3
#define SW_OTHERUNZOOM 4
#endif

#define AW_HOR_POSITIVE 0x00000001
#define AW_HOR_NEGATIVE 0x00000002
#define AW_VER_POSITIVE 0x00000004
#define AW_VER_NEGATIVE 0x00000008
#define AW_CENTER 0x00000010
#define AW_HIDE 0x00010000
#define AW_ACTIVATE 0x00020000
#define AW_SLIDE 0x00040000
#define AW_BLEND 0x00080000

#define KF_EXTENDED 0x0100
#define KF_DLGMODE 0x0800
#define KF_MENUMODE 0x1000
#define KF_ALTDOWN 0x2000
#define KF_REPEAT 0x4000
#define KF_UP 0x8000

#ifndef NOVIRTUALKEYCODES

#define VK_LBUTTON 0x01
#define VK_RBUTTON 0x02
#define VK_CANCEL 0x03
#define VK_MBUTTON 0x04
#define VK_XBUTTON1 0x05
#define VK_XBUTTON2 0x06
#define VK_BACK 0x08
#define VK_TAB 0x09
#define VK_CLEAR 0x0C
#define VK_RETURN 0x0D
#define VK_SHIFT 0x10
#define VK_CONTROL 0x11
#define VK_MENU 0x12
#define VK_PAUSE 0x13
#define VK_CAPITAL 0x14
#define VK_KANA 0x15
#define VK_HANGEUL 0x15
#define VK_HANGUL 0x15
#define VK_JUNJA 0x17
#define VK_FINAL 0x18
#define VK_HANJA 0x19
#define VK_KANJI 0x19
#define VK_ESCAPE 0x1B
#define VK_CONVERT 0x1C
#define VK_NONCONVERT 0x1D
#define VK_ACCEPT 0x1E
#define VK_MODECHANGE 0x1F
#define VK_SPACE 0x20
#define VK_PRIOR 0x21
#define VK_NEXT 0x22
#define VK_END 0x23
#define VK_HOME 0x24
#define VK_LEFT 0x25
#define VK_UP 0x26
#define VK_RIGHT 0x27
#define VK_DOWN 0x28
#define VK_SELECT 0x29
#define VK_PRINT 0x2A
#define VK_EXECUTE 0x2B
#define VK_SNAPSHOT 0x2C
#define VK_INSERT 0x2D
#define VK_DELETE 0x2E
#define VK_HELP 0x2F

#define VK_LWIN 0x5B
#define VK_RWIN 0x5C
#define VK_APPS 0x5D
#define VK_SLEEP 0x5F
#define VK_NUMPAD0 0x60
#define VK_NUMPAD1 0x61
#define VK_NUMPAD2 0x62
#define VK_NUMPAD3 0x63
#define VK_NUMPAD4 0x64
#define VK_NUMPAD5 0x65
#define VK_NUMPAD6 0x66
#define VK_NUMPAD7 0x67
#define VK_NUMPAD8 0x68
#define VK_NUMPAD9 0x69
#define VK_MULTIPLY 0x6A
#define VK_ADD 0x6B
#define VK_SEPARATOR 0x6C
#define VK_SUBTRACT 0x6D
#define VK_DECIMAL 0x6E
#define VK_DIVIDE 0x6F
#define VK_F1 0x70
#define VK_F2 0x71
#define VK_F3 0x72
#define VK_F4 0x73
#define VK_F5 0x74
#define VK_F6 0x75
#define VK_F7 0x76
#define VK_F8 0x77
#define VK_F9 0x78
#define VK_F10 0x79
#define VK_F11 0x7A
#define VK_F12 0x7B
#define VK_F13 0x7C
#define VK_F14 0x7D
#define VK_F15 0x7E
#define VK_F16 0x7F
#define VK_F17 0x80
#define VK_F18 0x81
#define VK_F19 0x82
#define VK_F20 0x83
#define VK_F21 0x84
#define VK_F22 0x85
#define VK_F23 0x86
#define VK_F24 0x87
#define VK_NUMLOCK 0x90
#define VK_SCROLL 0x91
#define VK_OEM_NEC_EQUAL 0x92
#define VK_OEM_FJ_JISHO 0x92
#define VK_OEM_FJ_MASSHOU 0x93
#define VK_OEM_FJ_TOUROKU 0x94
#define VK_OEM_FJ_LOYA 0x95
#define VK_OEM_FJ_ROYA 0x96
#define VK_LSHIFT 0xA0
#define VK_RSHIFT 0xA1
#define VK_LCONTROL 0xA2
#define VK_RCONTROL 0xA3
#define VK_LMENU 0xA4
#define VK_RMENU 0xA5
#define VK_BROWSER_BACK 0xA6
#define VK_BROWSER_FORWARD 0xA7
#define VK_BROWSER_REFRESH 0xA8
#define VK_BROWSER_STOP 0xA9
#define VK_BROWSER_SEARCH 0xAA
#define VK_BROWSER_FAVORITES 0xAB
#define VK_BROWSER_HOME 0xAC
#define VK_VOLUME_MUTE 0xAD
#define VK_VOLUME_DOWN 0xAE
#define VK_VOLUME_UP 0xAF
#define VK_MEDIA_NEXT_TRACK 0xB0
#define VK_MEDIA_PREV_TRACK 0xB1
#define VK_MEDIA_STOP 0xB2
#define VK_MEDIA_PLAY_PAUSE 0xB3
#define VK_LAUNCH_MAIL 0xB4
#define VK_LAUNCH_MEDIA_SELECT 0xB5
#define VK_LAUNCH_APP1 0xB6
#define VK_LAUNCH_APP2 0xB7
#define VK_OEM_1 0xBA
#define VK_OEM_PLUS 0xBB
#define VK_OEM_COMMA 0xBC
#define VK_OEM_MINUS 0xBD
#define VK_OEM_PERIOD 0xBE
#define VK_OEM_2 0xBF
#define VK_OEM_3 0xC0
#define VK_OEM_4 0xDB
#define VK_OEM_5 0xDC
#define VK_OEM_6 0xDD
#define VK_OEM_7 0xDE
#define VK_OEM_8 0xDF
#define VK_OEM_AX 0xE1
#define VK_OEM_102 0xE2
#define VK_ICO_HELP 0xE3
#define VK_ICO_00 0xE4
#define VK_PROCESSKEY 0xE5
#define VK_ICO_CLEAR 0xE6
#define VK_PACKET 0xE7
#define VK_OEM_RESET 0xE9
#define VK_OEM_JUMP 0xEA
#define VK_OEM_PA1 0xEB
#define VK_OEM_PA2 0xEC
#define VK_OEM_PA3 0xED
#define VK_OEM_WSCTRL 0xEE
#define VK_OEM_CUSEL 0xEF
#define VK_OEM_ATTN 0xF0
#define VK_OEM_FINISH 0xF1
#define VK_OEM_COPY 0xF2
#define VK_OEM_AUTO 0xF3
#define VK_OEM_ENLW 0xF4
#define VK_OEM_BACKTAB 0xF5
#define VK_ATTN 0xF6
#define VK_CRSEL 0xF7
#define VK_EXSEL 0xF8
#define VK_EREOF 0xF9
#define VK_PLAY 0xFA
#define VK_ZOOM 0xFB
#define VK_NONAME 0xFC
#define VK_PA1 0xFD
#define VK_OEM_CLEAR 0xFE
#endif

#ifndef NOWH

#define WH_MIN (-1)
#define WH_MSGFILTER (-1)
#define WH_JOURNALRECORD 0
#define WH_JOURNALPLAYBACK 1
#define WH_KEYBOARD 2
#define WH_GETMESSAGE 3
#define WH_CALLWNDPROC 4
#define WH_CBT 5
#define WH_SYSMSGFILTER 6
#define WH_MOUSE 7
#define WH_HARDWARE 8
#define WH_DEBUG 9
#define WH_SHELL 10
#define WH_FOREGROUNDIDLE 11
#define WH_CALLWNDPROCRET 12

#define WH_KEYBOARD_LL 13
#define WH_MOUSE_LL 14

#define WH_MAX 14

#define WH_MINHOOK WH_MIN
#define WH_MAXHOOK WH_MAX

#define HC_ACTION 0
#define HC_GETNEXT 1
#define HC_SKIP 2
#define HC_NOREMOVE 3
#define HC_NOREM HC_NOREMOVE
#define HC_SYSMODALON 4
#define HC_SYSMODALOFF 5

#define HCBT_MOVESIZE 0
#define HCBT_MINMAX 1
#define HCBT_QS 2
#define HCBT_CREATEWND 3
#define HCBT_DESTROYWND 4
#define HCBT_ACTIVATE 5
#define HCBT_CLICKSKIPPED 6
#define HCBT_KEYSKIPPED 7
#define HCBT_SYSCOMMAND 8
#define HCBT_SETFOCUS 9

  typedef struct tagCBT_CREATEWNDA {
    struct tagCREATESTRUCTA *lpcs;
    HWND hwndInsertAfter;
  } CBT_CREATEWNDA,*LPCBT_CREATEWNDA;

  typedef struct tagCBT_CREATEWNDW {
    struct tagCREATESTRUCTW *lpcs;
    HWND hwndInsertAfter;
  } CBT_CREATEWNDW,*LPCBT_CREATEWNDW;
#ifdef UNICODE
  typedef CBT_CREATEWNDW CBT_CREATEWND;
  typedef LPCBT_CREATEWNDW LPCBT_CREATEWND;
#else
  typedef CBT_CREATEWNDA CBT_CREATEWND;
  typedef LPCBT_CREATEWNDA LPCBT_CREATEWND;
#endif

  typedef struct tagCBTACTIVATESTRUCT
  {
    WINBOOL fMouse;
    HWND hWndActive;
  } CBTACTIVATESTRUCT,*LPCBTACTIVATESTRUCT;

  typedef struct tagWTSSESSION_NOTIFICATION {
    DWORD cbSize;
    DWORD dwSessionId;

  } WTSSESSION_NOTIFICATION,*PWTSSESSION_NOTIFICATION;

#define WTS_CONSOLE_CONNECT 0x1
#define WTS_CONSOLE_DISCONNECT 0x2
#define WTS_REMOTE_CONNECT 0x3
#define WTS_REMOTE_DISCONNECT 0x4
#define WTS_SESSION_LOGON 0x5
#define WTS_SESSION_LOGOFF 0x6
#define WTS_SESSION_LOCK 0x7
#define WTS_SESSION_UNLOCK 0x8
#define WTS_SESSION_REMOTE_CONTROL 0x9

#define MSGF_DIALOGBOX 0
#define MSGF_MESSAGEBOX 1
#define MSGF_MENU 2
#define MSGF_SCROLLBAR 5
#define MSGF_NEXTWINDOW 6
#define MSGF_MAX 8
#define MSGF_USER 4096

#define HSHELL_WINDOWCREATED 1
#define HSHELL_WINDOWDESTROYED 2
#define HSHELL_ACTIVATESHELLWINDOW 3

#define HSHELL_WINDOWACTIVATED 4
#define HSHELL_GETMINRECT 5
#define HSHELL_REDRAW 6
#define HSHELL_TASKMAN 7
#define HSHELL_LANGUAGE 8
#define HSHELL_SYSMENU 9
#define HSHELL_ENDTASK 10
#define HSHELL_ACCESSIBILITYSTATE 11
#define HSHELL_APPCOMMAND 12
#define HSHELL_WINDOWREPLACED 13
#define HSHELL_WINDOWREPLACING 14
#define HSHELL_HIGHBIT 0x8000
#define HSHELL_FLASH (HSHELL_REDRAW|HSHELL_HIGHBIT)
#define HSHELL_RUDEAPPACTIVATED (HSHELL_WINDOWACTIVATED|HSHELL_HIGHBIT)

#define ACCESS_STICKYKEYS 0x0001
#define ACCESS_FILTERKEYS 0x0002
#define ACCESS_MOUSEKEYS 0x0003

#define APPCOMMAND_BROWSER_BACKWARD 1
#define APPCOMMAND_BROWSER_FORWARD 2
#define APPCOMMAND_BROWSER_REFRESH 3
#define APPCOMMAND_BROWSER_STOP 4
#define APPCOMMAND_BROWSER_SEARCH 5
#define APPCOMMAND_BROWSER_FAVORITES 6
#define APPCOMMAND_BROWSER_HOME 7
#define APPCOMMAND_VOLUME_MUTE 8
#define APPCOMMAND_VOLUME_DOWN 9
#define APPCOMMAND_VOLUME_UP 10
#define APPCOMMAND_MEDIA_NEXTTRACK 11
#define APPCOMMAND_MEDIA_PREVIOUSTRACK 12
#define APPCOMMAND_MEDIA_STOP 13
#define APPCOMMAND_MEDIA_PLAY_PAUSE 14
#define APPCOMMAND_LAUNCH_MAIL 15
#define APPCOMMAND_LAUNCH_MEDIA_SELECT 16
#define APPCOMMAND_LAUNCH_APP1 17
#define APPCOMMAND_LAUNCH_APP2 18
#define APPCOMMAND_BASS_DOWN 19
#define APPCOMMAND_BASS_BOOST 20
#define APPCOMMAND_BASS_UP 21
#define APPCOMMAND_TREBLE_DOWN 22
#define APPCOMMAND_TREBLE_UP 23
#define APPCOMMAND_MICROPHONE_VOLUME_MUTE 24
#define 