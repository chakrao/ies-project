
#ifndef COMMON_H_
#define COMMON_H_

#include <stddef.h>


//#define DELAYEDSERIAL

//#define USENMIFORWAIT
#define AMDNP  //enable AMD nested paging support

#ifdef DELAYEDSERIAL
extern int useserial;
#endif

#define STATISTICS

//#define TSCHOOK

#define MAX_STACK_SIZE 0x10000

#if (defined SERIALPORT) && (SERIALPORT != 0)
  #define DEBUG //comment for release
  #define DEBUGINTHANDLER //comment for release
  #define CHECKAPICID
#endif

#if (DISPLAYDEBUG==1)
  #define DEBUG
  #define DEBUGINTHANDLER
  #define CHECKAPICID
#endif



#define ULTIMAPDEBUG //for debugging ultimap (I seem to have misplaced my serial port...)

#define EXIT_FAILURE 0xffffffff
#define EXIT_SUCCESS 0

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)
#define SHRT_MAX __SHRT_MAX__
#define SCHAR_MAX __SCHAR_MAX__
#define UCHAR_MAX (SCHAR_MAX * 2 + 1)


#define BYTE unsigned char
#define WORD unsigned short int
#define ULONG unsigned int
#define DWORD unsigned int
#define UINT32 unsigned int
#define UINT64 unsigned long long
#define QWORD UINT64
#ifndef NULL
#define NULL (void*)0
#endif

typedef int VMSTATUS;
#define VM_OK 0
#define VM_ERROR 1


typedef int BOOL;

#define TRUE 1
#define FALSE 0

#define UNUSED __attribute__((unused))

typedef struct{
  QWORD RIP;
  QWORD RBP;
  QWORD RBX;
  QWORD R12;
  QWORD R13;
  QWORD R14;
  QWORD R15;
  QWORD RFLAGS;
  QWORD RSP;
} _jmp_buf;

typedef _jmp_buf volatile jmp_buf[1]; //array holding one item, RSP

extern void longjmp(jmp_buf env, int val);
extern int setjmp(jmp_buf env);

#define try { int lastexception; jmp_buf previousexception; previousexception[0]=getcpuinfo()->OnException[0]; if ((lastexception=setjmp(getcpuinfo()->OnException))==0) {
#define except } else { QWORD UNUSED ExceptionRIP=getcpuinfo()->LastExceptionRIP;
#define tryend } getcpuinfo()->OnException[0]=previousexception[0]; }

typedef volatile struct _criticalSection
{