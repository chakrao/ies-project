
/*
 * api.c
 *
 *  Created on: Jul 20, 2011
 *      Author: erich
 *
 *      This unit will implement the api's CE uses
 *      This will be the main point of interest when porting to another system
 */

//#define _XOPEN_SOURCE 500

//todo for in the far future: Hook syscalls


#define TRACEPTRACE

#define _FILE_OFFSET_BITS 64
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#include <stdio.h>
#include <pthread.h>

#include <sys/mman.h>


#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <stdint.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/un.h>
#include <fcntl.h>
#include <unistd.h>

#include <strings.h>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <signal.h>

#ifdef __ANDROID__

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)
# include <string.h>    /* For prototype of `strlen'.  */

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif
#if defined (__arm__) || defined(__aarch64__)
#include <arm-linux-androideabi/asm/ptrace.h>
#endif
#endif

#ifndef __x86_64__
//#include <asm/signal.h>
#endif


#include <sys/eventfd.h>

#include <errno.h>

#include <semaphore.h>
#include <sys/queue.h>
#include <limits.h>

#include <sys/ptrace.h>

//#ifndef __x86_64__
#include <linux/elf.h>
//#include <linux/uio.h>
//#endif


#ifdef __arm__
  #ifndef __ANDROID__
    #include <linux/user.h>
  #endif
#endif


#include <dlfcn.h>

//blatantly stolen from the kernel source
#define PTRACE_GETHBPREGS 29
#define PTRACE_SETHBPREGS 30

/* Breakpoint */
#define ARM_BREAKPOINT_EXECUTE  0

/* Watchpoints */
#define ARM_BREAKPOINT_LOAD     1
#define ARM_BREAKPOINT_STORE    2

/* Privilege Levels */
#define ARM_BREAKPOINT_PRIV     1
#define ARM_BREAKPOINT_USER     2

/* Lengths */
#define ARM_BREAKPOINT_LEN_1    0x1
#define ARM_BREAKPOINT_LEN_2    0x3
#define ARM_BREAKPOINT_LEN_4    0xf
#define ARM_BREAKPOINT_LEN_8    0xff

#if defined (__arm__) || defined(__aarch64__)
static inline unsigned int encode_ctrl_reg(int mismatch, int len, int type, int privilege, int enabled)
{
        return (mismatch << 22) | (len << 5) | (type << 3) | (privilege << 1) | enabled;
}
#endif

#ifndef __ANDROID__
  #if defined(__i386__) || defined(__x86_64__)
    #include <sys/user.h>
  #endif
#endif



#include "api.h"
#include "porthelp.h"
#include "ceserver.h"
#include "threads.h"
#include "symbols.h"
#include "context.h"


PROCESS_VM_WRITEV process_vm_writev=NULL;
PROCESS_VM_READV process_vm_readv=NULL;

//#include <vector>
sem_t sem_DebugThreadEvent;

pthread_mutex_t memorymutex;
pthread_mutex_t debugsocketmutex;
//pthread_mutex_t mut_RPM;



int VerboseLevel=0;

int MEMORY_SEARCH_OPTION = 2; //0=file, 1=ptrace, 2=use process_vm_readv
int ATTACH_PID = 0;
int ATTACH_TO_ACCESS_MEMORY = 0;
int ATTACH_TO_WRITE_MEMORY = 1;
unsigned char SPECIFIED_ARCH = 9;

//Implementation for shared library version ceserver.
int debug_log(const char * format , ...)
{
  va_list list;
  va_start(list,format);
  int ret = vprintf(format,list);
  va_end(list);


  #ifdef __ANDROID__
  va_start(list,format);
  LOGD(format,list);
  va_end(list);
  #endif

  return ret;
}

#ifdef TRACEPTRACE
char *PTraceToString(int request)
{
  switch (request)
  {
    case PTRACE_ATTACH: return "PTRACE_ATTACH";
    case PTRACE_DETACH: return "PTRACE_DETACH";
    case PTRACE_PEEKDATA: return "PTRACE_PEEKDATA";
    case PTRACE_POKEDATA: return "PTRACE_POKEDATA";
    case PTRACE_CONT: return "PTRACE_CONT";
    case PTRACE_GETSIGINFO: return "PTRACE_GETSIGINFO";
    case PTRACE_GETREGSET: return "PTRACE_GETREGSET";
    case PTRACE_SINGLESTEP: return "PTRACE_SINGLESTEP";
    case PTRACE_SETREGS: return "PTRACE_SETREGS";
    case PTRACE_GETREGS: return "PTRACE_GETREGS";
#ifdef PT_GETFPXREGS
    case PTRACE_GETFPXREGS: return "PTRACE_GETFPXREGS";
#endif
    default:
      return "";

  }
}
#endif


//Implementation for consistency with Android Studio.
uintptr_t safe_ptrace(int request, pid_t pid, void * addr, void * data)
{
#ifdef TRACEPTRACE
 // debug_log("ATTACH_TO_ACCESS_MEMORY=%d\n", ATTACH_TO_ACCESS_MEMORY);
  if (threadname)
    debug_log("%s: ptrace called (%s(%x), %d, %p, %p)\n",threadname, PTraceToString(request),request, pid, addr, data);
  else
    debug_log("ptrace called (%s(%x), %d, %p, %p)\n",PTraceToString(request),request, pid, addr, data);


#endif
  uintptr_t result;
  errno = 0;
  result = ptrace(request, pid, addr, data);
  if(errno != 0)
  {
    debug_log("ptrace error(%s (%d))!\n",strerror(errno), errno);
  }
  return result;
}

int ptrace_attach_andwait(int pid)
//call this for quick attach/detach purposes. returns <0 on error, else the attached tid (usually just pid)
{
  if (safe_ptrace(PTRACE_ATTACH, pid,0,0)==0)
  {
    int status;
    while (1)
    {
      pid=waitpid(-1, &status,0);
      if (WIFSTOPPED(status))
      {
        if (WSTOPSIG(status)==SIGSTOP)
          return pid; //proper stop

        //not a sigstop
        debug_log("ptrace_attach_andwait:Received stop with signal %d instead of %d\n", WSTOPSIG(status), SIGSTOP);
        safe_ptrace(PTRACE_CONT, pid, (void*)0, (void*)(uint64_t)WSTOPSIG(status));
        continue;
      }

      if (WIFCONTINUED(status))
      {
        debug_log("ptrace_attach_andwait:It already continued?\n");
        continue;
      }

      if (WIFEXITED(status))
      {
        debug_log("ptrace_attach_andwait:Target terminated with code %d\n", WEXITSTATUS(status));
        return -2; //target exit
      }

      if (WIFSIGNALED(status))
      {
        debug_log("trace_attach_andwait:Target received a ");

        if (WTERMSIG(status))
          debug_log("terminate signal");

        if (WCOREDUMP(status))
          debug_log("core Dump");

        debug_log("\n");
        return -3;
      }


      debug_log("ptrace_attach_andwait: Unexpected status: %x\n", status);
      return -4;
    }

  }
  else
  {
    debug_log("ptrace_attach_andwait: ptrace attach failed\n");
    return -1; //ptrace attach failed
  }

}

int WakeDebuggerThread()
{
  return sem_post(&sem_DebugThreadEvent);
}

void mychildhandler(int signal, struct siginfo *info, void *context)
{
  //only call re-entrant functions

  int orig_errno = errno;
  WakeDebuggerThread();
  errno = orig_errno;
}

int windowsProtectionToLinux(uint32_t windowsprotection)
{
  int newprotection=0;
  switch (windowsprotection)
  {
    case PAGE_EXECUTE_READWRITE: newprotection=PROT_WRITE | PROT_READ | PROT_EXEC; break;
    case PAGE_EXECUTE_READ: newprotection=PROT_READ | PROT_EXEC; break;
    case PAGE_EXECUTE: newprotection=PROT_EXEC; break;
    case PAGE_READWRITE: newprotection=PROT_READ | PROT_WRITE; break;
    case PAGE_READONLY: newprotection=PROT_READ; break;
    default:
      newprotection=0;
  }

  return newprotection;
}

uint32_t linuxProtectionToWindows(int prot)
{
  int r=0, w=0, x=0;

  r=prot & PROT_READ;
  w=prot & PROT_WRITE;
  x=prot & PROT_EXEC;

  if (r && w && x)
    return PAGE_EXECUTE_READWRITE;

  if (r && x)
    return PAGE_EXECUTE_READ;

  if (x)
     return PAGE_EXECUTE;

  if (r && w)
    return PAGE_READWRITE;

  if (r)
    return PAGE_READONLY;

  return PAGE_NOACCESS;

}

int getArchitecture(HANDLE hProcess)
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    if (p->is64bit)
#if defined (__arm__) || defined(__aarch64__)
      return 3;
    else
      return 2;
#else
      return 1;
    else
      return 0;
#endif
  }

  return -1;
}

int GetDebugPort(HANDLE hProcess)
//return the debugserver fd
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    if (p->isDebugged)
    {
      return p->debuggerServer;
    }
  }

  return -1;

}

int FindPausedThread(PProcessData p)
/*
 * Scan the threadlist and return the threadid of the thread that is paused. Return 0 if no thread is paused
 */
{
  int i;
  for (i=0; i<p->threadlistpos; i++)
    if (p->threadlist[i].isPaused)
      return p->threadlist[i].tid;

  return 0;
}

int getBreakpointCapabilities(int tid, uint8_t *maxBreakpointCount, uint8_t *maxWatchpointCount, uint8_t *maxSharedBreakpoints)
//Only called when the thread is suspended
{

  *maxBreakpointCount=0;
  *maxWatchpointCount=0;
  *maxSharedBreakpoints=0;


#ifdef __arm__
  HBP_RESOURCE_INFO hwbpcap;

  memset(&hwbpcap, 0, sizeof(HBP_RESOURCE_INFO));
  if (safe_ptrace(PTRACE_GETHBPREGS, tid, 0, &hwbpcap)==0)
  {

    *maxBreakpointCount=hwbpcap.num_brps;
    *maxWatchpointCount=hwbpcap.num_wrps;
    *maxSharedBreakpoints=0;

    return 1;
  }
  else
    return 0;
#endif

#ifdef __aarch64__
  struct iovec iov;
  struct user_hwdebug_state hwd;
  memset(&hwd, 0, sizeof(hwd));

  iov.iov_base=&hwd;
  iov.iov_len=sizeof(hwd);

  if (safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_ARM_HW_WATCH, &iov)==0)
  {
    debug_log("NT_ARM_HW_WATCH: dbg_info=%x:\n", hwd.dbg_info);
    *maxWatchpointCount=hwd.dbg_info & 0xf;
  }
  else
  {
    debug_log("NT_ARM_HW_WATCH: Failure getting watch breakpoint information\n");
    *maxWatchpointCount=0;
  }

  iov.iov_base=&hwd;
  iov.iov_len=sizeof(hwd);
  if (safe_ptrace(PTRACE_GETREGSET, tid, (void*)NT_ARM_HW_BREAK, &iov)==0)
  {
    debug_log("NT_ARM_HW_BREAK: dbg_info=%x:\n", hwd.dbg_info);
    *maxBreakpointCount=hwd.dbg_info & 0xf;
  }
  else
  {
    debug_log("NT_ARM_HW_BREAK: Failure getting breakpoint information\n");
    *maxBreakpointCount=0;
  }

  return 1;
#endif

#if defined(__i386__) || defined(__x86_64__)
  *maxBreakpointCount=0;
  *maxWatchpointCount=0;
  *maxSharedBreakpoints=4;
  return 1;
#endif
}

int StartDebug(HANDLE hProcess)
{
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    struct sigaction childactionhandler;
    if (p->isDebugged)
    {
      debug_log("Trying to start debugging a process that is already debugged. (Close ceserver and try again if you had to force close CE earlier)\n");
    }

    //attach to each task
    //first get a threadlist
    InitializeProcessThreadlist(p);

    //setup an event
    memset(&childactionhandler, 0, sizeof(childactionhandler));
    childactionhandler.sa_handler=(void *)mychildhandler;
    childactionhandler.sa_flags=SA_SIGINFO;



    sigaction(SIGCHLD, &childactionhandler, NULL);




    //read the taskid's from /proc/pid/task
    char _taskdir[255];
    DIR *taskdir;

    sprintf(_taskdir, "/proc/%d/task", p->pid);

    taskdir=opendir(_taskdir);

    if (taskdir)
    {
      struct dirent *d;

      d=readdir(taskdir);
      while (d)
      {
        int tid=atoi(d->d_name);

        if (tid)
        {
          ThreadData td;
          memset(&td, 0, sizeof(td));
          td.tid=tid;
          td.isPaused=0;
          AddThreadToProcess(p, &td);

          pthread_mutex_lock(&memorymutex); //so there's no ptrace_attach busy when attaching after opening and reading memory


          if (safe_ptrace(PTRACE_ATTACH, tid,0,0)<0)
            debug_log("Failed to attach to thread %d\n", tid);
          else
          {
            DebugEvent createThreadEvent;



            if (p->isDebugged==0)
            {
              p->isDebugged=1; //at least one made it...
              p->debuggedThreadEvent.threadid=0; //none yet
              p->debuggerThreadID=pthread_self();

              threadname="CEServer Debugger Thread";

              socketpair(PF_LOCAL, SOCK_STREAM, 0, &p->debuggerServer);  //also sets debuggerClient

              //first event, create process
              DebugEvent createProcessEvent;

#if defined(__arm__) || defined (__aarch64__)

              if (WaitForDebugEventNative(p, &createProcessEvent, tid, -1))
              {
                //get the debug capabilities

                getBreakpointCapabilities(tid, &createProcessEvent.maxBreakpointCount, &createProcessEvent.maxWatchpointCount, &createProcessEvent.maxSharedBreakpoints);

                debug_log("hwbpcap:\n");
                debug_log("number of instruction breakpoints: %d\n", createProcessEvent.maxBreakpointCount);
                debug_log("number of data breakpoints:        %d\n", createProcessEvent.maxWatchpointCount);

                safe_ptrace(PTRACE_CONT, createProcessEvent.threadid, 0,0);

                PThreadData _td=GetThreadData(p, createProcessEvent.threadid);

                if (_td)
                  _td->isPaused=0;
                else
                  debug_log("Invalid first debug thread\n");
              }
              else
              {
                debug_log("Failure waiting for create event");
                createProcessEvent.maxBreakpointCount=0;
                createProcessEvent.maxWatchpointCount=0;
                createProcessEvent.maxSharedBreakpoints=4;
              }


#endif

#if defined(__i386__) || defined(__x86_64__)
              //4 breakpoints, hybrid
              createProcessEvent.maxBreakpointCount=0;
              createProcessEvent.maxWatchpointCount=0;
              createProcessEvent.maxSharedBreakpoints=4;
#endif

              createProcessEvent.debugevent=-2; //create process
              createProcessEvent.threadid=p->pid;

              AddDebugEventToQueue(p, &createProcessEvent);

            }

            createThreadEvent.debugevent=-1; //create thread event (virtual event)
            createThreadEvent.threadid=tid;
            AddDebugEventToQueue(p, &createThreadEvent);


           // p->debuggerThreadID=syscall(SYS_gettid);
          }

          pthread_mutex_unlock(&memorymutex);

        }

        d=readdir(taskdir);
      }

      closedir(taskdir);

    }
    else
    {
      debug_log("Failure opening %s",_taskdir);
    }


    return p->isDebugged;

  }
  else
  {
    //printf("Invalid handle\n");
    return FALSE;
  }

}

int SetBreakpoint(HANDLE hProcess, int tid, int debugreg, void *address, int bptype, int bpsize)
/*
 * Sets a breakpoint of the specifed type at the given address
 * tid of -1 means ALL threads
 * bptype: 0 = Execute
 * bptype: 1 = write
 * bptype: 2 = read (only)
 * bptype: 3 = Read/Write
 *
 * returns TRUE if the breakpoint was set *
 */
{
  int result=FALSE;


  debug_log("SetBreakpoint(%d, %d, %d, %p, %d, %d)\n", hProcess, tid, debugreg, address, bptype, bpsize);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



    if (p->debuggerThreadID==pthread_self())
    {

      int isdebugged=FindPausedThread(p);
      int wtid;
      DebugEvent de;



      debug_log("SetBreakpoint from debuggerthread\n");

      if (tid==-1)
      {
        int i,r;
        debug_log("Calling SetBreakpoint for all threads\n");

        for (i=0; i<p->threadlistpos; i++)
        {
          r=SetBreakpoint(hProcess, p->threadlist[i].tid, debugreg, address, bptype, bpsize);
          if (r)
            result=TRUE; //at least one thread succeeded
        }

      }
      else
      {
        PThreadData td=GetThreadData(p, tid);
        int wasPaused;

        if (td==NULL) //should NEVER happen
          return FALSE;

        wasPaused=td->isPaused;

        debug_log("Calling setbreakpoint for thread :%d\n", tid);

        debug_log("isdebugged=%d\n", isdebugged);

        if (wasPaused==0)
        {
          //manual
          debug_log("Target thread wasn't stopped yet\n");

          int k=0;

          debug_log("td=%p\n", td);
          debug_log("td->isPaused=%d\n", td->isPaused);




          wtid=tid;
          while ((td) && (td->isPaused==0) && (k<10))
          {
            debug_log("Not yet paused\n");
            syscall(__NR_tkill, tid, SIGSTOP);

            if (WaitForDebugEventNative(p, &de, tid, 100))
            {
              wtid=de.threadid;
              break;
            }
            k++;
          }



          if (wtid!=tid)
          {
            debug_log("<<<================UNEXPECTED TID (wtid=%d tid=%d)================>>>\n", wtid, tid);
          }

          debug_log("k=%d (number of tries)\n", k);

          if (k==10)
          {
            debug_log("Timeout when waiting for thread\n");
          }
        }
        else
        {
          debug_log("The thread I wanted to break was already broken. Yeeeh\n");
          wtid=isdebugged;
        }

        //debugging the given tid
        debug_log("Setting breakpoint in thread %d\n", wtid);
#ifdef __aarch64__
        struct user_pt_regs regset;

        int armbpsize=ARM_BREAKPOINT_LEN_4;

        struct iovec iov;
        int maxWatchCount=0;
        int maxBreakCount=0;

        debug_log("ceserver compiled for aarch64\n");

        memset(&regset, 0, sizeof(regset));
        memset(&iov, 0, sizeof(iov));
        iov.iov_base=&regset;
        iov.iov_len=sizeof(regset);
        int i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)NT_PRSTATUS, &iov);

        debug_log("PTRACE_GETREGSET returned %d\n", i);
        debug_log("iov.iov_len=%d\n", (int)iov.iov_len);  //272=64 bit app. 72=32 bit app


        if (iov.iov_len==72)
        {
          struct pt_regs32 {
            uint32_t uregs[18];
          };

          struct pt_regs32 *regset32; //can't use struct pt_regs as that uses long, which is 8 bytes

          regset32=iov.iov_base;
          debug_log("This is a 32 bit target\n");

          debug_log("pc=%x\n", regset32->ARM_pc);
          debug_log("r0 orig=%x\n", regset32->ARM_ORIG_r0);
          debug_log("r0=%x\n", regset32->ARM_r0);
          debug_log("r1=%x\n", regset32->ARM_r1);
          debug_log("r2=%x\n", regset32->ARM_r2);

          int i;
          for (i=0; i<18; i++)
            debug_log("uregs[%d]=%x\n", i, regset32->uregs[i]);

          if (((uintptr_t)address) & 1) //CE uses bit 1 to tell ceserver it's thumb
          {
            armbpsize=ARM_BREAKPOINT_LEN_2;
            address=(void *)((uintptr_t)address & 0xfffffffe); //ce might have set a bit
          }

        }
        else
        {
          debug_log("pc=%lx\n", regset.pc);
          debug_log("x0=%llx\n", regset.regs[0]);
          debug_log("x1=%llx\n", regset.regs[1]);
          debug_log("x2=%llx\n", regset.regs[2]);
          debug_log("x3=%llx\n", regset.regs[3]);
        }




        struct user_hwdebug_state hwd;
        memset(&hwd, 0, sizeof(hwd));


        iov.iov_base=&hwd;
        iov.iov_len=sizeof(hwd);
        i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)NT_ARM_HW_WATCH, &iov);

        if (i==0)
          maxWatchCount=hwd.dbg_info & 0xf;

        debug_log("iov.iov_len=%d\n", (int)iov.iov_len);  //272=64 bit app. 72=32 bit app
        debug_log("PTRACE_GETREGSET for NT_ARM_HW_WATCH returned %d %d\n", i,errno);

        iov.iov_base=&hwd;
        iov.iov_len=sizeof(hwd);
        i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)NT_ARM_HW_BREAK, &iov);
        if (i==0)
          maxBreakCount=hwd.dbg_info & 0xf;

        debug_log("PTRACE_GETREGSET for NT_ARM_HW_BREAK returned %d %d\n", i,errno);

        debug_log("iov.iov_len=%d\n", (int)iov.iov_len);  //272=64 bit app. 72=32 bit app



        int btype=0;
        int bplist=NT_ARM_HW_BREAK;

        int listsize;

        if (bptype==0)
        {
          //execute bp
          debug_log("Execute BP\n");
          bplist=NT_ARM_HW_BREAK;

          btype=ARM_BREAKPOINT_EXECUTE;
          listsize=maxBreakCount;

          //armbpsize is already set properly


        }
        else
        {
          //watchpoint
          debug_log("Watchpoint\n");
          bplist=NT_ARM_HW_WATCH;
          if (bptype==1)
            btype=ARM_BREAKPOINT_STORE;
          else
          if (bptype==2)
            btype=ARM_BREAKPOINT_LOAD;
          else
          if (bptype==3)
            btype=ARM_BREAKPOINT_STORE | ARM_BREAKPOINT_LOAD;

          listsize=maxWatchCount;


          //watchpoints have variable sizes
          if (bpsize<=1)
            armbpsize=ARM_BREAKPOINT_LEN_1;
          else if (bpsize<=2)
            armbpsize=ARM_BREAKPOINT_LEN_2;
          else if (bpsize<=4)
            armbpsize=ARM_BREAKPOINT_LEN_4;
          else
            armbpsize=ARM_BREAKPOINT_LEN_8;
        }


        debug_log("Caling PTRACE_GETREGSET for bplist %d\n", bplist);
        debug_log("iov.iov_len=%d\n", iov.iov_len);
        i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)(size_t)bplist, &iov);

        debug_log("PTRACE_GETREGSET returned %d\n", i);

        debug_log("debugreg=%d\n", debugreg);
        debug_log("Before:\n");

        iov.iov_len=8+16*listsize;

        for (i=0; i<listsize; i++)
        {
          if (hwd.dbg_regs[i].addr) //issue: PTRACE_GETREGSET bplist returns all debug registers as disabled.  Assume those with a proper address to not be disabled (so make sure to 0 the address when disabling)
          {
            if (bplist==NT_ARM_HW_BREAK)
              hwd.dbg_regs[i].ctrl=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_2, ARM_BREAKPOINT_EXECUTE, 0, 1);//hwd.dbg_regs[i].ctrl | 1; //encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_4, btype, 0, 1);
            else
              hwd.dbg_regs[i].ctrl=hwd.dbg_regs[i].ctrl | 1;
          }

          if (i==debugreg) debug_log("*");
          debug_log("%p - %x\n", (void*)hwd.dbg_regs[i].addr, hwd.dbg_regs[i].ctrl);
        }




        hwd.dbg_regs[debugreg].addr=(uintptr_t)address;
        hwd.dbg_regs[debugreg].ctrl=encode_ctrl_reg(0, armbpsize, btype, 0, 1);

        debug_log("setting hwd.dbg_regs[%d].addr to %p\n",debugreg, hwd.dbg_regs[debugreg].addr);
        debug_log("setting hwd.dbg_regs[%d].ctrl to %x\n",debugreg, hwd.dbg_regs[debugreg].ctrl);

        //iov.iov_len=8+16*(debugreg+1); //sizeof(hwd);



        debug_log("iov.iov_len=%d\n", iov.iov_len);


        debug_log("Caling PTRACE_SETREGSET\n", bplist);


        i=safe_ptrace(PTRACE_SETREGSET, wtid, (void*)(size_t)bplist, &iov);

        debug_log("set=%d",i);
        if (i==-1)
        {
          debug_log(" Error=%s",strerror(errno));

        }
        else
          result=TRUE; //success at least once

        debug_log("\n");

        memset(&hwd, 0, sizeof(hwd));

        i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)(size_t)bplist, &iov);

        debug_log("after:\n");
        if (bplist==NT_ARM_HW_BREAK)
        {
          int i;
          for (i=0; i<maxBreakCount; i++)
          {
            if (i==debugreg) debug_log("*");
            debug_log("%p - %x\n", (void*)hwd.dbg_regs[i].addr, hwd.dbg_regs[i].ctrl);
          }
        }
        else
        {
          int i;
          for (i=0; i<maxWatchCount; i++)
          {
            if (i==debugreg) debug_log("*");
            debug_log("%p - %x\n", (void*)hwd.dbg_regs[i].addr, hwd.dbg_regs[i].ctrl);
          }
        }


#endif

#ifdef __arm__
    //hwbps
        unsigned long long val;
        int bpindex=1+(2*debugreg);



        debug_log("PTRACE_GETHBPREGS=%d\n",PTRACE_GETHBPREGS);


        val=0;

        if (safe_ptrace(PTRACE_GETHBPREGS, wtid, 0, &val)==0)
        {
          int i;
          unsigned int hwbpreg;
          debug_log("BPREG0 (Info)=%x\n", val);

          debug_log("Setting bp address\n");



          if (bptype==0)
          {
            //execute
            void *rv=NULL;
           // safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex, &rv);
           // safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex+1, &rv);


            i=safe_ptrace(PTRACE_GETHBPREGS, wtid, bpindex, &rv);
            debug_log("%d: Before: %d=%p\n", i, bpindex, rv);

            i=safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex, &address);
            debug_log("i1=%d\n", i, hwbpreg);

            i=safe_ptrace(PTRACE_GETHBPREGS, wtid, bpindex, &rv);
            debug_log("%d: After: %d=%p\n", i, bpindex, rv);


            //right now i'm not really sure how the breakpoint len is set and why it works in some cases and why not in other cases
            result=i==0;

            hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_4, ARM_BREAKPOINT_EXECUTE, 2, 1);
            if (safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex+1, &hwbpreg)<0) //according to my guess, this should usually work, but just in case...
            {
              debug_log("f1\n");
              hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_2, ARM_BREAKPOINT_EXECUTE, 2, 1);
              if (safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex+1, &hwbpreg)<0)
              {
                debug_log("f2\n");
                hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_1, ARM_BREAKPOINT_EXECUTE, 2, 1);
                if (safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex+1, &hwbpreg)<0)
                {
                  debug_log("f3\n");
                  //last try, 8 ?
                  hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_8, ARM_BREAKPOINT_EXECUTE, 2, 1);
                  if (safe_ptrace(PTRACE_SETHBPREGS, wtid, bpindex+1, &hwbpreg)<0)
                  {
                    debug_log("Failure to set breakpoint\n");
                    result=FALSE;
                  }
                }

              }
            }

            debug_log("bpindex=%d bpindex+1=%d\n", bpindex, bpindex+1);

            debug_log("hwbpreg=%x\n", hwbpreg);

            i=safe_ptrace(PTRACE_GETHBPREGS, wtid, bpindex+1, &hwbpreg);
            debug_log("after=%x\n", hwbpreg);

          }
          else
          {
            //watchpoint
            //(negative)
            int btype;


            debug_log("watchpoint\n");

            i=safe_ptrace(PTRACE_SETHBPREGS, wtid, -bpindex, &address);
            debug_log("i1=%d\n", i, hwbpreg);

            btype=0;
            if (bptype==1)
              btype=ARM_BREAKPOINT_STORE;
            else
            if (bptype==2)
              btype=ARM_BREAKPOINT_LOAD;
            else
            if (bptype==3)
              btype=ARM_BREAKPOINT_STORE | ARM_BREAKPOINT_LOAD;

            hwbpreg=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_4, btype, 0, 1);
            i=safe_ptrace(PTRACE_SETHBPREGS, wtid, -(bpindex+1), &hwbpreg);

            debug_log("-bpindex=%d -(bpindex+1)=%d\n", -bpindex, -(bpindex+1));
          //  debug_log("i=%d  (hwbpreg=%x)\n", i, hwbpreg);
            result=i==0;

          }

        }
        else
          debug_log("Failure getting the debug capability register for thread %d (%d)\n", wtid, errno);


#endif

#if defined(__i386__) || defined(__x86_64__)
        //debug regs
        //PTRACE_SETREGS
        int r,r2;

        uintptr_t newdr7=safe_ptrace(PTRACE_PEEKUSER, wtid, (void *)(offsetof(struct user, u_debugreg[7])), 0);


        newdr7=newdr7 | (1<<debugreg*2);

        if (bptype==2) //x86 does not support read onlyhw bps
          bptype=3;

        newdr7=newdr7 | (bptype << (16+(debugreg*4))); //bptype

        debug_log("Setting DR7 to %x\n", newdr7);

        //bplen
        if (bpsize<=1)
          newdr7=newdr7 | (0 << (18+(debugreg*4)));
        else
        if (bpsize<=2)
          newdr7=newdr7 | (1 << (18+(debugreg*4)));
        else
          newdr7=newdr7 | (3 << (18+(debugreg*4)));


        r=safe_ptrace(PTRACE_POKEUSER, wtid, (void*)(offsetof(struct user, u_debugreg[debugreg])), address);
        r2=safe_ptrace(PTRACE_POKEUSER, wtid, (void*)(offsetof(struct user, u_debugreg[7])), (void*)newdr7);

        result=(r==0) && (r2==0);
        if (!result)
        {
          debug_log("Failure setting breakpoint\n");
        }

        debug_log("result=%d  (r=%d r2=%d)\n", result, r, r2);


#endif

        //store this breakpoint in the list


        if (wasPaused==0)
        {
          int r;

          debug_log("Continue self broken thread\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {
            debug_log("Not a SIGSTOP. Adding to queue and leave suspended\n");
            td->isPaused=1; //mark as paused for other api's
            AddDebugEventToQueue(p, &de);
          }
          else
          {
            td->isPaused=0;
            r=safe_ptrace(PTRACE_CONT, wtid, 0,0);
            debug_log("PTRACE_CONT=%d\n", r);
          }
        }
      }

    //

      debug_log("end of SetBreakpoint reached. result=%d\n", result);


    }
    else
    {
      //not the debugger thread. Send a message to the debuggerthread to execute this command
      debug_log("SetBreakpoint from outside the debuggerthread. Waking debuggerthread\n");
      //setup a sb command
  #pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
        int debugreg;
        uint64_t address;
        int bptype;
        int bpsize;
      } sb;
  #pragma pack()

      sb.command=CMD_SETBREAKPOINT;
      sb.hProcess=hProcess;
      sb.tid=tid;
      sb.debugreg=debugreg;
      sb.address=(uintptr_t)address;
      sb.bptype=bptype;
      sb.bpsize=bpsize;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        debug_log("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &sb, sizeof(sb), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        debug_log("Received reply from debugger thread: %d\n", result);


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }

  }
  else
  {
   // debug_log("Invalid handle\n");
  }

  return result;

}

int RemoveBreakpoint(HANDLE hProcess, int tid, int debugreg,int wasWatchpoint)
/*
 * Removes the breakpoint with the provided address
 */
{
  int result=FALSE;

  debug_log("RemoveBreakpoint(%d, %d, %d, %d)\n", hProcess, tid, debugreg, wasWatchpoint);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    if (p->isDebugged==0)
    {
      debug_log("The current process is not being debugged\n");
      return FALSE;
    }

    if (p->debuggerThreadID==pthread_self())
    {
      int isdebugged=p->debuggedThreadEvent.threadid;
      int wtid;
      DebugEvent de;

      debug_log("Called from the debuggerthread itself\n");

      if (tid==-1)
      {
        int i;
        debug_log("Calling RemoveBreakpoint for all threads\n");
        for (i=0; i<p->threadlistpos; i++)
        {
          if (RemoveBreakpoint(hProcess, p->threadlist[i].tid, debugreg, wasWatchpoint)==TRUE)
            result=TRUE;
        }
      }
      else
      {
        debug_log("specific thread\n");

        PThreadData td=GetThreadData(p, tid);
        int wasPaused=td->isPaused;

        if (wasPaused==0)
        {
          //manual
          debug_log("Not currently paused\n");
          debug_log("Going to kill and wait for this thread\n");

          int k;


          k=0;
          while ((td) && (td->isPaused==0) && (k<10))
          {
            syscall(__NR_tkill, tid, SIGSTOP);
            if (WaitForDebugEventNative(p, &de, tid, 100))
              break;
            k++;
          }

          wtid=de.threadid;


          debug_log("----AFTER WAIT----\n");

          debug_log("after wtid=%d\n", wtid);
          debug_log("^^^^AFTER WAIT^^^^\n");
        }
        else
        {
          debug_log("The thread I wanted to break was already broken. Yeeeh\n");
          wtid=isdebugged;
        }

        //debugging the given tid
        debug_log("Removing breakpoint from thread %d\n", wtid);


#ifdef __arm__
        int bpreg=0;
        int i,i2,i3;
        void *a=NULL;

        int bpIndex=1+(2*debugreg);

        debug_log("arm\n");

        if (wasWatchpoint)
        {
          i=safe_ptrace(PTRACE_SETHBPREGS, wtid, -bpIndex, &bpreg);
          i2=safe_ptrace(PTRACE_SETHBPREGS, wtid, -(bpIndex+1), &bpreg);
        }
        else
        {
          i=safe_ptrace(PTRACE_SETHBPREGS, wtid, bpIndex, &bpreg);
          i2=safe_ptrace(PTRACE_SETHBPREGS, wtid, bpIndex+1, &bpreg);
        }



        debug_log("i1=%d\n", i);

        debug_log("i2=%d\n", i2);



        i3=safe_ptrace(PTRACE_SETHBPREGS, wtid, 1, &a);

        result=(i==0) && (i2==0) && (i3==0);
#endif

#ifdef __aarch64__
        int i;
        struct user_hwdebug_state hwd;
        struct iovec iov;

        memset(&hwd, 0, sizeof(hwd));
        memset(&iov, 0, sizeof(iov));
        iov.iov_base=&hwd;
        iov.iov_len=sizeof(hwd);

        int bplist;

        if (wasWatchpoint)
          bplist=NT_ARM_HW_WATCH;
        else
          bplist=NT_ARM_HW_BREAK;

        i=safe_ptrace(PTRACE_GETREGSET, wtid, (void*)(size_t)bplist, &iov);
        if (i!=0)
          debug_log("PTRACE_GETREGSET failed\n");

        int listsize=hwd.dbg_info & 0xf;


        for (i=0; i<listsize; i++)
        {
          if (hwd.dbg_regs[i].addr)
          {
            if (bplist==NT_ARM_HW_BREAK)
              hwd.dbg_regs[i].ctrl=encode_ctrl_reg(0, ARM_BREAKPOINT_LEN_2, ARM_BREAKPOINT_EXECUTE, 0, 1);
            else
              hwd.dbg_regs[i].ctrl= hwd.dbg_regs[i].ctrl | 1;
          }

          if (i==debugreg) debug_log("*");
          debug_log("%p - %x\n", (void*)hwd.dbg_regs[i].addr, hwd.dbg_regs[i].ctrl);
        }




        if (debugreg<listsize)
        {
          hwd.dbg_regs[debugreg].addr=0;
          hwd.dbg_regs[debugreg].ctrl=0;

          iov.iov_len=8+16*(hwd.dbg_info & 0xf);

          i=safe_ptrace(PTRACE_SETREGSET, wtid, (void*)(size_t)bplist, (void*)&iov);
          if (i!=0)
            debug_log("PTRACE_SETREGSET failed :%s\n", strerror(errno));
        }
        else
        {
          debug_log("Error: Tried to remove a breakpoint index out of range\n");
          i=1000;
        }



        result=i;
#endif

#if defined(__i386__) || defined(__x86_64__)
        int r;
        uintptr_t dr7=0;
        debug_log("x86\n");

        dr7=safe_ptrace(PTRACE_PEEKUSER, wtid, (void*)(offsetof(struct user, u_debugreg[7])), 0);

        dr7&=~(3 << (debugreg*2)); //disable G# and L#
        dr7&=~(15 << (16+debugreg*4)); //set len and type for this debugreg to 0


        r=safe_ptrace(PTRACE_POKEUSER, wtid, (void*)(offsetof(struct user, u_debugreg[debugreg])), 0);


        r=safe_ptrace(PTRACE_POKEUSER, wtid, (void*)(offsetof(struct user, u_debugreg[7])), (void*)dr7);
        if (r==0)
          result=TRUE;
        else
          debug_log("Failure removing breakpoint from thread %d\n", wtid);


#endif

        if (wasPaused==0)
        {
          int r;
          PThreadData td=GetThreadData(p, tid);

          debug_log("Continue self broken thread\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {
            debug_log("Not a SIGSTOP. Adding to queue and leave suspended\n");
            td->isPaused=1;
            AddDebugEventToQueue(p, &de);
          }
          else
          {
            r=safe_ptrace(PTRACE_CONT, wtid, 0,0);
            debug_log("PTRACE_CONT=%d\n", r);

            td->isPaused=0;
          }
        }


      }

    }
    else
    {
      debug_log("Called from a secondary thread\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
        int debugreg;
        int wasWatchpoint;
      } rb;
#pragma pack()

      rb.command=CMD_REMOVEBREAKPOINT;
      rb.hProcess=hProcess;
      rb.tid=tid;
      rb.debugreg=debugreg;
      rb.wasWatchpoint=wasWatchpoint;


      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {

        debug_log("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &rb, sizeof(rb), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }

  }
  else
    debug_log("Invalid handle\n");

  fflush(stdout);

  return result;
}

BOOL GetThreadContext(HANDLE hProcess, int tid, PCONTEXT Context)
/*
 * Gets the context of the given thread
 * Freezes/Resumes the thread for you if it isn't suspended yet

 */
{
  BOOL r=FALSE;
  debug_log("GetThreadContext(%d)\n", tid);



  if (tid<=0)
  {
    debug_log("Invalid tid\n");
    return FALSE;
  }

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);

    if (!p->isDebugged)
    {
      debug_log("GetThreadContext with no debugger attached\n");
      int pid=ptrace_attach_andwait(tid);
      int k=getContext(pid, Context);

      safe_ptrace(PTRACE_DETACH, pid,0,0);
      if (k==0)
        return TRUE;
      else
        return FALSE;
    }


    if (p->debuggerThreadID==pthread_self())
    {
      PThreadData td=GetThreadData(p, tid);

      debug_log("Inside debuggerthread\n");

      if (td)
      {
        DebugEvent de;
        int wasPaused=td->isPaused;
        int k=0;


        while ((td->isPaused==0) && (k<10))
        {
          debug_log("This thread was not paused. Pausing it\n");
          syscall(__NR_tkill, tid, SIGSTOP);
          if (WaitForDebugEventNative(p, &de, tid, 100))
            break;

          k++;
        }

        //the thread is paused, so fetch the data

        debug_log("Getting context of thread %d\n", tid);

        k=getContext(tid, Context);


        //k=safe_ptrace(PTRACE_GETREGS, tid, 0, &Context->regs);
        debug_log("getRegisters() returned %d\n", k);

        if (k==0)
          r=TRUE;
        else
          r=FALSE;


        if (!wasPaused)
        {
          //continue if sigstop
          PThreadData td=GetThreadData(p, tid);

          debug_log("The thread was not paused, so resuming it now\n");

          if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
          {
            debug_log("Not a SIGSTOP. Adding to queue and leave suspended\n");
            td->isPaused=1;
            AddDebugEventToQueue(p, &de);

          }
          else
          {
            r=(r && (safe_ptrace(PTRACE_CONT, de.threadid, 0,0)==0));


            td->isPaused=0;
            debug_log("r=%d\n", r);
          }
        }


      }
      else
        debug_log("Invalid tid\n");

    }
    else
    {
      debug_log("Not the debugger thread. Pass to serverthread");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } gtc;
#pragma pack()

      gtc.command=CMD_GETTHREADCONTEXT;
      gtc.hProcess=hProcess;
      gtc.tid=tid;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        debug_log("Sending message to the debuggerthread\n");

        sendall(p->debuggerClient, &gtc, sizeof(gtc), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &r, sizeof(r), MSG_WAITALL);

        debug_log("Returned from the debuggerthread.  result=%d\n",r);

        if (r)
        {
          //followed by the contextsize
          uint32_t structsize;
          recvall(p->debuggerClient, &structsize, sizeof(structsize), MSG_WAITALL);
          debug_log("structsize received from p->debuggerClient=%d\n", structsize);

          recvall(p->debuggerClient, Context, structsize, MSG_WAITALL); //and context

          debug_log("context->structsize received from p->debuggerClient=%d\n",Context->structsize);
        }


        pthread_mutex_unlock(&debugsocketmutex);
      }

    }
  }
  else
    debug_log("invalid handle\n");


  return r;
}

/*
 * Sets the context of the given thread
 * Fails if the thread is not suspended first
 */
BOOL SetThreadContext(HANDLE hProcess, int tid, PCONTEXT Context)
{
  BOOL r=FALSE;
  debug_log("SetThreadContext(%d)\n", tid);



  if (tid<=0)
  {
    debug_log("Invalid tid\n");
    return FALSE;
  }

  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);



    if (p->debuggerThreadID==pthread_self())
    {
        PThreadData td=GetThreadData(p, tid);

        debug_log("Inside debuggerthread\n");

        if (td)
        {
          DebugEvent de;
          int wasPaused=td->isPaused;
          int k=0;


          while ((td->isPaused==0) && (k<10))
          {
            debug_log("This thread was not paused. Pausing it\n");
            syscall(__NR_tkill, tid, SIGSTOP);
            if (WaitForDebugEventNative(p, &de, tid, 100))
              break;

            k++;
          }

          //the thread is paused, so fetch the data

          k=setContext(tid, Context);


          //k=safe_ptrace(PTRACE_SETREGS, tid, 0, &Context->regs);
          debug_log("setRegisters() returned %d\n", k);

          if (k==0)
            r=TRUE;
          else
            r=FALSE;


          if (!wasPaused)
          {
            //continue if sigstop
            PThreadData td=GetThreadData(p, tid);

            debug_log("The thread was not paused, so resuming it now\n");

            if (de.debugevent!=SIGSTOP) //in case a breakpoint or something else happened before sigstop happened
            {
              debug_log("Not a SIGSTOP. Adding to queue and leave suspended\n");
              td->isPaused=1;
              AddDebugEventToQueue(p, &de);
            }
            else
            {
              r=(r && (safe_ptrace(PTRACE_CONT, de.threadid, 0,0)==0));


              td->isPaused=0;
              debug_log("r=%d\n", r);
            }
          }


        }
        else
          debug_log("Invalid tid\n");

      }
    } 
    else
      debug_log("invalid handle\n");

    return r;
}

int SuspendThread(HANDLE hProcess, int tid)
/*
 * Increase the suspendcount. If it was 0, stop the given thread if it isn't stopped yet
 * If the thread was already stopped, look up this event in the queue and remove it
 * if it's the currently debugged thread it will not be in the queue, but continueFromDebugEvent
 * will see it is suspended, and as a result, NOT continue it, but store the debug event into the thread
 * If it wasn't stopped, stop it, and fetch the debug event
 */
{
  int result=-1;

  debug_log("SuspendThread(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    PThreadData t=GetThreadData(p, tid);

    if (t==NULL)
    {
      debug_log("Invalid thread\n");
      return -1;
    }

    if (p->debuggerThreadID==pthread_self())
    {
      //inside the debuggerthrad
      debug_log("Inside the debugger thread.\n");

      if (t->isPaused)
      {
        debug_log("Already paused\n");

        if (t->suspendCount==0)
        {
          //first time suspend. Gather the debug event and remove it from the queue (hardly ever, most of the time it's just the currently broken thread)

          if (p->debuggedThreadEvent.threadid==tid) //it's the currently suspended thread
            t->suspendedDevent=p->debuggedThreadEvent;
          else
          {
            //go through the queuelist to find the debug event and remove it
            t->suspendedDevent=*FindThreadDebugEventInQueue(p,tid);
            RemoveThreadDebugEventFromQueue(p, tid);
          }
        }

        t->suspendCount++;


      }
      else
      {
        debug_log("Not yet paused\n");

        while (t->isPaused==0)
        {
          syscall(__NR_tkill, tid, SIGSTOP);
          if (WaitForDebugEventNative(p, &t->suspendedDevent, tid, 100))
            break;

        }

        t->suspendCount++;

      }

      return t->suspendCount;


      //PThreadData t=GetThreadData(tid);
    }
    else
    {
      debug_log("Not from the debugger thread. Switching...\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } st;
#pragma pack()

      st.command=CMD_SUSPENDTHREAD;
      st.hProcess=hProcess;
      st.tid=tid;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        sendall(p->debuggerClient, &st, sizeof(st), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        pthread_mutex_unlock(&debugsocketmutex);


      }
    }
  }
  else
  {
    debug_log("invalid handle\n");
    result=-1;
  }

  return result;

}

int ResumeThread(HANDLE hProcess, int tid)
/*
 * Decrease suspendcount. If 0, resume the thread by adding the stored debug event back to the queue
 */
{
  int result=-1;

  debug_log("ResumeThread(%d)\n", tid);
  if (GetHandleType(hProcess) == htProcesHandle )
  {
    PProcessData p=(PProcessData)GetPointerFromHandle(hProcess);
    PThreadData t=GetThreadData(p, tid);

    if (t==NULL)
    {
      debug_log("Invalid thread\n");
      return -1;
    }

    if (p->debuggerThreadID==pthread_self())
    {
      //inside the debuggerthread
      debug_log("Inside the debugger thread.\n");

      if ((t->isPaused) && (t->suspendCount>0))
      {

        t->suspendCount--;

        result=t->suspendCount;


        if (t->suspendCount==0)
        {
          //reached 0, continue process if sigstop, else add to queue
          PThreadData td=GetThreadData(p, tid);
          debug_log("suspeneCount==0\n");

          if (t->suspendedDevent.debugevent==SIGSTOP)
          {
            debug_log("SIGSTOP: Continue thread without queing\n");
            safe_ptrace(PTRACE_CONT, t->suspendedDevent.threadid, 0,0);
            td->isPaused=0;
          }
          else
          {
            debug_log("Not a SIGSTOP Add to the event queue\n");
            td->isPaused=1;
            AddDebugEventToQueue(p, &t->suspendedDevent);
            WakeDebuggerThread();
          }
        }
      }
      else
      {
        debug_log("Failure resuming this thread\n");

      }


      //PThreadData t=GetThreadData(tid);
    }
    else
    {
      debug_log("Not from the debugger thread. Switching...\n");
#pragma pack(1)
      struct
      {
        char command;
        HANDLE hProcess;
        int tid;
      } rt;
#pragma pack()

      rt.command=CMD_RESUMETHREAD;
      rt.hProcess=hProcess;
      rt.tid=tid;

      if (pthread_mutex_lock(&debugsocketmutex) == 0)
      {
        sendall(p->debuggerClient, &rt, sizeof(rt), 0);
        WakeDebuggerThread();
        recvall(p->debuggerClient, &result, sizeof(result), MSG_WAITALL);

        pthread_mutex_unlock(&debugsocketmutex);
      }
    }
  }
  else
    debug_log("invalid handle\n");

  return result;
}

int RemoveThreadDebugEventFromQueue(PProcessData p, int tid)
/*
 * removes the debug event from the queue
 */
{
  int result=FALSE;
  struct DebugEventQueueElement *deqe;

  pthread_mutex_lock(&p->debugEventQueueMutex);

 // debug_log("RemoveThreadDebugEventFromQueue(%d)\n", tid);

  deqe=p->debugEventQueue.tqh_first;
  while (deqe)
  {
   // debug_log("deqe->de.threadid=%d  (looking for %d)\n", deqe->de.threadid, tid);
    if (deqe->de.threadid==tid)
    {
      //printf("Found. Removing it\n");
      TAILQ_REMOVE(&p->debugEventQueue, deqe, entries);

      free(deqe);
      result=TRUE;
      break;
    }

   // debug_log("Not what I wanted. Check next\n");
    deqe=deqe->entries.tqe_next;
  }


  pthread_mutex_unlock(&p->debugEventQueueMutex);
  return result;
}

PDebugEvent FindThreadDebugEventInQueue(PProcessData p, int tid)
/*
 * Finds the DebugEvent for this specific thread if there is one
 * Note that it returns a pointer to it. If you plan on removing it from the queue, copy the results manually
 */
{
  PDebugEvent result=NULL;
  struct DebugEventQueueElement *deqe;
  pthread_mutex_lock(&p->debugEventQueueMutex);

  deqe=p->debugEventQueue.tqh_first;
  while (deqe)
  {
    if ((tid==-1) || (deqe->de.threadid==tid))
    {
      result=&deqe->de;
      break;
    }

    deqe=deqe->entries.tqe_next;
  }


  pthread_mutex_unlock(&p->debugEventQueueMutex);
  return result;
}

void AddDebugEventToQueue(PProcessData p, PDebugEvent devent)
{
  struct DebugEventQueueElement *deqe;

  if (devent->debugevent==SIGSTOP)
  {
    debug_log("<<<<<--------------------SIGSTOP ADDED TO THE QUEUE!\n");
  }

  pthread_mutex_lock(&p->debugEventQueueMutex);

  deqe=malloc(sizeof(struct DebugEventQueueElement));
  deqe->de=*devent;

  TAILQ_INSERT_TAIL(&p->debugEventQueue, deqe, entries);

  pthread_mutex_unlock(&p->debugEventQueueMutex);
}

int GetStopSignalFromThread(int tid)
{
  siginfo_t si;

  if (safe_ptrace(PTRACE_GETSIGINFO, tid, NULL, &si)==0)
    return si.si_signo;
  else
    return -1;
}

int WaitForDebugEventNative(PProcessData p, PDebugEvent devent, int tid, int timeout)
/*
 * Waits for a debug event for a specific thread, queues the devent if not the expected tid
 * Only call this from a(the) debugger thread (NO OTHER THREAD MAY CALL THIS)
 */
{
  int currentTID;
  int status;
  int r;





  //printf("WaitForDebugEventNative (p=%p, devent=%p, tid=%d timeout=%d)\n", p, devent, tid, timeout);


  //first check if there is already a thread waiting
  currentTID=1;
  while (currentTID>0)
  {
    currentTID=waitpid(tid, &status, __WALL | WNOHANG);

    //printf("First waitid=%d\n", currentTID);

    if (currentTID>0)
    {
      devent->threadid=currentTID;
      devent->debugevent=GetStopSignalFromThread(devent->threadid);

      PThreadData td=GetThreadData(p, currentTID);
      if (td)
        td->isPaused=1;


      if ((tid==-1) || (currentTID==tid))
        return TRUE;

      //still here, this wasn't what I was looking for...
      //add it to the queue

      //printf("Unexpected event from thread %d while waiting for %d\n", currentTID, tid);



      AddDebugEventToQueue(p, devent);
    }
    //try again, perhaps there is another one available right now
  }

 // debug_log("Checking for debug server command\n");

//  fflush(stdout);

  //still here
  //CheckForAndDispatchCommand(p->debuggerServer);


 // debug_log("After check and dispatch\n");
 // fflush(stdout);


  if (timeout>=0)
  {
    //wait till there is an event

    if (timeout>0)
    {
      struct timespec abstime;