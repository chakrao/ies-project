/*
 * extentionloader.c  (todo: rename to ptracecodeexecutor.c)
 *
 *  Created on: Aug 19, 2013
 *      Author: eric
 *
 *  Used for loading an module that will extend the ceserver
 *  client<-->ceserver<-->extention
 *
 *  It doesn't HAVE to be used if the forced module load method works (Do not assume so)
 *
 *  How it works:
 *  Ptrace the target  (this means it must be loaded BEFORE the debugger is attached)
 *  Cause a stop and make sure it's runnable (Not sure if it executes if it's suspended for some reason. e.g: wait for event/critical section that may never happen)
 *  Change the current instruction pointer to the beginning of dlopen and the register/stack state setup to execute
 *  Set the return addres to an invalid return address (e.g 0x0ce0)
 *  Execute it and wait till a sigtrap/sigseg happens on that specific invalid address
 *  Then restore the state back
 *
 *  On arm32: Bit J and T in CPSR define the current execution state
 *  J T
 *  0 0 = ARM
 *  0 1 = Thumb
 *  1 0 = Jazelle (java...)
 *  1 1 = ThumbEE*
 *
 *  If ARM so set to 0 0 and restore that as well
 *  Note that The least significant bit in an address specifier also determines if it's THUMB or ARM (edit: nope.  But lets go with this in CE)
 *  It doesn't seem to matter if you set the least significant bit in the PC register. It will ignore that bit on execute. (probably a good idea to clear that bit anyhow)
 *
 *
 *  Problem: It doesn't return properly when the registers are changed when it's waiting in a syscall, so only change it when outside of a syscall
 *  Better solution: It seems it failed because the stop was at a syscall, so the program counter was decremented tithe the size of the syscall
 *  To prevent this RESTART change EAX to 0 so it won't do the restart.  Also works on ARM
 *
 *  Problem2: In android dlopen is in /system/bin/linker but not using a symbol (so ce's symbollist can't be used to find the address)
 *
 *  dlopen("libdl.so", RTLD_NOW) actually works in android and dlsym as well. (point to the linker version)
 *  This is useful since this makes it cross compatible with normal linux.
 *  for some reason getting the address of dlopen in x86 returns a local stub and I don't know yet how to prevent those stubs
 *
 *  so, to find dlopen find address range dlopen is in in this process (/proc/selfpid/maps), get the base address of that specific module
 *  and then add that offset to the same named module in the target process
 *
 */

#include <stdlib.h>

#include <stdio.h>
#include <sys/wait.h>

#include <sys/ptrace.h>
#include <sys/mman.h>

#include <errno.h>
#include <stdint.h>
#include <string.h>

#ifdef HAS_LINUX_USER_H
#include <linux/user.h>
#else
#include <sys/user.h>
#endif

#include <dlfcn.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <libgen.h>

#ifdef __aarch64__
#include <elf.h>
#ifdef __ANDROID__
#include <arm-linux-androideabi/asm/ptrace.h>
#endif
#endif

#include "symbols.h"
#include "porthelp.h"
#include "api.h"
#include "ceserver.h"

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif

#if defined(__i386__) || defined(__x86_64__)
typedef struct user_regs_struct process_state;
#endif

#ifdef __arm__
typedef struct pt_regs process_state;
#endif

#ifdef __aarch64__
typedef struct user_pt_regs process_state;
typedef struct {uint32_t uregs[18];} process_state32;
#endif

int setProcessState(int pid, process_state *state)
{
#ifdef __aarch64__
  struct iovec iov;
  iov.iov_base=state;
  iov.iov_len=sizeof(process_state);
  return ptrace(PTRACE_SETREGSET, pid, (void*)NT_PRSTATUS, &iov);
#else
  return ptrace(PTRACE_SETREGS, pid, 0, state);
#endif
}

#ifdef __aarch64__
int setProcessState32(int pid, process_state32 *state)
{
  struct iovec iov;
  iov.iov_base=state;
  iov.iov_len=sizeof(process_state);
  return ptrace(PTRACE_SETREGSET, pid, (void*)NT_PRSTATUS, &iov);
}
#endif

int getProcessState(int pid, process_state *state)
{
#ifdef __aarch64__
  struct iovec iov;
  iov.iov_base=state;
  iov.iov_len=sizeof(process_state);
  return ptrace(PTRACE_GETREGSET, pid, (void*)NT_PRSTATUS, &iov);
#else
  return ptrace(PTRACE_GETREGS, pid, 0, state);
#endif
}

#ifdef __aarch64__
int getProcessState32(int pid, process_state32 *state)
{
  struct iovec iov;
  iov.iov_base=state;
  iov.iov_len=sizeof(process_state);
  return ptrace(PTRACE_GETREGSET, pid, (void*)NT_PRSTATUS, &iov);
}
#endif

int WaitForPid(int *status)
{
  int pid=-1;
  while (pid==-1)
  {
    pid=waitpid(-1, status, __WALL);
    if ((pid==-1) && (errno!=EINTR))
    {
      debug_log("WaitForPid wait fail. :%d (%s)\n", errno, strerror(errno) );
      return -1; //something bad happened
    }
  }
  return pid;
}

int resumeProcess(PProcessData p, int pid)
{
  if (!p->isDebugged)
  {
    debug_log("Detaching\n");
    return ptrace(PTRACE_DETACH, pid,0,0);
  }
  else
  {
    debug_log("Resuming\n");
    return ptrace(PTRACE_CONT,pid,(void *)0,(void *)SIGCONT);
  }
}

int pauseProcess(PProcessData p)
{
  if (!p->isDebugged)
    return ptrace_attach_andwait(p->pid);
  else
  {
    int pid;
    int status;
    if (p->debuggedThreadEvent.threadid)
    {
      debug_log("Debugging active on a broken thread. Can't pause the process for code execution at this time\n");
      return -1;
    }

    debug_log("Killing pid %d\n", p->pid);
    kill(p->pid, SIGSTOP);
    pid=WaitForPid(&status);

    if (WIFSTOPPED(status))
      debug_log("Stopped with signal %d\n", WSTOPSIG(status));
    else
      debug_log("Unexpected status: %x\n", status);

    return pid;
  }

}

int showRegisters(int pid)
{

    return 0;
}

uintptr_t finddlopen(int pid, uintptr_t *_dlerror)  //todo: use the elf parsing routines (they work better now)
{
    void *libdl;
    void *realdlopen;
    void *realdlerror;
    libdl=dlopen("libdl.so", RTLD_NOW);

    debug_log("libdl=%p\n", libdl);

    realdlopen=dlsym(libdl,"dlopen");
    realdlerror=dlsym(libdl,"dlerror");
    debug_log("dlopen=%p\n", dlopen);
    debug_log("realdlopen=%p\n", realdlopen);

    debug_log("dlerror=%p\n", dlerror);
    debug_log("realdlerror=%p\n", realdlerror);
#ifndef __arm__
    if (dlopen==realdlopen)
      debug_log("Please tell db what you did to get this to function (excluding manually editing this if statement)\n");
#endif


    //open /proc/self/maps and look up the region that holds realdlopen

    FILE *maps=fopen("/proc/self/maps", "r");

    char x[200];
    char currentmodule[256];
    char modulepath[256];

    unsigned long long currentmodulestart;

    currentmodule[0]=0;


    while (fgets(x, 200, maps))
    {
      unsigned long long start;
      unsigned long long stop;
      debug_log("%s", x);

      sscanf(x, "%llx-%llx %*s %*s %*s %*s %s\n", &start, &stop, modulepath);

      if (strcmp(modulepath, currentmodule)!=0)
      {
         strcpy(currentmodule, modulepath);
         currentmodulestart=start;
      }

      if (
           (((uintptr_t)realdlopen)>=start) &&
           (((uintptr_t)realdlopen)<stop)
         )
      {
        unsigned int offset=(uintptr_t)realdlopen-currentmodulestart;
        unsigned int offset2=(uintptr_t)realdlerror-currentmodulestart;
        char mapsfilename[255];
        debug_log("found it. Module: %s Offset=%x\n", currentmodule, offset);

        //find this module in the target process and apply this offset to get the address of dlopen
        sprintf(mapsfilename, "/proc/%d/maps", pid);
        FILE *maps2=fopen(mapsfilename, "r");
        if (maps2)
        {
          char y[200];
          while (fgets(y, 200, maps2))
          {
             if (y[strlen(y)-1]!='\n')
             {
               //need to go to the end of line first

               char discard[100];

               do
               {
                 discard[99]=0;
                 fgets(discard, 99, maps);
               } while (discard[99]!=0);
             }


             debug_log("%s", y);

             modulepath[0]='\0';
             sscanf(y, "%llx-%llx %*s %*s %*s %*s %s\n", &start, &stop, modulepath);

             debug_log("Check if '%s' == '%s'\n", modulepath, currentmodule);
             if (strcmp(modulepath, currentmodule)==0)
             {
                debug_log("found the module in the target process\n");
                fclose(maps);
                fclose(maps2);
                *_dlerror=start+offset2;
                return start+offset;
             }
          }
          fclose(maps2);

        }
        else
        {
           debug_log("Failure to open %s\n", mapsfilename);
        }


        fclose(maps);
        return 0;
      }
      else debug_log("Nope\n");

    }

    fclose(maps);

    return 1;
}

void writeString(int pid, uintptr_t address, char *string)
{
  int l=strlen(string)+1;
  long *p;
  long v;
  int i;
  int bs;
  i=0;

  debug_log("l=%d\n", l);


  while (i<l)
  {
    p=(long *)&string[i];
    if ((l-i)<sizeof(long))
    {
      bs=sizeof(long);
      v=*p;
    }
    else
    {
      v=string[i];
      bs=1;
    }

    safe_ptrace(PTRACE_POKEDATA, pid, (void*)(address+i), (void*)v);
    i+=bs;
  }
}

int openExtension(int pid, int *openedSocket)
{
  char name[256];
  sprintf(name, "ceserver_extension%d", pid);
  HANDLE h=OpenPipe(name,0);
  if (h)
  {
    PPipeData pd=GetPointerFromHandle(h);
    *openedSocket=pd->socket;
    if (pd->pipename)
      free(pd->pipename);

    free(pd);
    RemoveHandle(h); //not needed anymore, but do keep the socket open

    return 1;
  }
  else
    return 0;
}

int isExtensionLoaded(int pid)
{
  int s;
  int result=openExtension(pid, &s);

  if (result)
    close(s);

  return result;
}

int loadExtension(PProcessData p, char *path)
{

    int pid;
    uintptr_t dlerror;
    uintptr_t str;
    int status;
    int pathlen=strlen(path)+1; //0-terminater

    debug_log("loadExtension()\n");




    debug_log("Phase 0: Check if it's already open\n");
    if (isExtensionLoaded(p->pid))
    {
      debug_log("Already loaded\n");
      return TRUE;
    }
    else
      debug_log("Not yet loaded\n");

    if (access(path, X_OK))
    {
      debug_log("FAILURE: %s is not executable or does not even exist! (%s)\n",path, strerror(errno));
      return 0;
    }
    else
      debug_log("Execute check passed\n");





    if (p->dlopen==0) //fallback to the old method
    {
      debug_log("Phase 1: Find dlopen in target (old wonky method)\n");
      p->dlopen=finddlopen(p->pid, &dlerror);
    }

    if (p->dlopen==0)
    {
      debug_log("dlopen==NULL Abort!\n");
      return 0;
    }

    debug_log("dlopen=%p\n", (void *)p->dlopen);
    //debug_log("dlerror=%p\n", (void *)dlerror);


    pid=pauseProcess(p);
    if (pid==-1)
      return FALSE;

    debug_log("After pauseProcess: PID=%d\n", pid);

    //save the current state and set the state to what I need it to be
  process_state origregs;
  process_state newregs;

#ifdef __aarch64__
  process_state32 origregs32;
  process_state32 newregs32;
#endif



#ifdef __aarch64__
  struct iovec iov;
#endif

#ifdef __aarch64__

      if (p->is64bit)
        status=getProcessState(pid, &origregs);
      else
        status=getProcessState32(pid, &origregs32);

      if (status)
#else
      if (getProcessState(pid, &origregs))
#endif
      {
        debug_log("getProcessState failed\n");
        safe_ptrace(PTRACE_DETACH, pid,0,0);

        return FALSE;
      }

      newregs=origregs;
#ifdef __aarch64__
      newregs32=origregs32;
#endif

      uintptr_t returnaddress=0x0ce0;


#ifdef __arm__
      //allocate space in the stack

      newregs.ARM_sp-=8+4*((pathlen+3)/ 4);

      //not sur eif [sp] is written to with a push or if it's [sp-4] and then sp decreased, so start at sp+4 instead
      str=newregs.ARM_sp+4;
      writeString(pid, str, path);

      newregs.ARM_lr=returnaddress;
      newregs.ARM_pc=p->dlopen;
      newregs.ARM_r0=str;
      newregs.ARM_r1=RTLD_NOW;
      newregs.ARM_r2=p->dlopencaller; //needed by android: loader_dlopen


      if (newregs.ARM_pc & 1)
      {
         //THUMB Address link
         debug_log("THUMB destination\n");
         newregs.ARM_cpsr=newregs.ARM_cpsr | (1 << 5);

         //not sure how to set the J bit (thumbee uses it...)
         //for now disable it until a bug happens
         newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<25)); //unset J
      }
      else
      {
        debug_log("ARM destination\n");
        debug_log("newregs.ARM_cpsr was %x\n", newregs.ARM_cpsr);
        newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<5)); //unset T
        newregs.ARM_cpsr=newregs.ARM_cpsr & (~(1<<25)); //unset J
        debug_log("newregs.ARM_cpsr is %x\n", newregs.ARM_cpsr);
      }
#endif

#ifdef __aarch64__
      if (p->is64bit==0)
      {
        debug_log("orig pc=%lx\n", origregs32.ARM_pc);
        debug_log("orig sp=%lx\n", origregs32.ARM_sp);
        debug_log("orig cpsr=%lx\n", origregs32.ARM_cpsr);

        newregs32.ARM_sp-=8+4*((pathlen+3)/ 4);

        //not sure if [sp] is written to with a push or if it's [sp-4] and then sp decreased, so start at sp+4 instead
        str=newregs32.ARM_sp+4;
        writeString(pid, str, path);

        newregs32.ARM_lr=returnaddress;
        newregs32.ARM_pc=p->dlopen;
        newregs32.ARM_r0=str;
        newregs32.ARM_r1=RTLD_NOW;
        newregs32.ARM_r2=p->dlopencaller; //needed by android: loader_dlopen



        if (newregs32.ARM_pc & 1)
        {
           //THUMB Address link
           debug_log("THUMB destination\n");
           newregs32.ARM_cpsr=newregs32.ARM_cpsr | (1 << 5);

           //not sure how to set the J bit (thumbee uses it...)
           //for now disable it until a bug happens
           newregs32.ARM_cpsr=newregs32.ARM_cpsr & (~(1<<25)); //unset J


        }
        else
        {
          debug_log("ARM destination\n");
          debug_log("newregs32.ARM_cpsr was %x\n", newregs32.ARM_cpsr);
          newregs32.ARM_cpsr=newregs32.ARM_cpsr & (~(1<<5)); //unset T
          newregs32.ARM_cpsr=newregs32.ARM_cpsr & (~(1<<25)); //unset J
          debug_log("newregs32.ARM_cpsr is %x\n", newregs32.ARM_cpsr);
        }

        debug_log("new pc=%lx\n", newregs32.ARM_pc);
        debug_log("new sp=%lx\n", newregs32.ARM_sp);
        debug_log("new cpsr=%lx\n", newregs32.ARM_cpsr);


      }
      else
      {
        debug_log("64-bit target\n");

        debug_log("orig pc=%llx\n", origregs.pc);
   