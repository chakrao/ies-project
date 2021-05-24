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
   