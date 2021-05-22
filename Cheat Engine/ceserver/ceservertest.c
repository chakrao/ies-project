/*
 * ceservertest.c
 *
 *  Created on: Jul 1, 2013
 *      Author: eric
 *
 *  this is basically a standalone client process, but because i'm lazy it runs in the same context as the server
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef __ANDROID__
#include <sys/ptrace.h>
#endif
#include <asm/ptrace.h>
#include <linux/types.h>



#include "ceserver.h"
#include "api.h" //for debugevent

int pHandle;

int cenet_connect(void)
{
  int fd;
  int i;


  struct sockaddr_in addr;

  fd=socket(AF_INET, SOCK_STREAM, 0);


  memset(&addr, 0, sizeof(addr));
  addr.sin_family=AF_INET;
  addr.sin_port=htons(PORT);
  addr.sin_addr.s_addr=htonl(INADDR_LOOPBACK); //0x1600a8c0; //INADDR_LOOPBACK;

  debug_log("calling connect... (port=%d)\n", PORT);
  i=connect(fd, (struct sockaddr *)&addr, sizeof(addr));

  debug_log("after connect. %d\n", i);


  return fd;
}

int cenet_OpenProcess(int fd, int pid)
{
#pragma pack(1)
  struct
  {
    char command;
    int pid;
  } op;
#pragma pack()

  int pHandle;

  debug_log("cenet_OpenProcess(%d,%d)\n", fd, pid);



  op.command=CMD_OPENPROCESS;
  op.pid=pid;

  pHandle=0;

  sendall(fd, &op, sizeof(op), 0);
  recv(fd, &pHandle, sizeof(pHandle),MSG_WAITALL);

  return pHandle;
}

unsigned char cenet_getArchitecture(int fd, int pHandle)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
  } sd;
#pragma pack()
  unsigned char result;

  sd.command=CMD_GETARCHITECTURE;
  sd.pHandle=pHandle;

  sendall(fd, &sd, sizeof(sd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_startDebugger(int fd, int pHandle)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
  } sd;
#pragma pack()

  int result;


  sd.command=CMD_STARTDEBUG;
  sd.pHandle=pHandle;

  sendall(fd, &sd, sizeof(sd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;

}

int cenet_waitForDebugEvent(int fd, int pHandle, DebugEvent* devent, int timeout)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
    int timeout;
  } wfd;
#pragma pack()


  int result;




  wfd.command=CMD_WAITFORDEBUGEVENT;
  wfd.pHandle=pHandle;
  wfd.timeout=timeout;

  sendall(fd, &wfd, sizeof(wfd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);
  if (result)
    recv(fd, devent, sizeof(DebugEvent), MSG_WAITALL);

  debug_log(">>>>>>>>>>>>>>>>>>cenet_waitForDebugEvent returned<<<<<<<<<<<<<<<<\n");

  return result;

}

BOOL cenet_getThreadContext(int fd, int pHandle, int tid, void* context)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE hProcess;
    int tid;
    int type;
  } gtc;
#pragma pack()

  uint32_t result;


  debug_log("ceservertest: cenet_getThreadContext(%d, %d, %d, %p)\n", fd, pHandle, tid, context);

  gtc.command=CMD_GETTHREADCONTEXT;
  gtc.hProcess=pHandle;
  gtc.tid=tid;
  gtc.type=0;

  sendall(fd, &gtc, sizeof(gtc), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  debug_log("ceservertest cenet_getThreadContext returned %d\n", result);

  if (result)
  {
    CONTEXT c;
    uint32_t structsize;
    recv(fd, &structsize, sizeof(uint32_t), MSG_WAITALL);

    debug_log("structsize=%d\n", structsize);


    if (structsize<=sizeof(c))
    {
      recv(fd, &c, structsize, MSG_WAITALL);

      if (c.type==2)
        debug_log("ARM32 context type\n");

      if (c.type==3)
        debug_log("ARM64 context type\n");

      memcpy(context, &c, sizeof(c));
    }
    else
      debug_log("Received context is too big\n");
  }

  return result;

}

int cenet_continueFromDebugEvent(int fd, int pHandle, int tid, int ignore)
{
#pragma pack(1)
  struct
  {
    char command;
    HANDLE pHandle;
    int tid;
    int ignore;
  } cfd;
#pragma pack()

  int result;


  cfd.command=CMD_CONTINUEFROMDEBUGEVENT;
  cfd.pHandle=pHandle;
  cfd.tid=tid;
  cfd.ignore=ignore;

  sendall(fd, &cfd, sizeof(cfd), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_readProcessMemory(int fd, int pHandle, unsigned long long address, void *dest, int size)
{
#pragma pack(1)
  struct
  {
    char command;
    uint32_t pHandle;
    uint64_t address;
    uint32_t size;
    uint8_t compress;

  } rpm;
#pragma pack()

  int result;

 // printf("cenet_readProcessMemory\n");


 // debug_log("cenet_readProcessMemory(%d, %d, %llx, %p, %d)", fd, pHandle, address, dest, size);
 // fflush(stdout);


  rpm.command=CMD_READPROCESSMEMORY;
  rpm.pHandle=pHandle;
  rpm.address=address;
  rpm.size=size;
  rpm.compress=0;

  sendall(fd, &rpm, sizeof(rpm), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

 // debug_log("result=%d\n", result);
  recv(fd, dest, result, MSG_WAITALL);


  return result;

}

int cenet_setBreakpoint(int fd, int pHandle, int tid, void *Address, int bptype, int bpsize, int debugreg)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      int tid;
      int debugreg;
      uint64_t Address;
      int bptype;
      int bpsize;
    } sb;
#pragma pack()
    int result;

  debug_log("cenet_setBreakpoint sizeof(sb)=%d\n", sizeof(sb));
  sb.command=CMD_SETBREAKPOINT;
  sb.hProcess=pHandle;
  sb.tid=tid;
  sb.debugreg=debugreg;
  sb.Address=(uintptr_t)Address;
  sb.bptype=bptype;
  sb.bpsize=bpsize;

  sendall(fd, &sb, sizeof(sb), 0);


  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_removeBreakpoint(int fd, int pHandle, int tid, int debugreg, int waswatchpoint)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint32_t tid;
      uint32_t debugreg;
      uint32_t waswatchpoint;
    } rb;
#pragma pack()
    int result;

  debug_log("cenet_removeBreakpoint\n");
  rb.command=CMD_REMOVEBREAKPOINT;
  rb.hProcess=pHandle;
  rb.tid=tid;
  rb.debugreg=debugreg;
  rb.waswatchpoint=waswatchpoint;

  sendall(fd, &rb, sizeof(rb), 0);
  recv(fd, &result, sizeof(result), MSG_WAITALL);

  return result;
}

int cenet_loadExtension(int fd, int pHandle)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
    } le;
#pragma pack()
  int result;

  le.command=CMD_LOADEXTENSION;
  le.hProcess=pHandle;
  sendall(fd, &le, sizeof(le), 0);

  recv(fd, &result, sizeof(result),MSG_WAITALL);
  return result;
}


#define ARM_DBG_READ(N, M, OP2, VAL) do {\
         asm volatile("mrc p14, 0, %0, " #N "," #M ", " #OP2 : "=r" (VAL));\
} while (0)


uint64_t cenet_VirtualAllocEx(int fd, int pHandle, void *addr, size_t length, int windowsprotection)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint64_t preferedBase;
      uint32_t size;
      uint32_t windowsprotection;
    } vae;
#pragma pack()
    uint64_t result;
    vae.command=CMD_ALLOC;
    vae.hProcess=pHandle;
    vae.preferedBase=(uint64_t)addr;
    vae.size=length;
    vae.windowsprotection=windowsprotection;

    sendall(fd, &vae, sizeof(vae),0);

    recvall(fd, &result, sizeof(result),0);

    return result;
}

int cenet_VirtualQueryExFull(int fd, int pHandle, DWORD flags)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint8_t flags;
    } vqef;
#pragma pack()

    vqef.command=CMD_VIRTUALQUERYEXFULL;
    vqef.hProcess=pHandle;
    vqef.flags=flags;

    sendall(fd, &vqef, sizeof(vqef),0);
}

uint64_t cenet_loadModule(int fd, int pHandle, char *path)
{
#pragma pack(1)
    struct
    {
      char command;
      HANDLE hProcess;
      uint32_t modulepathlength;
    } lm;
#pragma pack()
    uint64_t result;


    lm.command=CMD_LOADMODULE;
    lm.hProcess=pHandle;
    lm.modulepathlength=strlen(path);
    sendall(fd, &lm, sizeof(lm), MSG_MORE);
    sendall(fd,path,strlen(path),0);

    recvall(fd,&result,sizeof(result),0);

    return result;
}

uint64_t cenet_openNamedPipe(int fd, char *pipename, int timeout)
{
  HANDLE h;
  debug_log("cenet_openNamedPipe(%d, \"%s\", %d)\n",fd,pipename, timeout);
  char command=CMD_OPENNAMEDPIPE;
  sendall(fd, &command, 1,MSG_MORE);
  sendstring16(fd,pipename,MSG_MORE);
  sendall(fd, &timeout, sizeof(timeout),0);

  recvall(fd, &h, sizeof(h),0);

  return h;
}

void *CESERVERTEST_DEBUGGERTHREAD(void *arg)
{
  int count=0;
  int fd=cenet_connect();

  int dscr=0;
#ifdef __arm__
  ARM_DBG_READ(c0, c1, 0, dscr);
  debug_log("after: %x\n", dscr);
#endif




  debug_log("calling cenet_startDebugger\n");

  if (cenet_startDebugger(fd, pHandle))
  {
    int i;
    DebugEvent devent;

    debug_log("cenet_startDebugger=true\n");

    while (1)
    {
      count++;
      debug_log("count