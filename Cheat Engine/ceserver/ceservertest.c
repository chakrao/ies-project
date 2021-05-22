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
  