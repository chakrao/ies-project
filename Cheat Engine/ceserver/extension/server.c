/*
 * server.c
 *
 *  Created on: Aug 25, 2013
 *      Author: eric
 *
 * The communication layer between the process and ceserver
 * This module will be loaded into the game by either a PRELOAD like LD_PRELOAD or a config, or injected using ceserver's module injector
 *
 * When launched the constructor will spawn a thread and open a named shared object/pipe (does linux support that) for communication
 */

#include <errno.h>
#include <stdio.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>
#include <stdarg.h>
#ifdef __ANDROID__
#include <android/log.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/mman.h>



#include "server.h"
#include "speedhack.h"

int done=0;

#ifndef SUN_LEN //missing in android (copy from linux sys/un.h)
# include <string.h>    /* For prototype of `strlen'.  */

/* Evaluate to actual length of the `sockaddr_un' structure.  */
# define SUN_LEN(ptr) ((size_t) (((struct sockaddr_un *) 0)->sun_path)        \
          + strlen ((ptr)->sun_path))
#endif

void dvmJitStats();


typedef struct
{
  uint64_t address;
  uint32_t size;
} AllocEntry, *PAllocEntry;

PAllocEntry allocList;
int allocList_Max;
int allocList_Pos;



#ifdef __ANDROID__
  #define LOG_TAG "CESERVER_EXTENSION"
  #define LOGD(fmt, args...) __android_log_vprint(ANDROID_LOG_DEBUG, LOG_TAG, fmt, ##args)
#endif

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

void allocListAdd(uint64_t address, uint32_t size)
{
  if (allocList==NULL)
  {
    allocList=calloc(16, sizeof(AllocEntry));
    allocList_Max=16;
  }

  allocList[allocList_Pos].address=address;
  allocList[allocList_Pos].size=size;

  allocList_Pos++;
  if (allocList_Pos==allocList_Max)
  {
    allocList_Max*=2;
    allocList=realloc(allocList, allocList_Max*sizeof(AllocEntry));
  }
}

void allocListRemove(uint64_t address)
{
  int i;
  for (i=0; i<allocList_Pos; i++)
  {
    if (allocList[i].address==address)
    {
      int j;
      for (j=i; j<allocList_Pos-1; j++)
        allocList[j]=allocList[j+1];

      allocList_Pos--;
      break;
    }
  }

}

int allocListFind(uint64_t address)
/*
 * Return the size. (0 if not found)
 */
{
  int i;
  for (i=0; i<allocList_Pos; i++)
  {
    if (allocList[i].address==address)
    {
      return allocList[i].size;
    }
  }

  return 0;

}



ssize_t recvall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  //printf("enter recvall\n");

  flags=flags | MSG_WAITALL;

  while (sizeleft>0)
  {
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      debug_log("recv returned 0\n");
      return i;
    }

    if (i==-1)
    {
      debug_log("recv returned -1\n");
      if (errno==EINTR)
      {
        debug_log("errno = EINTR\n");
        i=0;
      }
      else
      {
        debug_log("Error during recvall: %d. errno=%d\n",(int)i, errno);
        return i; //read error, or disconnected
      }

    }

    totalreceived+=i;
    sizeleft-=i;
  }

  //printf("leave recvall\n");
  return totalreceived;
}

ssize_t sendall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalsent=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  while (sizeleft>0)
  {
    ssize_t i=send(s, &buffer[totalsent], sizeleft, flags);

    if (i==0)
    {
      return i;
    }

    if (i==-1)
    {
      if (errno==EINTR)
        i=0;
      else
      {
        debug_log("Error during sendall: %d. errno=%d\n",(int)i, errno);
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i