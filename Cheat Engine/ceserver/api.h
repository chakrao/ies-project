/*
 * api.h
 *
 *  Created on: Jul 21, 2011
 *      Author: erich
 */

#ifndef API_H_
#define API_H_

#include <stdint.h>
#include <pthread.h>
#include <sys/queue.h>

#include "porthelp.h"
#include "context.h"

#include <sys/ptrace.h>

#ifdef __ANDROID__
  #include<android/log.h>
#endif
/*

#if defined(__arm__) || defined(__ANDROID__)
#include <linux/user.h>
#else
#include <sys/user.h>
#endif
*/

#ifdef HAS_LINUX_USER_H
  #include <linux/user.h>
#else
  #include <sys/user.h>
#endif

#include <sys/uio.h>



#define VQE_PAGEDONLY 1
#define VQE_DIRTYONLY 2
#define VQE_NOSHARED 4


typedef struct
{
  unsigned long long baseAddress;
  int part;
  int is64bit;
  int moduleSize;
  char *moduleName;
} ModuleListEntry, *PModuleListEntry;

typedef struct
{
  int PID;
  char *ProcessName;

} ProcessListEntry, *PProcessListEntry;

typedef struct
{
  int ReferenceCount;
  int processListIterator;
  int processCount;
  PProcessListEntry processList;
} ProcessList, *PProcessList;

typedef struct
{
  int ReferenceCount;
  int moduleListIterator;
  int moduleCount;
  PModuleListEntry moduleList;
} ModuleList, *PModuleList;

typedef struct
{
  int ReferenceCount;
  int threadListIterator;
  int threadCount;
  int *threadList;
} ThreadList, *PThreadList;

typedef struct
{
  int socket;
  char* pipename;
} PipeData, *PPipeData;

#pragma pack(1)

typedef struct
{
  uint8_t num_brps;   //number of instruction breakpoints
  uint8_t num_wrps;   //number of data breakpoints
  uint8_t wp_len;     //max length of a data breakpoint
  uint8_t debug_arch; //debug architecture

} HBP_RESOURCE_INFO, *PHBP_RESOURCE_INFO;

#ifdef __arm__
/*  struct user_pt_regs
  {
    long regs[18];
  };

  struct user_hwdebug_state {
   __u32 dbg_info;
   struct {
   __u32 addr;
   __u32 ctrl;
   } dbg_regs[16];
  };

#define NT_ARM_HW_WATCH 0x403
#define PTRACE_GETREGSET 0x4204
#define PTRACE_SETREGSET 0x4205
*/
#endif


typedef struct {
  int debugevent;
  int64_t threadid;
  union
  {
    uint64_t address; //TRAP: Address that caused trap
    struct {
      uint8_t maxBreakpointCount; //Number of execute breakpoints this system supports at most
      uint8_t maxWatchpointCount;
      uint8_t maxSharedBreakpoints; //If the system uses the same kind of breakpoints for exec