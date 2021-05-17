
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <sys/select.h>

#include <zlib.h>

#include <errno.h>
#include <elf.h>
#include <signal.h>
#include <sys/prctl.h>

#include <unistd.h>
#include <errno.h>
#include <dlfcn.h>

#include <sys/mman.h>
#include <libgen.h>
#include <dirent.h>
#include <fcntl.h>





#include "ceserver.h"
#include "porthelp.h"
#include "api.h"
#include "ceservertest.h"
#include "symbols.h"
#include "extensionfunctions.h"
#include "native-api.h"
#include "extensionloader.h"
#include "options.h"

pthread_t pth;
pthread_t identifierthread;
volatile int done;
int PORT;

int ALLOC_WITHOUT_EXTENSION=0; //in case extension loading fails

__thread int isDebuggerThread; //0 when not, else it contains the processhandle
__thread int debugfd;

__thread char* threadname;

#define CESERVERVERSION 5


char versionstring[]="CHEATENGINE Network 2.2";
char *CESERVERPATH;

volatile int connections=0;
pthread_mutex_t connectionsCS;


void initCESERVERPATH()
{
  int l;
  CESERVERPATH=malloc(512);
  CESERVERPATH[0]=0;

  l=readlink("/proc/self/exe", CESERVERPATH, 256);

  //basename and basedir bahave different in android, so just do this:
  while (l)
  {
    if (CESERVERPATH[l]=='/')
      return;
    else
    {
      CESERVERPATH[l]=0;
      l--;
    }


  }
  strcpy(CESERVERPATH,"./");
}

ssize_t recvall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  // enter recvall
  flags=flags | MSG_WAITALL;

  while (sizeleft>0)
  {
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      if (threadname)
        debug_log("%s: Error: recv returned 0\n", threadname);
      else
        debug_log("Error: recv returned 0\n");
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

  // leave recvall
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
        debug_log("Error during sendall: %d. error=%s\n",(int)i, strerror(errno));
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i;
  }

  return totalsent;
}

ssize_t sendstring16(int s, char *str, int flags)
{
  uint16_t l;
  if (str)
    l=strlen(str);
  else
    l=0;

  sendall(s, &l,sizeof(l),l?MSG_MORE:flags);
  if (l)
    sendall(s, str, l,flags);

  return l;
}

int sendinteger(int s, int val, int flags)
{
  return sendall(s, &val,sizeof(val),flags);
}


char* receivestring16(int s)
/* Receives a string that is preceded by a 16 bit length identifier (Allocates a string. Clean it up yourself)
 * returns NULL if the length is 0 bytes
 */

{
  char *str;
  uint16_t l;
  recvall(s, &l, sizeof(l),0);

  if (l)
  {
    str=malloc(l+1);
    recvall(s, str, l,0);
    str[l]=0;
    return str;
  }
  else
    return NULL;
}


int DispatchCommand(int currentsocket, unsigned char command)
{
  int r;

  switch (command)
  {
    case CMD_GETVERSION:
    {
      PCeVersion v;
      //debug_log("version request");
      fflush(stdout);
      int versionsize=strlen(versionstring);
#ifdef SHARED_LIBRARY
      versionsize+=3;
#endif
      v=(PCeVersion)malloc(sizeof(CeVersion)+versionsize);
      v->stringsize=versionsize;
      v->version=CESERVERVERSION;

#ifdef SHARED_LIBRARY
      memcpy((char *)v+sizeof(CeVersion),"lib",3);//tell ce it's the lib version
      memcpy((char *)v+sizeof(CeVersion)+3, versionstring, versionsize);

#else
      memcpy((char *)v+sizeof(CeVersion), versionstring, versionsize);
#endif

      //version request
      sendall(currentsocket, v, sizeof(CeVersion)+versionsize, 0);

      free(v);

      break;
    }

    case CMD_SET_CONNECTION_NAME:
    {
      debug_log("CMD_SET_CONNECTION_NAME\n");
      uint32_t namelength;


      if (recvall(currentsocket, &namelength, sizeof(namelength), MSG_WAITALL)>0)
      {
        char name[namelength+1];

        recvall(currentsocket, name, namelength, MSG_WAITALL);
        name[namelength]=0;

        if (threadname)
        {
          free(threadname);
          threadname=NULL;
        }
        threadname=strdup(name);

        debug_log("This thread is called %s\n", name);
      }

      fflush(stdout);

      break;
    }

    case CMD_GETABI:
    {
#ifdef WINDOWS
      unsigned char abi=0;
#else
      unsigned char abi=1;
#endif
      sendall(currentsocket, &abi, sizeof(abi), 0);
      break;
    }

    case CMD_GETARCHITECTURE:
    {
      unsigned char arch;
      HANDLE h;
      //ce 7.4.1+ : Added the processhandle

      debug_log("CMD_GETARCHITECTURE\n");

      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        //intel i386=0
        //intel x86_64=1
        //arm 32 = 2
        //arm 64 = 3
        debug_log("(%d)",h);
        arch=getArchitecture(h);
      }

      if(SPECIFIED_ARCH != 9)
      {
        arch = SPECIFIED_ARCH;
      }
      debug_log("=%d\n", arch);
      sendall(currentsocket, &arch, sizeof(arch), 0);
      break;
    }

    case CMD_CLOSECONNECTION:
    {
      debug_log("Connection %d closed properly\n", currentsocket);
      fflush(stdout);
      close(currentsocket);

      return 0;
    }

    case CMD_TERMINATESERVER:
    {
      debug_log("Command to terminate the server received\n");
      fflush(stdout);
      close(currentsocket);
      exit(0);
    }

    case CMD_STARTDEBUG:
    {
      HANDLE h;
      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        int r;
        debug_log("Calling StartDebug(%d)\n", h);
        r=StartDebug(h);
        sendall(currentsocket, &r, sizeof(r), 0);

        if (r)
        {
          isDebuggerThread=h;
          debugfd=GetDebugPort(h);
        }
      }
      break;
    }

    case CMD_WAITFORDEBUGEVENT:
    {
      struct
      {
        HANDLE pHandle;
        int timeout;
      } wfd;

      if (recvall(currentsocket, &wfd, sizeof(wfd), MSG_WAITALL)>0)
      {
        int r;
        DebugEvent event;
        memset(&event, 0, sizeof(event));

        r=WaitForDebugEvent(wfd.pHandle, &event, wfd.timeout);
        sendall(currentsocket, &r, sizeof(r), r?MSG_MORE:0);

        if (r)
        {
          if (event.debugevent==SIGTRAP)
          {
            debug_log("!!!SIGTRAP!!!\n");
            debug_log("event.address=%llx\n", event.address);
          }

          sendall(currentsocket, &event, sizeof(event),0);
        }
      }
      break;
    }

    case CMD_CONTINUEFROMDEBUGEVENT:
    {
      struct
      {
        HANDLE pHandle;
        int tid;
        int ignore;
      } cfd;

      if (recvall(currentsocket, &cfd, sizeof(cfd), MSG_WAITALL)>0)
      {
        int r;
        // Calling ContinueFromDebugEvent
        r=ContinueFromDebugEvent(cfd.pHandle, cfd.tid, cfd.ignore);
        // Returned from ContinueFromDebugEvent
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_SETBREAKPOINT:
    {
      CeSetBreapointInput sb;

      debug_log("CMD_SETBREAKPOINT. sizeof(sb)=%d\n", sizeof(sb));

      if (recvall(currentsocket, &sb, sizeof(sb), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling SetBreakpoint\n");
        r=SetBreakpoint(sb.hProcess, sb.tid, sb.debugreg, (void *)sb.Address, sb.bptype, sb.bpsize);
        debug_log("SetBreakpoint returned %d\n",r);
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_REMOVEBREAKPOINT:
    {
      CeRemoveBreapointInput rb;

      if (recvall(currentsocket, &rb, sizeof(rb), MSG_WAITALL)>0)
      {
        int r;



        debug_log("%s: Calling RemoveBreakpoint\n", threadname);
        r=RemoveBreakpoint(rb.hProcess, rb.tid, rb.debugreg, rb.wasWatchpoint);
        debug_log("RemoveBreakpoint returned: %d\n", r);
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_GETTHREADCONTEXT:
    {
#pragma pack(1)
      struct
      {
        HANDLE hProcess;
        uint32_t tid;
      } gtc;
#pragma pack()

      CONTEXT Context;
      uint32_t result;

      debug_log("CMD_GETTHREADCONTEXT:\n");

      recvall(currentsocket, &gtc, sizeof(gtc), MSG_WAITALL);

      debug_log("Going to call GetThreadContext(%d, %d, %p)\n", gtc.hProcess, gtc.tid, &Context);
      memset(&Context, 0, sizeof(Context));

      result=GetThreadContext(gtc.hProcess, gtc.tid, &Context);

      debug_log("result=%d\n", result);

      if (result)
      {
        debug_log("Context.structsize=%d\n", Context.structsize);
        uint32_t structsize=Context.structsize;
        sendall(currentsocket, &result, sizeof(result), MSG_MORE);
        sendall(currentsocket, &structsize, sizeof(structsize), MSG_MORE);
        sendall(currentsocket, &Context, structsize, 0); //and context
      }
      else
        sendall(currentsocket, &result, sizeof(result), 0);

      break;

    }

case CMD_SETTHREADCONTEXT:
    {
#pragma pack(1)
      struct
      {
        HANDLE hProcess;
        uint32_t tid;
        uint32_t structsize;
      } stc;
#pragma pack()

      uint32_t result;

      PCONTEXT c;

      debug_log("CMD_SETTHREADCONTEXT:\n");

      recvall(currentsocket, &stc, sizeof(stc), MSG_WAITALL);
      debug_log("hProcess=%d tid=%d structsize=%d\n", stc.hProcess, stc.tid, stc.structsize);

      c=(PCONTEXT)malloc(stc.structsize);
      recvall(currentsocket, c, stc.structsize, MSG_WAITALL);

      debug_log("received a context with data: structsize=%d type=%d\n", c->structsize, c->type);

      debug_log("Going to call SetThreadContext(%d, %d, %p)\n", stc.hProcess, stc.tid, c);

      result=SetThreadContext(stc.hProcess, stc.tid, c);
      free(c);

      debug_log("result=%d\n", result);

      sendall(currentsocket, &result, sizeof(result), 0);


      break;

    }

    case CMD_SUSPENDTHREAD:
    {
      CeSuspendThreadInput st;

      if (recvall(currentsocket, &st, sizeof(st), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling SuspendThread\n");
        r=SuspendThread(st.hProcess, st.tid);
        debug_log("SuspendThread returned\n");
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_RESUMETHREAD:
    {
      CeResumeThreadInput rt;

      if (recvall(currentsocket, &rt, sizeof(rt), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling ResumeThread\n");
        r=ResumeThread(rt.hProcess, rt.tid);
        debug_log("ResumeThread returned\n");
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_CLOSEHANDLE:
    {
      HANDLE h;

      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        CloseHandle(h);
        int r=1;
        sendall(currentsocket, &r, sizeof(r), 0); //stupid naggle

      }
      else
      {
        debug_log("Error during read for CMD_CLOSEHANDLE\n");
        close(currentsocket);
        fflush(stdout);
        return 0;
      }
      break;
    }


    case CMD_CREATETOOLHELP32SNAPSHOTEX:
    {
      CeCreateToolhelp32Snapshot params;
      //debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX\n");

      if (recvall(currentsocket, &params, sizeof(CeCreateToolhelp32Snapshot), MSG_WAITALL) > 0)
      {
        HANDLE r=CreateToolhelp32Snapshot(params.dwFlags, params.th32ProcessID);

        if ((params.dwFlags & TH32CS_SNAPTHREAD)==TH32CS_SNAPTHREAD)
        {
          //send the list of threadid's

          if (r)
          {
            PThreadList tl=(PThreadList)GetPointerFromHandle(r);
            sendall(currentsocket, &tl->threadCount, sizeof(int), MSG_MORE);
            sendall(currentsocket, &tl->threadList[0], tl->threadCount*sizeof(int),0);

            CloseHandle(r);
          }
          else
          {
            int n=0;
            sendall(currentsocket, &n, sizeof(int), 0);
          }
        }
        else
        if ((params.dwFlags & TH32CS_SNAPMODULE)==TH32CS_SNAPMODULE)
        {
          ModuleListEntry me;

          char *outputstream;
          int pos=0;

         // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX with TH32CS_SNAPMODULE\n");

          outputstream=malloc(65536);
          memset(outputstream,0,65536);

          if (r && (Module32First(r, &me))) do
          {
            int namelen=strlen(me.moduleName);
            PCeModuleEntry m;


            if ((pos+sizeof(CeModuleEntry)+namelen) > 65536)
            {
              //flush the stream
             // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList flush in loop\n");
              sendall(currentsocket, outputstream, pos, 0);
              pos=0;
            }

            m=(PCeModuleEntry)&outputstream[pos];
            m->modulebase=me.baseAddress;
            m->modulesize=me.moduleSize;
            m->modulenamesize=namelen;
            m->modulepart=me.part;
            m->result=1;

            // Sending %s size %x\n, me.moduleName, r->modulesize
            memcpy((char *)m+sizeof(CeModuleEntry), me.moduleName, namelen);

            pos+=sizeof(CeModuleEntry)+namelen;


          } while (Module32Next(r, &me));

          if (pos) //flush the stream
          {
           // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList flush after loop\n");
            sendall(currentsocket, outputstream, pos, 0);
          }

          //send the end of list module
         // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList end of list\n");

          CeModuleEntry eol;
          eol.result=0;
          eol.modulenamesize=0;
          sendall(currentsocket, &eol, sizeof(eol), 0);

          free(outputstream);

          if (r)
            CloseHandle(r);

        }
        else
        {
          sendall(currentsocket, &r, sizeof(HANDLE), 0); //the others are not yet implemented
        }
      }
      else
      {
        debug_log("Error during read for CMD_CREATETOOLHELP32SNAPSHOTEX\n");
        fflush(stdout);
        close(currentsocket);
        return 0;
      }
      break;
    }

    case CMD_CREATETOOLHELP32SNAPSHOT:
    {
      CeCreateToolhelp32Snapshot params;
      HANDLE result;

      //debug_log("CMD_CREATETOOLHELP32SNAPSHOT\n");

      if (recvall(currentsocket, &params, sizeof(CeCreateToolhelp32Snapshot), MSG_WAITALL) > 0)
      {
        //debug_log("Calling CreateToolhelp32Snapshot\n");
        result=CreateToolhelp32Snapshot(params.dwFlags, params.th32ProcessID);
       // debug_log("result of CreateToolhelp32Snapshot=%d\n", result);

       // fflush(stdout);





        sendall(currentsocket, &result, sizeof(HANDLE), 0);

      }
      else
      {
        debug_log("Error during read for CMD_CREATETOOLHELP32SNAPSHOT\n");
        fflush(stdout);
        close(currentsocket);
        return 0;
      }

      break;
    }

    case CMD_MODULE32FIRST: //slightly obsolete now
    case CMD_MODULE32NEXT:
    {
      HANDLE toolhelpsnapshot;
      if (recvall(currentsocket, &toolhelpsnapshot, sizeof(toolhelpsnapshot), MSG_WAITALL) >0)
      {
        BOOL result;
        ModuleListEntry me;
        CeModuleEntry *r;
        int size;

        if (command==CMD_MODULE32FIRST)
          result=Module32First(toolhelpsnapshot, &me);
        else
          result=Module32Next(toolhelpsnapshot, &me);

        if (result)
        {
          size=sizeof(CeModuleEntry)+ strlen(me.moduleName);
          r=(PCeModuleEntry)malloc(size);
          r->modulebase=me.baseAddress;
          r->modulesize=me.moduleSize;
          r->modulenamesize=strlen(me.moduleName);
          r->modulepart=me.part;


          // Sending %s size %x\n, me.moduleName, r->modulesize
          memcpy((char *)r+sizeof(CeModuleEntry), me.moduleName, r->modulenamesize);
        }
        else
        {
          size=sizeof(CeModuleEntry);
          r=(PCeModuleEntry)malloc(size);
          r->modulebase=0;
          r->modulesize=0;
          r->modulenamesize=0;
          r->modulepart=0;
        }

        r->result=result;
/*
        if (result)
        {
          debug_log("CMD_MODULE32 returning %s : base=%x size=%x part=%d (me.part=%d)\n", me.moduleName, r->modulebase, r->modulesize, r->modulepart, me.part);

        }
        else
          debug_log("CMD_MODULE32 returning <nomodule> : base=%x size=%x part=%d\n", r->modulebase, r->modulesize, r->modulepart);*/

        sendall(currentsocket, r, size, 0);

        free(r);
      }
      break;
    }

    case CMD_PROCESS32FIRST: //obsolete
    case CMD_PROCESS32NEXT:
    {
      HANDLE toolhelpsnapshot;
      if (recvall(currentsocket, &toolhelpsnapshot, sizeof(toolhelpsnapshot), MSG_WAITALL) >0)
      {
        ProcessListEntry pe;
        BOOL result;
        CeProcessEntry *r;
        int size;

        if (command==CMD_PROCESS32FIRST)
          result=Process32First(toolhelpsnapshot, &pe);
        else
          result=Process32Next(toolhelpsnapshot, &pe);

        //  debug_log("result=%d\n", result);

        if (result)
        {
          size=sizeof(CeProcessEntry)+ strlen(pe.ProcessName);
          r=(PCeProcessEntry)malloc(size);
          r->processnamesize=strlen(pe.ProcessName);
          r->pid=pe.PID;
          memcpy((char *)r+sizeof(CeProcessEntry), pe.ProcessName, r->processnamesize);
        }
        else
        {
          size=sizeof(CeProcessEntry);
          r=(PCeProcessEntry)malloc(size);
          r->processnamesize=0;
          r->pid=0;
        }

        r->result=result;

        sendall(currentsocket, r, size, 0);

        free(r);

      }
      break;
    }

    case CMD_READPROCESSMEMORY:
    {
      CeReadProcessMemoryInput c;

      r=recvall(currentsocket, &c, sizeof(c), MSG_WAITALL);
      if (r>0)
      {
        PCeReadProcessMemoryOutput o=NULL;
        o=(PCeReadProcessMemoryOutput)malloc(sizeof(CeReadProcessMemoryOutput)+c.size);

        o->read=ReadProcessMemory(c.handle, (void *)(uintptr_t)c.address, &o[1], c.size);

        if (c.compress)
        {
          //compress the output
#define COMPRESS_BLOCKSIZE (64*1024)
          int i;
          unsigned char *uncompressed=(unsigned char *)&o[1];
          uint32_t uncompressedSize=o->read;
          uint32_t compressedSize=0;
          int maxBlocks=1+(c.size / COMPRESS_BLOCKSIZE);

          unsigned char **compressedBlocks=malloc(maxBlocks*sizeof(unsigned char *) ); //send in blocks of 64kb and reallocate the pointerblock if there's not enough space
          int currentBlock=0;

          z_stream strm;
          strm.zalloc = Z_NULL;
          strm.zfree = Z_NULL;
          strm.opaque = Z_NULL;
          deflateInit(&strm, c.compress);

          compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
          strm.avail_out=COMPRESS_BLOCKSIZE;
          strm.next_out=compressedBlocks[currentBlock];

          strm.next_in=uncompressed;
          strm.avail_in=uncompressedSize;

          while (strm.avail_in)
          {
            r=deflate(&strm, Z_NO_FLUSH);
            if (r!=Z_OK)
            {
              if (r==Z_STREAM_END)
                break;
              else
              {
                debug_log("Error while compressing\n");
                break;
              }
            }

            if (strm.avail_out==0)
            {
              //new output block
              currentBlock++;
              if (currentBlock>=maxBlocks)
              {
                //list was too short, reallocate
                debug_log("Need to realloc the pointerlist (p1)\n");

                maxBlocks*=2;
                compressedBlocks=realloc(compressedBlocks, maxBlocks*sizeof(unsigned char*));
              }
              compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
              strm.avail_out=COMPRESS_BLOCKSIZE;
              strm.next_out=compressedBlocks[currentBlock];
            }
          }
          // finishing compressiong
          while (1)
          {

            r=deflate(&strm, Z_FINISH);

            if (r==Z_STREAM_END)
              break; //done

            if (r!=Z_OK)
            {
              debug_log("Failure while finishing compression:%d\n", r);
              break;
            }

            if (strm.avail_out==0)
            {
              //new output block
              currentBlock++;
              if (currentBlock>=maxBlocks)
              {
                //list was too short, reallocate
                debug_log("Need to realloc the pointerlist (p2)\n");
                maxBlocks*=2;
                compressedBlocks=realloc(compressedBlocks, maxBlocks*sizeof(unsigned char*));
              }
              compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
              strm.avail_out=COMPRESS_BLOCKSIZE;
              strm.next_out=compressedBlocks[currentBlock];
            }
          }
          deflateEnd(&strm);

          compressedSize=strm.total_out;
          // Sending compressed data
          sendall(currentsocket, &uncompressedSize, sizeof(uncompressedSize), MSG_MORE); //followed by the compressed size
          sendall(currentsocket, &compressedSize, sizeof(compressedSize), MSG_MORE); //the compressed data follows
          for (i=0; i<=currentBlock; i++)
          {
            if (i!=currentBlock)
              sendall(currentsocket, compressedBlocks[i], COMPRESS_BLOCKSIZE, MSG_MORE);
    