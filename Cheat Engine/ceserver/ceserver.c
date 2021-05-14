
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
  return sendall(s, &va