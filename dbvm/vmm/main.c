/* vmm.c: This is the virtual machine
 * It will be loaded at virtual address 0x00400000 (vmma.asm that is which just jumps short to intialize paging)
 * On initialization 0 to 4MB is identity mapped, to the stored memory regions are available to mess with
 */



#include "common.h"
#include "main.h"
#include "mm.h"
#include "neward.h"
#include "apic.h"
#include "displaydebug.h"

#include "multicore.h"
#include "inthandlers.h"
#include "vmmhelper.h"
#include "vmeventhandler.h"

#include "distorm.h"
#include "keyboard.h"

#include "pci.h"
#include "offloados.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"

#include "test.h"
#include "vmcall.h"
#include "vmpaging.h"
#include "vmxsetup.h"
#include "vmcall.h"
#include "exports.h"

#include "luahandler.h"
#include "interrupthandler.h"


//#include "psod.h" //for pink screen of death support

/*
//#include "logo.c"
*/

#define APIC_ID_OFFSET 0x020
#define APIC_SVR_OFFSET 0x0f0




void menu2(void);





unsigned char *ffpage=NULL;
PPTE_PAE   ffpagetable=NULL;
PPDE_PAE   ffpagedir=NULL;


PTSS testTSS=NULL;

int cpu_stepping;
int cpu_model;
int cpu_familyID;
int cpu_type;
int cpu_ext_modelID;
int cpu_ext_familyID;

int debugtestvar=0x12345678;


unsigned long long IA32_APIC_BASE=0xfee00000;
unsigned long long APIC_ID=0xfee00020;
unsigned long long APIC_SVR=0xfee000f0;

unsigned int BOOT_ID=0xffffffff;

extern unsigned int isAP;
volatile int AP_Terminate; //set to 1 to terminate the AP cpu's
volatile int AP_Launch; //set to 1 to launch the AP cpu's

#if (DISPLAYDEBUG==0)
int needtospawnApplicationProcessors=1;
#else
int needtospawnApplicationProcessors=0; //the display is cluttered as it is
#endif




#ifdef DEBUG
int autostart=0; //since simnow amd emu doesn't support serial import...
#else
int autostart=1;
#endif //debug

//int isrunning=0;

pcpuinfo firstcpuinfo, lastaddedcpuinfo; //just for debugging, nothing important




char bootdisk;





void startNextCPU(void)
{
  //setup a stack for the first AP cpu
  nextstack=(QWORD)malloc2(4096*16);
  markPageAsNotReadable((void *)nextstack);  //when the thread tries to allocate more than it can it'll cause a pagefault instead of fucking with other memory

  nextstack=(QWORD)nextstack+(4096*16)-16;

  sendstringf("startNextCPU. nextstack=%6\n", nextstack);

  asm volatile ("": : :"memory");
  initcs=0; //let the next cpu pass
  asm volatile ("": : :"memory");
}


void CheckCRCValues(void)
{
  unsigned int newcrc;

  sendstringf("Original VMM crc = %x\n\r",originalVMMcrc);
  newcrc=generateCRC((void *)vmxloop,0x2a000);
  sendstringf("Current  VMM crc = %x\n\r",newcrc);
  if (originalVMMcrc!=newcrc)
  {
    sendstring("!!!!!!!!!!MISMATCH!!!!!!!!!!\n\r");
  }


  return;
}



void vmm_entry2_hlt(pcpuinfo currentcpuinfo)
{
  UINT64 a,b,c,d;

  nosendchar[getAPICID()]=0;

  if (currentcpuinfo)
    sendstringf("CPU %d : Terminating...\n\r",currentcpuinfo->cpunr);
  else
    sendstringf("Unknown(%d) terminating...", getcpunr() );

  while (1)
  {
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

    if (currentcpuinfo)
      currentcpuinfo->active=0;
    a=1;
    _cpuid(&a,&b,&c,&d);  //always serialize after writing to a structure read by a different cpu
    __asm("hlt\n\r");
  }
}


void setupFSBase(void *fsbase)
{
  writeMSR(IA32_FS_BASE_MSR, (UINT64)fsbase);
}

volatile int inflooponerror=1;
volatile int initializedCPUCount; //assume the first (main) cpu managed to start up

void vmm_entry2(void)
//Entry for application processors
//Memory manager has been initialized and GDT/IDT copies have been made
{
  unsigned int cpunr;
  if (AP_Terminate)
  {
    sendstringf("AP_Terminate!=0\n");
    startNextCPU();
    vmm_entry2_hlt(NULL);
  }

  if