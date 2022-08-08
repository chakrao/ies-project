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

  if ((needtospawnApplicationProcessors>1) || (needtospawnApplicationProcessors<0))
  {
    sendstringf("memory corruption\n");

    while (inflooponerror) ;
  }


  //debug code on my 8 core cpu:
  //if (initializedCPUCount>=8)
  //{
  //  sendstringf("More cpu's than expected\n");
  //  while (inflooponerror);
  //}


  //setup the GDT and IDT
  setGDT((UINT64)GDT_BASE, GDT_SIZE);
  setIDT((UINT64)intvector, 16*256);

  //cLIDT(&intvector);

  //sendstringf("Welcome to a extra cpu (cpunr=%d)\n",cpucount);
  cpunr=initializedCPUCount;
  sendstringf("Setting up cpunr=%d\n",cpunr);


  initializedCPUCount++;

  if (!loadedOS)
    cpucount++; //cpucount is known, don't increase it

  pcpuinfo cpuinfo=malloc2(sizeof(tcpuinfo)<4096?4096:sizeof(tcpuinfo)<4096);

  zeromemory(cpuinfo, sizeof(tcpuinfo));
  cpuinfo->active=1;
  cpuinfo->cpunr=cpunr;
  cpuinfo->apicid=getAPICID();

  cpuinfo->TSbase=0;
  cpuinfo->TSlimit=0xffff;
  cpuinfo->TSsegment=0;
  cpuinfo->TSaccessRights=0x8b;
  cpuinfo->self=cpuinfo;

  lastaddedcpuinfo->next=cpuinfo;
  lastaddedcpuinfo=cpuinfo;

  displayline("%d: New CPU CORE. CPUNR=%d APICID=%d (cpuinfo struct at : %p rsp=%x)\n",cpunr, cpuinfo->cpunr, cpuinfo->apicid, cpuinfo, getRSP());


  setupFSBase((void *)cpuinfo);

  sendstringf("%d: launching next CPU\n", cpunr);
  startNextCPU(); //put at start for async

  sendstringf("%d: Waiting till AP_Launch is not 0\n", cpunr);

  while (AP_Launch==0)
  {
    resync();
    if (AP_Terminate==1)
      vmm_entry2_hlt(cpuinfo);
  }

  if (AP_Terminate==1)
  {
    startNextCPU();
    vmm_entry2_hlt(cpuinfo);
  }

  sendstringf("Starting VMX for cpu %d\n", cpunr);

  displayline("CPU CORE %d: entering VMX mode\n",cpunr);

  startvmx(cpuinfo);

  nosendchar[getAPICID()]=0;
  sendstringf("Application cpu returned from startvmx\n\r");

  vmm_entry2_hlt(cpuinfo);
  while (1);
}





void vmm_entry(void)
{
  //make sure WP is on
  enableserial();

  sendstringf("vmm_entry\n");

  setCR0(getCR0() | CR0_WP);
  writeMSR(EFER_MSR, readMSR(EFER_MSR) | (1<<11)); //no execute
  //setCR4(getCR4() | CR4_SMEP);

  if (isAP)
  {
    vmm_entry2();

    nosendchar[getAPICID()]=0;
    sendstringf("vmm_entry2 has PHAILED!!!!");
    while (1) outportb(0x80,0xc7);
  }
  isAP=1; //all other entries will be an AP

  initializedCPUCount=1; //I managed to run this at least...

  int i,k;
  UINT64 a,b,c,d;
  pcpuinfo cpuinfo;


  //stack has been properly setup, so lets allow other cpu's to launch as well
  InitCommon();

  Password1=0x76543210; //later to be filled in by user, sector on disk, or at compile time
  Password2=0xfedcba98;
  Password3=0x90909090;

  /*version 1 was the 32-bit only version,
   * 2 added 64-bit,
   * 3 had a revised int1 redirect option,
   * 4 has major bugfixes,
   * 5=more fixes and some basic device recog,
   * 6=Even more compatibility fixes,
   * rm emu, and new vmcalls,
   * 7=driver loading ,
   * 8=amd support,
   * 9 memory usage decrease and some fixes for newer systems,
   * 10=xsaves (win10)
   * 11=new memory manager , dynamic cpu initialization, UEFI boot support, EPT, unrestricted support, and other new features
   * 12=vpid
   * 13=basic TSC emulation
   * 14=properly emulate debug step
   * 15=some amd fixes/contiguous memory param/dbvmbp
   * 16=3th vmcall password
   */
  dbvmversion=16;
  int1redirection=1; //redirect to int vector 1 (might change this to the perfcounter interrupt in the future so I don't have to deal with interrupt prologue/epilogue)
  int3redirection=3;
  int14redirection=14;

  //get max physical address
  QWORD rax=0x80000008;
  QWORD rbx=0;
  QWORD rcx=0;
  QWORD rdx=0;
  _cpuid(&rax, &rbx, &rcx, &rdx);
  MAXPHYADDR=rax & 0xff;

  MAXPHYADDRMASK=0xFFFFFFFFFFFFFFFFULL;
  MAXPHYADDRMASK=MAXPHYADDRMASK >> MAXPHYADDR; //if MAXPHYADDR==36 then MAXPHYADDRMASK=0x000000000fffffff
  MAXPHYADDRMASK=~(MAXPHYADDRMASK << MAXPHYADDR); //<< 36 = 0xfffffff000000000 .  after inverse : 0x0000000fffffffff
  MAXPHYADDRMASKPB=MAXPHYADDRMASK & 0xfffffffffffff000ULL; //0x0000000ffffff000

  sendstringf("MAXPHYADDR=%d\n", MAXPHYADDR);
  sendstringf("MAXPHYADDRMASK=%6\n", MAXPHYADDRMASK);
  sendstringf("MAXPHYADDRMASKPB=%6\n", MAXPHYADDRMASKPB);





  //enableserial();




  sendstringf("If you see this that means that the transition from unpaged to paged was a success\n\r");
  sendstringf("loadedOS=%6\n",loadedOS);

  currentdisplayline=7;


  displayline("BOOT CPU CORE initializing\n");

  displayline("CR3=%6\n", getCR3());
  displayline("pagedirlvl4=%6\n",(UINT64)pagedirlvl4);
  displayline("&pagedirlvl4=%6\n",(UINT64)&pagedirlvl4);
  displayline("vmmstart=%6 (this is virtual address 00400000)\n",(UINT64)vmmstart);

  //displayline("press any key to continue\n");


  //initialize

  sendstring("Welcome to Dark Byte\'s Virtual Machine Manager\n\r")