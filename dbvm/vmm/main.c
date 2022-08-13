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

  sendstring("Welcome to Dark Byte\'s Virtual Machine Manager\n\r");
  sendstringf("pagedirlvl4=%6\n\r",(unsigned long long)pagedirlvl4);

  sendstring("Initializing MM\n\r");
  InitializeMM((QWORD)pagedirlvl4+4096);
  sendstring("Initialized MM\n\r");
  /*
   * POST INIT
   */


  cpucount=1;
  cpuinfo=malloc2(sizeof(tcpuinfo)<4096?4096:sizeof(tcpuinfo));
  sendstringf("allocated cpuinfo at %6\n\r", cpuinfo);
  zeromemory(cpuinfo,sizeof(tcpuinfo));
  cpuinfo->active=1;
  cpuinfo->cpunr=0;
  cpuinfo->apicid=getAPICID();
  cpuinfo->isboot=1;
  cpuinfo->self=cpuinfo;



  setupFSBase((void*)cpuinfo);


  //debug info
  firstcpuinfo=cpuinfo;
  lastaddedcpuinfo=cpuinfo;

#if DISPLAYDEBUG==1
  initialize_displaydebuglogs();
#endif


  sendstringf("initialized cpuinfo at %6\n\r", cpuinfo);


  //IA32_APIC_BASE=(unsigned long long)readMSR(0x1b);

  IA32_APIC_BASE=(QWORD)mapPhysicalMemory((unsigned long long)readMSR(0x1b) & 0xfffffffffffff000ULL, 4096);
  sendstringf("IA32_APIC_BASE=%6\n\r",IA32_APIC_BASE);

  APIC_ID=IA32_APIC_BASE+APIC_ID_OFFSET;
  APIC_SVR=IA32_APIC_BASE+APIC_SVR_OFFSET;

  SetPageToWriteThrough((void*)IA32_APIC_BASE);





  displayline("IA32_APIC_BASE=%6\n\r",IA32_APIC_BASE);
  sendstringf("IA32_APIC_BASE=%6\n\r",IA32_APIC_BASE);
  sendstringf("\tLocal APIC base=%6\n\r",IA32_APIC_BASE & 0xfffff000);
  sendstringf("\tAPIC global enable/disable=%d\n\r",(IA32_APIC_BASE >> 11) & 1);
  sendstringf("\tBSP=%d\n\r",(IA32_APIC_BASE >> 8) & 1);

  a=1;
  _cpuid(&a,&b,&c,&d);
  displayline("CPUID.1: %8, %8, %8, %8\n",a,b,c,d);

  cpu_stepping=a & 0xf;
  cpu_model=(a >> 4) & 0xf;
  cpu_familyID=(a >> 8) & 0xf;
  cpu_type=(a >> 12) & 0x3;
  cpu_ext_modelID=(a >> 16) & 0xf;
  cpu_ext_familyID=(a >> 20) & 0xff;


  cpu_model=cpu_model + (cpu_ext_modelID << 4);
  cpu_familyID=cpu_familyID + (cpu_ext_familyID << 4);


 // if (0)
  if (1) //((d & (1<<28))>0) //this doesn't work in vmware, so find a different method
  {
    QWORD entrypage=0x30000;
    unsigned long long initialcount;
    unsigned int foundcpus;
    sendstring("Multi processor supported\n\r");
    sendstring("Launching application cpu's\n");


    //displaystring("Multi processor supported\n");
    displayline("Launching other cpu cores if present\n");
    sendstring("Starting other cpu's\n\r");


    APStartsInSIPI=1;

    if ((needtospawnApplicationProcessors>1) || (needtospawnApplicationProcessors<0))
    {
      sendstringf("memory corruption\n");
      volatile int debug=1;
      while (debug) ;
    }

    if (loadedOS)
    {
      sendstringf("mapping loadedOS (%6)...\n", loadedOS);

      POriginalState original=(POriginalState)mapPhysicalMemory(loadedOS, sizeof(OriginalState));
      sendstringf("Success. It has been mapped at virtual address %6\n",original);

      entrypage=original->APEntryPage;

      sendstringf("original->cpucount=%d\n", original->cpucount);
      if (original->cpucount>1000)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("More than 1000 cpu\'s are currently not supported\n");
        ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
        while (1);
      }

      if (original->cpucount)
      {
        needtospawnApplicationProcessors=0;
        foundcpus=original->cpucount;
        APStartsInSIPI=0; //AP should start according to the original state
      }

      if (original->FrameBufferBase)
      {
        DDFrameBufferBase=original->FrameBufferBase;
        DDFrameBufferSize=original->FrameBufferSize;
        DDHorizontalResolution=original->HorizontalResolution;
        DDVerticalResolution=original->VerticalResolution;
        DDPixelsPerScanLine=original->PixelsPerScanLine;

        if (DDFrameBufferBase)
        {
          /*
          char c=0;
          sendstring("Before mapping of the framebuffer\n");
          while (c==0)
          {
            c=waitforchar();
          }
          sendstring("Mapping framebuffer\n");*/

          //DDFrameBuffer=mapPhysicalMemoryGlobal(0x90000000, 8294400);
          //DDFrameBuffer=(unsigned char *)mapPhysicalMemory(0x90000000, 8294400);


          DDFrameBuffer=(unsigned char *)mapPhysicalMemoryGlobal(DDFrameBufferBase, DDFrameBufferSize);
          if (DDFrameBuffer==NULL)
          {
            sendstring("Failure mapping memory");
          }
          else
          {



         /* SetPageToWriteThrough(DDFrameBuffer);*/
            ddDrawRectangle(0,0,DDHorizontalResolution, DDVerticalResolution,0x00ff00);
        /*    ddDrawRectangle(0,0,100,100,0xff00ff);
            ddDrawRectangle(DDHorizontalResolution-100,0,100,100,0xff0000);
            ddDrawRectangle(0,DDVerticalResolution-100,100,100,0x0000ff);
            ddDrawRectangle(DDHorizontalResolution-100,DDVerticalResolution-100,100,100,0xffffff);*/

/*
            unsigned int pi;
            for (pi=0; pi<DDFrameBufferSize; pi++)
            {
              DDFrameBuffer[pi]=0x30;
            }*/
          }

        }
      }


      //while (1) ;
      unmapPhysicalMemory(original, sizeof(OriginalState));
    }

    if (needtospawnApplicationProcessors) //e.g UEFI boot with missing mpsupport
    {
      sendstringf("needtospawnApplicationProcessors!=0\n");
#ifndef NOMP

      BOOT_ID=apic_getBootID();

      //setup some info so that the AP cpu can find  this
      APBootVar_CR3=getCR3();

      void *GDTbase=(void *)getGDTbase();
      int GDTsize=getGDTsize();
      APBootVar_Size=GDTsize;

      if (GDTsize>192)
      {
        sendstringf("Update the AP boot GDT section to be bigger than 192 bytes");
      }
      memcpy(APBootVar_GDT, GDTbase, GDTsize);

      APBootVar_GDT[2 ]=0x00cf9b000000ffffULL;  //24: 32-bit code
      APBootVar_GDT[2 ]|=(entrypage >> 8) << 24;
      //with 0x5e000 or it with 0x05e0

      sendstringf("vmmentrycount before launch=%d\n", vmmentrycount);
      foundcpus=initAPcpus(entrypage);

      sendstringf("foundcpus=%d cpucount=%d. Waiting till cpucount==foundcpus, or timeout\n",foundcpus, cpucount);

      QWORD timeout=2000000000ULL;

      for (i=0; i<3; i++)
      {
        initialcount=_rdtsc();
        while ((_rdtsc()-initialcount) < timeout)
        {
          //sendstringf("cpucount=%d foundcpus=%d\n\r", cpucount, foundcpus);

          if (vmmentrycount>=foundcpus)
            break;

          _pause();
        }
        displayline(".");
        if (vmmentrycount>=foundcpus)
          break;
      }

      if (i>=3)
        displayline("Timeout\n");
      else
        displayline("\n");

    }
    else
      AP_Launch=1; //no need to let the others wait. the launcher will decide when to load

    displayline("Wait done. Cpu's found : %d (expected %d)\n",vmmentrycount, foundcpus);
    sendstringf("vmmentrycount after launch=%d\n", vmmentrycount);

    //the other CPU's should now be waiting in the spinlock at the start of dbvm
#endif
  }

  //copy GDT and IDT to VMM memory
  GDT_BASE=malloc(4096);
  GDT_SIZE=4096; //getGDTsize();

  if (GDT_BASE==NULL)
  {
    nosendchar[getAPICID()]=0;
    sendstring("Memory allocation failed\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xc8);
  }

  sendstringf("Allocated GDT_BASE %6\n", GDT_BASE);

  {
    void *GDTbase=(void *)getGDTbase();
    int GDTsize=getGDTsize();

    sendstringf("getGDTbase=%p, getGDTsize=%d\n",GDTbase,GDTsize );

    copymem(GDT_BASE,GDTbase,GDTsize);
  }
  sendstringf("Allocated and copied GDT to %x\n\r",(UINT64)GDT_BASE);
  setGDT((UINT64)GDT_BASE, 4096);

  {
    //set the GDT to the way I like it (loaders could fuck this up)
    QWORD *g=(QWORD *)GDT_BASE;
    g[0 ]=0;            //0 :
    g[1 ]=0x00cf92000000ffffULL;  //8 : 32-bit data
    g[2 ]=0x00cf96000000ffffULL;  //16: test, stack, failed, unused
    g[3 ]=0x00cf9b000000ffffULL;  //24: 32-bit code
    g[4 ]=0x00009a000000ffffULL;  //32: 16-bit code
    g[5 ]=0x000092000000ffffULL;  //40: 16-bit data
    g[6 ]=0x00009a030000ffffULL;  //48: 16-bit code, starting at 0x30000
    g[7 ]=0;            //56: 32-bit task
    g[8 ]=0;            //64: 64-bit task
    g[9 ]=0;            //72:  ^   ^   ^
    g[10]=0x00af9b000000ffffULL;  //80: 64-bit code
    g[11]=0x00cf9b000000ffffULL;  //88: 32-bit code compat mode
    g[12]=0;            //96: 64-bit tss descriptor (2)
    g[13]=0;            //104: ^   ^   ^
  }

  //now replace the old IDT with a new one
  intvector=malloc(sizeof(INT_VECTOR)*256);
  zeromemory(intvector,sizeof(INT_VECTOR)*256);
  sendstringf("Allocated intvector at %6\n\r",(unsigned long long)intvector);

  setints();
  sendstring("after setints()\n");

  i=0;
  setDR0((QWORD)((volatile void *)&&BPTest));
  setDR6(0xffff0ff0);
  setDR7(getDR7() | (1<<0));
  displayline("Going to execute test breakpoint\n");

  cpuinfo->OnInterrupt.RSP=getRSP();
  cpuinfo->OnInterrupt.RBP=getRBP();
  cpuinfo->OnInterrupt.RIP=(QWORD)((volatile void *)&&AfterBPTest);
  asm volatile ("": : :"memory");
BPTest:
  i=1;
  sendstring("<<---------------WRONG!!! BPTest got executed...(ok if a jtag debugger is present)\n");
  asm volatile ("": : :"memory");
AfterBPTest:
  if (i==0)
    sendstringf("BPTest success.  dr6=%6\n", getDR6());
  else
    sendstring(":(\n");

  cpuinfo->OnInterrupt.RSP=0;
  cpuinfo->OnInterrupt.RBP=0;
  cpuinfo->OnInterrupt.RIP=0;

  try
  {
    sendstring("Trying memory access BP\n");

    regDR7 dr7;
    dr7.DR7=getDR7();

    dr7.L1=1;
    dr7.G1=1;
    dr7.RW1=3;
    dr7.LEN1=3;

