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

    setDR7(dr7.DR7);
    setDR1((QWORD)((volatile void *)&debugtestvar));

    setDR6(0xffffffff);
    sendstringf("Before access DR6=%6\n",getDR6());
    asm volatile ("": : :"memory");
    debugtestvar=0;
    debugtestvar=890;
    asm volatile ("": : :"memory");

    sendstringf("Memory access BP failure\n DR1=%6 DR7=%6", getDR1(), getDR7());

  }
  except
  {
    sendstringf("Except block success.  dr6=%6\n", getDR6());
  }
  tryend




  sendstringf("Letting the first AP cpu go through\n");
  startNextCPU();



  fakeARD=malloc(4096);
  fakeARD[0].Type=255;
  sendstringf("Allocated fakeARD at %6\n",(unsigned long long)fakeARD);
  sendstringf("That is physical address %6\n", VirtualToPhysical(fakeARD));


  if (!loadedOS)
  {
    sendstring("Copying ARD from 80000 to ARD location");
    copymem((void *)fakeARD,(void *)0x80000,4096); //that should be enough
  }

  //ARD Setup
  //The ARD is used for old boot mechanisms. This way DBVM can tell the guest which physical pages are 'reserved' by the system and should not be used/overwritten
  //In loadedOS mode DBVM makes use of built-in memory allocation systems so not needed

  sendstring("Calling initARDcount()\n");
  initARDcount();
  sendstring("Calling sendARD()\n");
  sendARD();

  sendstring("after sendARD()\n");



  // alloc VirtualMachineTSS_V8086
  VirtualMachineTSS_V8086=malloc(3*4096);
  zeromemory(VirtualMachineTSS_V8086,3*4096);

  RealmodeRing0Stack=malloc(4096); //(not even 1 KB used, but let's waste some mem)


  //configure ffpage

  //create a page filled with 0xff (for faking non present memory access)
  ffpage=malloc(4096);
  for (i=0; i<4096; i++)
    ffpage[i]=0xce;

  sendstringf("Physical address of ffpage=%6\n\r",(UINT64)VirtualToPhysical(ffpage));


  //create a pagetable with only 0xff (2MB 0xff)
  ffpagetable=malloc(4096);
  zeromemory(ffpagetable,4096);
  for (i=0; i<4096/8; i++)
  {
    *(QWORD*)(&ffpagetable[i])=VirtualToPhysical(ffpage);
    ffpagetable[i].P=1;
    ffpagetable[i].RW=0;
  }

  sendstringf("Physical address of ffpagetable=%6\n\r",(UINT64)VirtualToPhysical((void *)ffpagetable));

  //create a pagedir where all entries point to the ffpagetable
  ffpagedir=malloc(4096);
  zeromemory(ffpagedir,4096);
  for (i=0; i<4096/8; i++)
  {
    *(QWORD*)(&ffpagedir[i])=VirtualToPhysical((void *)ffpagetable);
    ffpagedir[i].P=1;
    ffpagedir[i].RW=0;
    ffpagedir[i].PS=0;
  }

  sendstringf("Physical address of ffpagedir=%6\n\r",(UINT64)VirtualToPhysical((void *)ffpagedir));

  __asm("mov %cr3,%rax\n  mov %rax,%cr3\n");

  displayline("emulated virtual memory has been configured\n");

  displayline("Paging:\n");
  displayline("0x00000000 is at %6\n", (UINT64)VirtualToPhysical((void *)0));
  displayline("0x00200000 is at %6\n", (UINT64)VirtualToPhysical((void *)0x00200000));
  displayline("0x00400000 is at %6\n", (UINT64)VirtualToPhysical((void *)0x00400000));
  displayline("0x00600000 is at %6\n", (UINT64)VirtualToPhysical((void *)0x00600000));



  //setup nonpagedEmulationPagedir


  displayline("Calling hascpuid()\n");
	if (hascpuid())
	{
		char *t;
		a=0;
		b=0;
		c=0;
		d=0;
		_cpuid(&a,&b,&c,&d);
		sendstringf("Your comp supports cpuid! (%d , %x %x %x )\n\r",a,b,d,c);

		t=(char *)&b;
		sendstringf("Max basicid=%x\n\r",a);
		sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);


		t=(char *)&d;
		sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);


		t=(char *)&c;
		sendstringf("%c%c%c%c\n\r",t[0],t[1],t[2],t[3]);



    if ((b==0x68747541) && (d==0x69746e65) && (c==0x444d4163))
    {
      isAMD=1;
      vmcall_instr=vmcall_amd;
      AMD_hasDecodeAssists=0;
      sendstring("This is an AMD system. going to use the AMD virtualization tech\n\r");
    }
    else
    {
      isAMD=0;
      vmcall_instr=vmcall_intel;
    }

    //a=0x80000000; _cpuid(&a,&b,&c,&d);
    //if (!(a & 0x80000000))
    {
      unsigned int j;

     // sendstring("\n\r\n\rBranch string=");
      displayline("Branch string=");

      for (j=0x80000002; j<=0x80000004; j++)
      {
        a=j;
        _cpuid(&a,&b,&c,&d);

        t=(char *)&a;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
        //sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&b;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
        //sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&c;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
      //  sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&d;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
     //   sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);
      }
    //  sendstringf("\n\r");
      displayline(" \n");

    }


	}
	else
  {
		sendstring("Your crappy comp doesn\'t support cpuid\n\r");
    displayline("Your system does not support CPUID\n");
  }

  cpuinfo->TSbase=0;
  cpuinfo->TSlimit=0xffff;
  cpuinfo->TSsegment=0;
  cpuinfo->TSaccessRights=0x8b;

  sendstringf("Setting up idttable and jumptable\n\r");

  jumptable=malloc(4096);
  idttable32=malloc(4096);

  sendstringf("jumptable allocated at %x (%6)\n\r",(UINT64)jumptable, VirtualToPhysical(jumptable));
  sendstringf("idttable32 allocated at %x (%6)\n\r",(UINT64)idttable32, VirtualToPhysical(idttable32));

  //fill jumptable and IDT
  PINT_VECTOR32 idt32=(PINT_VECTOR32)idttable32;
  UINT64 pa=VirtualToPhysical(jumptable);
  UINT64 inthandler32address=(UINT64)VirtualToPhysical(&inthandler_32);

  unsigned char *jumptablepc;
  jumptablepc=(unsigned char *)jumptable;
  for (i=0; i<256; i++, pa+=10)
  {
    //push i
    jumptablepc[i*10]=0x68; //push
    *(DWORD *)&jumptablepc[i*10+1]=i;

    //jmp inthandler_32
    jumptablepc[i*10+5]=0xe9;
    *(DWORD *)(&jumptablepc[i*10+6])=inthandler32address-(pa+10); //from (current offset+5(including push 7)) to inthandler_32
    idt32[i].bAccess=0x8e;
    idt32[i].bUnused=0;
    idt32[i].wHighOffset=pa >> 16;
    idt32[i].wLowOffset=pa & 0xffff;
    idt32[i].wSelector=24;

    //sendstringf("idt32[%x]=%x\n\r",i, *(UINT64*)&idt32[i]);
  }

  sendstring("setting up gdt entry at offset 0x64 as virtual8086 task\n\r");
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();


  sendstringf("currentgdt is %x (limit=%x)\n\r",(UINT64)currentgdt, getGDTsize());
  ULONG length=(ULONG)sizeof(TSS)+32+8192+1;


  currentgdt[8].Limit0_15=length;
  currentgdt[8].Base0_23=(QWORD)VirtualMachineTSS_V8086;
  currentgdt[8].Type=0x9;
  currentgdt[8].NotSystem=0;
  currentgdt[8].DPL=3;
  currentgdt[8].P=1;
  currentgdt[8].Limit16_19=length >> 16;
  currentgdt[8].AVL=1;
  currentgdt[8].L=0;
  currentgdt[8].B_D=0;
  currentgdt[8].G=0;
  currentgdt[8].Base24_31=(QWORD)VirtualMachineTSS_V8086 >> 24;
  *(QWORD*)&currentgdt[9]=((QWORD)VirtualMachineTSS_V8086) >> 32;

  //setup GDT for realmode jump
  currentgdt[4].Base0_23=0x20000;


  //if (!loadedOS)
  {
    sendstringf("Setting up 64-bit TS and TSS\n\r");

    TSS64 *temp=malloc(4096);

    sendstringf("temp allocated at %x\n", temp);
    ownTSS=temp;
    zeromemory(temp,4096);
    currentgdt=(PGDT_ENTRY)(getGDTbase()+96);

    currentgdt[0].Limit0_15=4096;
    currentgdt[0].Base0_23=(UINT64)temp;
    currentgdt[0].Type=0x9;
    currentgdt[0].NotSystem=0;
    currentgdt[0].DPL=0;
    currentgdt[0].P=1;
    currentgdt[0].Limit16_19=4096 >> 16;
    currentgdt[0].AVL=1;
    currentgdt[0].L=0;
    currentgdt[0].B_D=0;
    currentgdt[0].G=0;
    currentgdt[0].Base24_31=((UINT64)temp) >> 24;
    *(QWORD*)&currentgdt[1]=((UINT64)temp) >> 32;

    temp->IST1=(QWORD)malloc(8192)+8192-0x10; //panic stack

    loadTaskRegister(96);
  }

  displayline("Generating debug information\n\r");
  originalVMMcrc=generateCRC((void*)vmxloop,0x2a000);


  displayline("Virtual machine manager loaded\n");




#ifdef DEBUG
  displayline("Entering menu system\n");
#else
  displayline("Skipping menu system and autostarting VM\n");
#endif

  {
    //mark the region between 0 to 0x00400000 as readonly, if you need to write, map it
    PPDPTE_PAE pml4entry;
    PPDPTE_PAE pagedirpointerentry;
    PPDE_PAE pagedirentry;
    PPTE_PAE pagetableentry;

    VirtualAddressToPageEntries(0, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
    pagedirentry[0].RW=0;
    pagedirentry[1].RW=0;
  }

#if DISPLAYDEBUG==0
  if (needtospawnApplicationProcessors)
#endif
    textmemory=(QWORD)mapPhysicalMemory(0xb8000, 4096); //at least enough for 80*25*2


  InitExports();

  setDR6(0xffff0ff0);

  //outportb(0x80,0x10);

  menu2();
  return;
}

//#pragma GCC push_options
//#pragma GCC optimize ("O0")
int testexception(void)
{

  volatile int result=2;


  bochsbp();

  __asm("nop");
  __asm("nop");
  __asm("nop");

  result=readMSRSafe(553);

  //nothing happened
  //result=0;
  displayline("result=%d\n", result);


  return result;
}

void vmcalltest(void)
{
  int dbvmversion;
  dbvmversion=0;

  try
  {
    dbvmversion=vmcalltest_asm();
    sendstringf("dbvm is loaded. Version %x\n", dbvmversion);
  }
  except
  {
    sendstringf("dbvm is not loaded\n");
  }
  tryend
}


//#pragma GCC pop_options


void apentryvmx()
{
  nosendchar[getAPICID()]=0;
 // sendstringf("Hello from %d", getAPICID());

  while (1)
  {
   // sendchar("-");
    QWORD eax,ebx,ecx,edx;
    _cpuid(&eax,&ebx,&ecx,&edx);


  }

}

void reboot(int skipAPTerminationWait)
{
  {
    //remapping pagetable entry 0 to 0x00400000 so it's writabe (was marked unwritable after entry)
    PPDPTE_PAE pml4entry;
    PPDPTE_PAE pagedirpointerentry;
    PPDE_PAE pagedirentry;
    PPTE_PAE pagetableentry;

    VirtualAddressToPageEntries(0, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
    pagedirentry[0].RW=1;
    pagedirentry[1].RW=1;
    asm volatile ("": : :"memory");
  }

  //Disable the AP cpu's as on a normal reboot, the memory they are looping in will first be zeroed out (it picks the same memory block)
  AP_Terminate=1; //tells the AP cpu's to stop

  if (skipAPTerminationWait==0) //can be skipped if ran by vmlaunch (the other cpu's will stay active as they are in wait-for-sipi mode)
  {
    startNextCPU(); //just make sure there is none waiting

    int stillactive=1;
    while (stillactive)
    {
      pcpuinfo c=firstcpuinfo->next;
      stillactive=0;
      while (c)
      {
        if ((c->vmxsetup==0) && (c->active)) //not configured to be a VMX, and still active
          stillactive=1;

        c=c->next;
      }
    }
  }

  UINT64 gdtaddress=getGDTbase();  //0x40002 contains the address of the GDT table

  sendstring("Copying gdt to low memory\n\r");
  copymem((void *)0x50000,(void *)(UINT64)gdtaddress,128); //copy gdt to 0x50000

  sendstring("copying movetoreal to 0x2000\n\r");
  copymem((void *)0x20000,(void *)(UINT64)&movetoreal,(UINT64)&vmxstartup_end-(UINT64)&movetoreal);


  sendstring("Calling quickboot\n\r");

  if (skipAPTerminationWait==0xcedead) //PSOD
    *(unsigned char *)0x7c0e=0xff;
  else
    *(unsigned char *)0x7c0e=bootdisk;



  quickboot();
  sendstring("WTF?\n\r");
}


#ifdef DEBUG
#define SHOWFIRSTMENU 1
int showfirstmenu=1;
#else
#define SHOWFIRSTMENU 0
int showfirstmenu=0;
#endif

void menu2(void)
{
  unsigned char key;





  sendstringf("loadedOS=%6\n",loadedOS);


  //*(BYTE *)0x7c0e=0x80;
  bootdisk=0x80;
  while (1)
  {
    clearScreen();
    currentdisplayline=0;

    {
    	vmcb x;
    	int offset;

    	offset=(QWORD)&x.DR6-(QWORD)&x;


        	displayline("DR6=%x\n", offset);


    }

    displayline("Welcome to the DBVM interactive menu\n\n");
    displayline("These are your options:\n");
    displayline("0: Start virtualization\n");
    displayline("1: Keyboard test\n");
    displayline("2: Set disk to startup from (currently %2)\n",bootdisk);
    displayline("3: Disassembler test\n");
    displayline("4: Interrupt test\n");
    displayline("5: Breakpoint test\n");
    displayline("6: Set Redirects with dbvm (only if dbvm is already loaded)\n");
    displayline("7: cr3 fuck test\n");
    displayline("8: PCI enum test (finds db's serial port)\n");
    displayline("9: test input\n");
    displayline("a: test branch profiling\n");
    displayline("b: boot without vm (test state vm would set)\n");
    displayline("v: control register test\n");
    displayline("e: efer test\n");
    displayline("o: out of memory test\n");
    displayline("n: NMI Test\n");

    if (getDBVMVersion())
    {
      displayline("w: DBVM write watch test\n");
      displayline("r: DBVM write read/write test\n");
      displayline("x: DBVM write execute test\n");
      displayline("c: DBVM cloak test\n");
      displayline("m: DBVM changeregonbp test\n");
    }


    key=0;
    while (!key)
    {

      if ((!loadedOS) || (showfirstmenu))
      {
#ifdef DELAYEDSERIAL
        if (!useserial)
          key='0';
        else
#endif
        if (loadedOS)
          key=waitforchar();
        else
          key=kbd_getchar();
      }
      else
        key='0';

      while (IntHandlerDebug) ;

      if (key)
      {
        char temps[16];
        displayline("%c\n", key);

        switch (key)
        {
          case '0':
            clearScreen();
            menu();
            break;

          case '1':
            displayline("kdbstatus=%2\n",kbd_getstatus());
            displayline("kdb_getoutputport=%2\n",kdb_getoutputport());
            displayline("kdb_getinputport=%2\n",kdb_getinputport());
            displayline("kdb_getcommandbyte=%2\n",kdb_getcommandbyte());
            break;

          case '2':
            displayline("Set the new drive: ");
            readstringc(temps,2,16);
            temps[15]=0;

            bootdisk=atoi2(temps,16,NULL);
            displayline("\nNew drive=%2 \n",bootdisk);
            break;

          case '3': //disassemblerr
            {
              _DecodedInst disassembled[22];
              unsigned int i;
              unsigned int used=0;
              distorm_decode((UINT64)&menu2,(unsigned char*)menu2, 256, Decode64Bits, disassembled, 22, &used);

              if (used)
              {
                for (i=0; i<used; i++)
                {
                  displayline("%x : %s - %s %s\n",
                    disassembled[i].offset,
                    disassembled[i].instructionHex.p,
                    disassembled[i].mnemonic.p,
                    disassembled[i].operands.p);
                }

              }
              else
              {
                displayline("Failure...\n");
              }


            }
            break;

          case '4':
          {
            __asm("sti"); //enable interrupts
            break;
          }

          case '5':
          {
            UINT64 rflags;
            pcpuinfo i=getcpuinfo();

            ClearDR6OnInterrupt=1;



            try
            {
              displayline("Doing an int3 bp\n");
              int3bptest();
              displayline("Failure to int3 break\n");
            }
            except
            {
              displayline("caught level 1 int3 :%d\n", lastexception);
            }
            tryend

            try
            {
              displayline("Doing an int3 bp 2\n");
              int3bptest();
              displayline("Failure to int3 break 2\n");
            }
            except
            {
              displayline("caught level 1 int 3 2:%d\n", lastexception);
            }
            tryend


            displayline("testing multilevel try/except\n");
            try
            {
              displayline("inside try. Entering 2nd try\n");
              try
              {
                displayline("inside 2nd try. calling int3bptest\n");
                int3bptest();
                displayline("int3bptest in level 2 failed to get caught\n");
              }
              except
              {
                displayline("caught level 2 int3:%d\n", lastexception);
              }
              tryend

              int3bptest();
              displayline("Failure to int3 break\n");
            }
            except
            {
              displayline("caught level 1 int3:%d\n", lastexception);
            }
            tryend

            displayline("Setting the GD flag");

            i->OnInterrupt.RSP=getRSP();
            i->OnInterrupt.RBP=getRBP();
            i->OnInterrupt.RIP=(QWORD)((volatile void *)&&afterGDtest);
            asm volatile ("": : :"memory");
            setDR6(0xffffffff);
            setDR7(getDR7() | (1<<13));
            asm volatile ("": : :"memory");
            setDR3(0x12345678);

            sendstringf("Failure to break on GD");

            asm volatile ("": : :"memory");
afterGDtest:

            //RF
            sendstringf("After GD test. DR6=%6\n", getDR6());


            displayline("Setting an execute breakpoint\n\r");
            setDR0((QWORD)getCR0);
            setDR6(0xffff0ff0);
            setDR7(getDR7() | (1<<0));
            displayline("Going to execute it\n");

            i->OnInterrupt.RSP=getRSP();
            i->OnInterrupt.RBP=getRBP();
            i->OnInterrupt.RIP=(QWORD)((volatile void *)&&afterEXBPtest);
            asm volatile ("": : :"memory");
            getCR0();

            sendstringf("Failure to break on execute\n");
            asm volatile ("": : :"memory");
afterEXBPtest:

            displayline("Setting a RW breakpoint\n\r");
            setDR0((QWORD)&isAP);
            setDR6(0xffff0ff0);
            setDR7(getDR7() | (3<<18) | (3<<16) | (1<<0));
            displayline("Going to write to that breakpoint\n");

            i->OnInterrupt.RSP=getRSP();
            i->OnInterrupt.RBP=getRBP();
            i->OnInterrupt.RIP=(QWORD)((volatile void *)&&afterWRBPtest);
            asm volatile ("": : :"memory");

            isAP++;
            asm volatile ("": : :"memory");
            sendstringf("Failure to break on write. %d\n", isAP);
afterWRBPtest:
            asm volatile ("": : :"memory");
            displayline("done writing\n");


            displayline("Setting the single step flag (this will give unhandled exceptions)\n\r");
            rflags=getRFLAGS(); //NO RF
            setRFLAGS(rflags | (1<<8));

            setRFLAGS(rflags & (~(1<<8))); //unset

            ClearDR6OnInterrupt=0;


            break;
          }

          case '6':
          {
            displayline("Setting the redirects. #UD Interrupt will fire if dbvm is not loaded (and crash)");
            vmcall_setintredirects();

            break;

          }

          case '7':
          {
            QWORD cr3=getCR3();
            displayline("CR3 was %6\n", cr3);

            cr3=cr3&0xfffffffffffff000ULL;
            setCR3(cr3);
            setCR4(getCR4() | CR4_PCIDE);

            cr3=cr3 | 2;
            setCR3(cr3);

            cr3=getCR3();
            displayline("CR3 is %6\n", cr3);

            cr3=cr3 | 0x8000000000000000ULL;
            setCR3(cr3);
            cr3=getCR3();
            displayline("CR3 is %6\n", cr3);

            break;
          }

          case '8':
          {
            //pci enum test
            pciConfigEnumPci();
            break;
          }

          case '9':
          {
            {
              char temps[17];
              UINT64 address;
              int size;
              int err2,err3;

              sendstring("\nAddress:");
              readstring(temps,16,16);
              address=atoi2(temps,16,&err2);

              sendstring("\nNumber of bytes:");
              readstring(temps,16,16);
              size=atoi2(temps,10,&err3);

              {
                _DecodedInst disassembled[22];
                unsigned int i;
                unsigned int used=0;
                distorm_decode((UINT64)address,(unsigned char*)address, size, Decode64Bits, disassembled, 22, &used);

                if (used)
                {
                  for (i=0; i<used; i++)
                  {
                    displayline("%x : %s - %s %s\n",
                      disassembled[i].offset,
                      disassembled[i].instructionHex.p,
                      disassembled[i].mnemonic.p,
                      disassembled[i].operands.p);
                  }

                }
                else
                {
                  displayline("Failure...\n");
                }
              }

            }

            break;
          }

          case 'a':
          {
            testBranchPrediction();
            break;
          }

          case 'b':
          {
            reboot(0);
            displayline("WTF?\n");
            break;
          }

          case 'e':
          {
            QWORD old=readMSR(EFER_MSR);
            QWORD new;

            sendstringf("old=%6\n", old);

            new=old ^ (1<<11);
            new=new & (~(1<<10));
            sendstringf("new1=%6\n", new);
            writeMSR(EFER_MSR, new);

            new=readMSR(EFER_MSR);
            sendstringf("new2=%6\n", new);
            break;
          }

          case 'o':
          {
            int count;
            void *mem;

            while (1)
            {
              mem=malloc2(4096);
              if (mem==NULL)
              {
                sendstringf("alloc fail\n");
                while (1);
              }
              count++;
              if (count%10==0)
              {
                sendstringf("count=%d\n", count);
              }

            }


            break;
          }

          case 'v':
          {
            QWORD cr0=getCR0();
            sendstringf("CR0=%x\n", cr0);

            sendstring("Flipping WP\n");

            cr0=cr0 ^ CR0_WP;
            setCR0(cr0);
            cr0=getCR0();
            sendstringf("CR0=%x\n", cr0);

            sendstring("Flipping NE\n");
            cr0=cr0 ^ CR0_NE;
            setCR0(cr0);
            cr0=getCR0();
            sendstringf("CR0=%x\n", cr0);

            sendstring("Flipping NE again \n");
            cr0=cr0 ^ CR0_NE;
            setCR0(cr0);
            cr0=getCR0();
            sendstringf("CR0=%x\n", cr0);

            QWORD cr4=getCR4();
            sendstringf("CR4=%x\n", cr4);

            sendstring("Flipping CR4_OSXSAVE\n");

            cr4=cr4 ^ CR4_OSXSAVE;
            setCR4(cr4);
            cr4=getCR4();
            sendstringf("CR4=%x\n", cr4);

            sendstring("Flipping CR4_VMXE\n");

            cr4=cr4 ^ CR4_VMXE;
            setCR4(cr4);
            cr4=getCR4();
            sendstringf("CR4=%x\n", cr4);

            sendstring("Flipping CR4_VMXE again\n");

            cr4=cr4 ^ CR4_VMXE;
            setCR4(cr4);
            cr4=getCR4();
            sendstringf("CR4=%x\n", cr4);



            break;
          }

          case 'w':
          {
            dbvm_watch_writes_test();
            break;
          }

          case 'r':
          {
            dbvm_watch_reads_test();
            break;
          }

          case 'x':
          {
            dbvm_watch_execute_test();
            break;
          }

          case 'c':
          {
            dbvm_cloak_test();
            break;
          }

          case 'm':
          {
      