/*
 * vmxsetup.c
 *
 *  Created on: Jan 26, 2018
 *      Author: eric heijnen
 */


#include "vmxsetup.h"
#include "vmreadwrite.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"
#include "main.h"
#include "mm.h"
#include "vmmhelper.h"
#include "offloados.h"
#include "vmpaging.h"
#include "vmeventhandler.h"

#include "epthandler.h"
#include "neward.h"
#include "displaydebug.h"
#include "common.h"


criticalSection setupVMX_lock={.name="setupVMX_lock", .debuglevel=2};

volatile unsigned char *MSRBitmap;
volatile unsigned char *IOBitmap;

volatile unsigned char *VMREADBitmap;
volatile unsigned char *VMWRITEBitmap;


int hasEPTsupport=0;
int TSCHooked=0;
int hasNPsupport=1;


int canToggleCR3Exit=0; //intel only flag


#ifdef USENMIFORWAIT
int canExitOnNMI=0;
#endif

int hasMTRRsupport;
MTRRCAP MTRRCapabilities;
MTRRDEF MTRRDefType;

int has_EPT_1GBsupport;
int has_EPT_2MBSupport;
int has_EPT_ExecuteOnlySupport;
int has_EPT_INVEPTSingleContext;
int has_EPT_INVEPTAllContext;

int hasUnrestrictedSupport;
int hasVPIDSupport;
int canToggleCR3Exit;
int hasVMCSShadowingSupport;
int hasCETSupport;

int has_VPID_INVVPIDIndividualAddress;
int has_VPID_INVVPIDSingleContext;
int has_VPID_INVVPIDAllContext;
int has_VPID_INVVPIDSingleContextRetainingGlobals;



//AMD
int has_VGIFSupport;
int has_NP_1GBsupport;
int has_NP_2MBsupport;



extern void realmode_inthooks();
extern void realmode_inthooks_end();

extern void realmode_inthook_new12();
extern void realmode_inthook_new15();

void realmode_inthook_calladdress();

extern DWORD realmode_inthook_original12;
extern DWORD realmode_inthook_original15;
extern WORD realmode_inthook_conventional_memsize;



void setupVMX_AMD(pcpuinfo currentcpuinfo)
{
  UINT64 eax, ebx, ecx,edx; //cpuid values

  //setup the vmcb
  Segment_Attribs reg_csaccessrights;
  Segment_Attribs reg_traccessrights UNUSED;

  if (currentcpuinfo->cpunr!=0)
  {
    sendstringf("setupVMX_AMD for AP cpu\n");
  }

#ifdef AMDNP
  //nested paging, works. But using it for memory cloak is not as fast as on Intel (at best like stealthedit plugin on windows, which can be unstable)
  currentcpuinfo->vmcb->NP_ENABLE=1;
  currentcpuinfo->vmcb->G_PAT=readMSR(0x277);
  //setup a pagebase describing the physical memory

  volatile PPML4 pml4=(PPML4)malloc(4096);
  zeromemory((volatile void*)pml4,4096);


  currentcpuinfo->vmcb->N_CR3=VirtualToPhysical((void*)pml4);
  sendstringf("Setup nCR3 at %6\n", currentcpuinfo->vmcb->N_CR3);

  has_NP_1GBsupport=1;
  has_NP_2MBsupport=1;

  //copy the PML4 table and set a new CR3  (PML4 should never change after this, at least not on a global level)
  PPML4 newpml4=(PPML4)malloc(4096);
  copymem(newpml4, pml4table, 4096);

  sendstringf("Created new PML4 table at %6 (PA %6)\n", newpml4, VirtualToPhysical((void*)newpml4));


  *(QWORD*)(&newpml4[510])=currentcpuinfo->vmcb->N_CR3;
  newpml4[510].P=1;
  newpml4[510].RW=1;

  *(QWORD*)(&newpml4[511])=VirtualToPhysical((void*)newpml4); //point to this one
  newpml4[511].P=1;
  newpml4[511].RW=1;

  asm volatile ("": : :"memory");

  setCR3(VirtualToPhysical((void*)newpml4));

  sendstringf("Set CR3 to %6 . It is now %6\n", VirtualToPhysical((void*)newpml4), getCR3() );

  _invlpg(0xffffff0000000000ULL);

#endif

  currentcpuinfo->vmcb_pending[0]=0;
  currentcpuinfo->vmcb_pending[1]=0;
  currentcpuinfo->vmcb_pending[2]=0;
  currentcpuinfo->vmcb_pending[3]=0;
  currentcpuinfo->vmcb_pending[4]=0;
  currentcpuinfo->vmcb_pending[5]=0;
  currentcpuinfo->vmcb_pending[6]=0;
  currentcpuinfo->vmcb_pending[7]=0;
  currentcpuinfo->vmcb_pending[8]=0;
  currentcpuinfo->vmcb_pending[9]=0;
  currentcpuinfo->vmcb_pending[10]=0;
  currentcpuinfo->vmcb_pending[11]=0;
  currentcpuinfo->vmcb_pending[12]=0;
  currentcpuinfo->vmcb_pending[13]=0;
  currentcpuinfo->vmcb_pending[14]=0;
  currentcpuinfo->vmcb_pending[15]=0;


  currentcpuinfo->vmcb_GIF=1;
  currentcpuinfo->vmcb->InterceptVMRUN=1;


  //check if it can virtualize vmload/vmsave/GIF:
  eax=0x8000000a;
  _cpuid(&eax, &ebx, &ecx,&edx);

  /*
  //VMLOAD/VMSAVE (After testing, this can be disabled)
  if (edx & (1<<15))
  {
    sendstringf("Supports Virtualized VMSAVE and VMLOAD\n");
    currentcpuinfo->vmcb->VirtualizedVMSAVEandVMLOAD=1;
  }
  else
  {
    currentcpuinfo->vmcb->InterceptVMLOAD=1;
    currentcpuinfo->vmcb->InterceptVMSAVE=1;
  }
  */

  if (edx & (1<<16)) //virtualize GIF
  {
    sendstringf("Supports Virtualized GIF\n");
    has_VGIFSupport=1;
    currentcpuinfo->vmcb->V_GIF=1;
    currentcpuinfo->vmcb->V_GIF_ENABLED=1;
  }
  else
  {
    currentcpuinfo->vmcb->InterceptCLGI=1;
    currentcpuinfo->vmcb->InterceptSTGI=1;
  }

 // currentcpuinfo->vmcb->InterceptINVLPGA=1;

  //currentcpuinfo->vmcb->InterceptHLT=1;



  currentcpuinfo->vmcb->GuestASID=1;//+(_rdtsc()% (ebx-1));  //1

  currentcpuinfo->vmcb->EFER=0x1500 | (1<<8) | (1<<10);

  reg_traccessrights.SegmentAttrib=0;
  reg_traccessrights.Segment_type=11; //11=32-bit 3=16-bit
  reg_traccessrights.S=0;
  reg_traccessrights.DPL=0;
  reg_traccessrights.P=1;
  reg_traccessrights.G=0;
  reg_traccessrights.D_B=1;

  reg_csaccessrights.SegmentAttrib=0;
  reg_csaccessrights.Segment_type=11;
  reg_csaccessrights.S=1;
  reg_csaccessrights.DPL=0;
  reg_csaccessrights.P=1;
  reg_csaccessrights.L=1;
  reg_csaccessrights.G=0;
  reg_csaccessrights.D_B=0;

  currentcpuinfo->vmcb->CR4=getCR4();
  currentcpuinfo->vmcb->CR3=getCR3();
  currentcpuinfo->vmcb->CR0=getCR0();


  currentcpuinfo->guestCR3=getCR3();
  currentcpuinfo->guestCR0=getCR0();
  currentcpuinfo->hasIF=0;

  currentcpuinfo->vmcb->gdtr_base=getGDTbase();
  currentcpuinfo->vmcb->idtr_base=getIDTbase();

  currentcpuinfo->vmcb->gdtr_limit=0x58;
  currentcpuinfo->vmcb->idtr_limit=8*256;

  currentcpuinfo->vmcb->cs_selector=80;
  //currentcpuinfo->vmcb->cs_limit=0;//0xffffffff;
  //currentcpuinfo->vmcb->ss_limit=0;//0xffffffff;
  currentcpuinfo->vmcb->cs_attrib=(WORD)reg_csaccessrights.SegmentAttrib;

  currentcpuinfo->vmcb->ds_selector=8;
  currentcpuinfo->vmcb->es_selector=8;
  currentcpuinfo->vmcb->ss_selector=8;
  currentcpuinfo->vmcb->fs_selector=8;
  currentcpuinfo->vmcb->gs_selector=8;
  currentcpuinfo->vmcb->ldtr_selector=0;
  currentcpuinfo->vmcb->tr_selector=64;

  sendstringf("cs_attrib(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->cs_attrib-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->cs_attrib);
  sendstringf("gdtr_limit(%x)  set to %x\n", ((UINT64)&currentcpuinfo->vmcb->gdtr_limit-(UINT64)currentcpuinfo->vmcb), currentcpuinfo->vmcb->gdtr_limit);




/*
  currentcpuinfo->vmcb->tr_limit=(UINT64)sizeof(TSS)+32+8192+1;
  currentcpuinfo->vmcb->tr_base=(UINT64)mainTSS;
  currentcpuinfo->vmcb->tr_attrib=(WORD)reg_traccessrights.SegmentAttrib;*/

  currentcpuinfo->vmcb->DR7=0x400;

  currentcpuinfo->vmcb->RSP=((UINT64)malloc(4096))+0x1000-0x28;
  currentcpuinfo->vmcb->RFLAGS=getRFLAGS();

  if (currentcpuinfo->cpunr==0)
    currentcpuinfo->vmcb->RIP=(UINT64)reboot;
  else
    currentcpuinfo->vmcb->RIP=(UINT64)apentryvmx;


  if (!loadedOS)
    currentcpuinfo->vmcb->InterceptINT=1; //break on software interrupts (int 0x15 in realmode to tell the os not to mess with stuff)

  currentcpuinfo->vmcb->InterceptShutdown=1; //in case of a severe error
  currentcpuinfo->vmcb->InterceptVMMCALL=1;
  currentcpuinfo->vmcb->MSR_PROT=1; //some msr's need to be protected

  currentcpuinfo->vmcb->InterceptExceptions=(1<<1) | (1<<3);// | (1<<14); //intercept int1, 3 and 14

 // currentcpuinfo->vmcb->InterceptINTR=1;
 // currentcpuinfo->vmcb->InterceptDR0_15Write=(1<<6); //dr6 so I can see what changed



  /*
   if (currentcpuinfo->cpunr)
   {
     currentcpuinfo->vmcb->InterceptINIT=1; //cpu init (init-sipi-sipi. I need to implement a virtual apic to suppot boot
     currentcpuinfo->vmcb->InterceptCPUID=1;
     currentcpuinfo->vmcb->InterceptExceptions=0xffffffff;
     currentcpuinfo->vmcb->InstructionIntercept2=0xffffffff;

     setCR8(15);
   }*/


  if (MSRBitmap==NULL)
  {
    int i;
    //allocate a MSR bitmap
    MSRBitmap=allocateContiguousMemory(2); //

    if (MSRBitmap==NULL)
    {
      sendstringf("allocateContiguousMemory failed. MSRBitmap=NULL\n");
      while(1);
    }
    //fill with 1's (the msr's that have a 1 do not cause an intercept)

    //bochsbp();
    for (i=0; i<4096*2; i++)
      MSRBitmap[i]=0;

    //Must protect 0xc0010117 (MSRPM_BASE_PA)
    MSRBitmap[0x1000+(0x0117*2)/8]|=3 << ((0x0117*2) % 8);

    MSRBitmap[0x1000+(0x0115*2)/8]|=3 << ((0x0115*2) % 8);

    //also 0xc0000080 (EFER)
    //if (hideEFER)
    MSRBitmap[0x800+(0x80*2)/8]|=3 << ((0x80*2) % 8);
  }
  currentcpuinfo->vmcb->MSRPM_BASE_PA=VirtualToPhysical((void *)MSRBitmap);

  currentcpuinfo->guest_VM_HSAVE_PA=0;


  globals_have_been_configured=1;

  SaveExtraHostState(currentcpuinfo->vmcb_PA); //save some of MSR's that are needed (init did touch the segment registers so those need to be overridden if loadedOS)


  if (loadedOS)
  {
    POriginalState originalstate=(POriginalState)mapPhysicalMemory(loadedOS, sizeof(OriginalState));;
    PGDT_ENTRY gdt=NULL,ldt=NULL;
    ULONG ldtselector=originalstate->ldt;
    int notpaged;


    sendstringf("Setting up guest based on loadedOS settings\n");


    sendstringf("originalstate=%6\n", originalstate);
    sendstringf("originalstate->cpucount(%x)=%d\n",&originalstate->cpucount, originalstate->cpucount);
    sendstringf("originalstate->cr0=%6\n",originalstate->cr0);
    sendstringf("originalstate->cr2=%6\n",originalstate->cr2);
    sendstringf("originalstate->cr3=%6\n",originalstate->cr3);
    sendstringf("originalstate->cr4=%6\n",originalstate->cr4);
    sendstringf("originalstate->rip(%x)=%6\n",&origina