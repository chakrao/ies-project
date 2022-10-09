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
    sendstringf("originalstate->rip(%x)=%6\n",&originalstate->rip, originalstate->rip);
    sendstringf("originalstate->cs=%x\n",originalstate->cs);
    sendstringf("originalstate->ss=%x\n",originalstate->ss);
    sendstringf("originalstate->ds=%x\n",originalstate->ds);
    sendstringf("originalstate->es=%x\n",originalstate->es);
    sendstringf("originalstate->fs=%x\n",originalstate->fs);
    sendstringf("originalstate->gs=%x\n",originalstate->gs);
    sendstringf("originalstate->ldt=%x\n",originalstate->ldt);
    sendstringf("originalstate->tr=%x\n",originalstate->tr);

    sendstringf("originalstate->dr7(%x)=%6\n",&originalstate->dr7, originalstate->dr7);
    sendstringf("originalstate->gdtbase=%6\n",originalstate->gdtbase);
    sendstringf("originalstate->gdtlimit=%x\n",originalstate->gdtlimit);
    sendstringf("originalstate->idtbase=%6\n",originalstate->idtbase);
    sendstringf("originalstate->idtlimit=%x\n",originalstate->idtlimit);
    sendstringf("originalstate->originalLME=%x\n",originalstate->originalLME);
    sendstringf("originalstate->rflags=%6\n",originalstate->rflags);

    sendstringf("originalstate->rax=%6\n",originalstate->rax);
    sendstringf("originalstate->rbx=%6\n",originalstate->rbx);
    sendstringf("originalstate->rcx=%6\n",originalstate->rcx);
    sendstringf("originalstate->rdx=%6\n",originalstate->rdx);
    sendstringf("originalstate->rsi=%6\n",originalstate->rsi);
    sendstringf("originalstate->rdi=%6\n",originalstate->rdi);
    sendstringf("originalstate->rbp=%6\n",originalstate->rbp);
    sendstringf("originalstate->rsp=%6\n",originalstate->rsp);
    sendstringf("originalstate->r8=%6\n",originalstate->r8);
    sendstringf("originalstate->r9=%6\n",originalstate->r9);
    sendstringf("originalstate->r10=%6\n",originalstate->r10);
    sendstringf("originalstate->r11=%6\n",originalstate->r11);
    sendstringf("originalstate->r12=%6\n",originalstate->r12);
    sendstringf("originalstate->r13=%6\n",originalstate->r13);
    sendstringf("originalstate->r14=%6\n",originalstate->r14);
    sendstringf("originalstate->r15=%6\n",originalstate->r15);

    sendstringf("originalstate->fsbase=%6\n",originalstate->fsbase);
    sendstringf("originalstate->gsbase=%6\n",originalstate->gsbase);

    currentcpuinfo->vmcb->CR4=originalstate->cr4;
    currentcpuinfo->vmcb->CR3=originalstate->cr3;
    currentcpuinfo->vmcb->CR0=originalstate->cr0;
    currentcpuinfo->vmcb->RFLAGS=originalstate->rflags;

    currentcpuinfo->efer=originalstate->originalEFER;
    currentcpuinfo->vmcb->EFER=currentcpuinfo->efer | (1<<12);


    currentcpuinfo->vmcb->gdtr_base=(UINT64)originalstate->gdtbase;
    currentcpuinfo->vmcb->gdtr_limit=(UINT64)originalstate->gdtlimit;

    currentcpuinfo->vmcb->idtr_base=(UINT64)originalstate->idtbase;
    currentcpuinfo->vmcb->idtr_limit=(UINT64)originalstate->idtlimit;


    //gdt MUST be paged in
    gdt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, originalstate->gdtbase, &notpaged), originalstate->gdtlimit);


    ULONG ldtlimit;
    if ((UINT64)originalstate->ldt)
    {
      UINT64 ldtbase; //should be 0 in 64bit


      sendstring("ldt is valid, so getting the information\n\r");

      ldtbase=(gdt[(ldtselector >> 3)].Base24_31 << 24) + gdt[(ldtselector >> 3)].Base0_23;
      ldtlimit=(gdt[(ldtselector >> 3)].Limit16_19 << 16) + gdt[(ldtselector >> 3)].Limit0_15;
      ldt=(PGDT_ENTRY)(UINT64)mapPhysicalMemory(getPhysicalAddressVM(currentcpuinfo, ldtbase, &notpaged), ldtlimit);
    }

    currentcpuinfo->vmcb->es_selector=originalstate->es;
    currentcpuinfo->vmcb->es_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->es_AccessRights);
    currentcpuinfo->vmcb->es_limit=originalstate->es_Limit;

    currentcpuinfo->vmcb->cs_selector=originalstate->cs;
    currentcpuinfo->vmcb->cs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->cs_AccessRights);
    currentcpuinfo->vmcb->cs_limit=originalstate->cs_Limit;

    currentcpuinfo->vmcb->ss_selector=originalstate->ss;
    currentcpuinfo->vmcb->ss_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->ss_AccessRights);
    currentcpuinfo->vmcb->ss_limit=originalstate->ss_Limit;

    currentcpuinfo->vmcb->ds_selector=originalstate->ds;
    currentcpuinfo->vmcb->ds_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->ds_AccessRights);
    currentcpuinfo->vmcb->ds_limit=originalstate->ds_Limit;

    currentcpuinfo->vmcb->fs_selector=originalstate->fs;
    currentcpuinfo->vmcb->fs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->fs_AccessRights);
    currentcpuinfo->vmcb->fs_limit=originalstate->fs_Limit;

    currentcpuinfo->vmcb->gs_selector=originalstate->gs;
    currentcpuinfo->vmcb->gs_attrib=convertSegmentAccessRightsToSegmentAttrib(originalstate->gs_AccessRights);
    currentcpuinfo->vmcb->gs_limit=originalstate->gs_Limit;

    currentcpuinfo->vmcb->ldtr_selector=originalstate->ldt;
    currentcpuinfo->vmcb->tr_selector=originalstate->tr;

    if (originalstate->originalLME)
    {
      //64-bit
      currentcpuinfo->vmcb->cs_base=0;
      currentcpuinfo->vmcb->ss_base=0;
      currentcpuinfo->vmcb->ds_base=0;
      currentcpuinfo->vmcb->es_base=0;
      currentcpuinfo->vmcb->fs_base=originalstate->fsbase;
      currentcpuinfo->vmcb->gs_base=originalstate->gsbase;
      currentcpuinfo->vmcb->tr_base=getSegmentBaseEx(gdt,ldt,originalstate->tr, 1);      
    }
    else
    {
      //32-bit
      currentcpuinfo->vmcb->cs_base=getSegmentBase(gdt, ldt, originalstate->cs);
      currentcpuinfo->vmcb->ss_base=getSegmentBase(gdt, ldt, originalstate->ss);
      currentcpuinfo->vmcb->ds_base=getSegmentBase(gdt, ldt, originalstate->ds);
      currentcpuinfo->vmcb->es_base=getSegmentBase(gdt, ldt, originalstate->es);
      currentcpuinfo->vmcb->fs_base=getSegmentBase(gdt, ldt, originalstate->fs);
      currentcpuinfo->vmcb->gs_base=getSegmentBase(gdt, ldt, originalstate->gs);
      currentcpuinfo->vmcb->tr_base=getSegmentBase(gdt, ldt, originalstate->tr);
    }
    currentcpuinfo->vmcb->ldtr_limit=getSegmentLimit(gdt, ldt, originalstate->ldt);
    if (originalstate->tr==0)
      currentcpuinfo->vmcb->tr_limit=0xffff;
    else
      currentcpuinfo->vmcb->tr_limit=getSegmentLimit(gdt, ldt, originalstate->tr);

    currentcpuinfo->vmcb->ldtr_base=getSegmentBase(gdt, ldt, originalstate->ldt);
    currentcpuinfo->vmcb->ldtr_attrib=getSegmentAttrib(gdt,ldt,originalstate->ldt);

    if (originalstate->tr)
      currentcpuinfo->vmcb->tr_attrib=getSegmentAttrib(gdt,ldt,originalstate->tr);
    else
      currentcpuinfo->vmcb->tr_attrib=0x8b;

    currentcpuinfo->vmcb->SYSENTER_CS=(UINT64)readMSR(IA32_SYSENTER_CS_MSR); //current msr
    currentcpuinfo->vmcb->SYSENTER_ESP=(UINT64)readMSR(IA32_SYSENTER_ESP_MSR);
    currentcpuinfo->vmcb->SYSENTER_EIP=(UINT64)readMSR(IA32_SYSENTER_EIP_MSR);

    currentcpuinfo->actual_sysenter_CS=currentcpuinfo->vmcb->SYSENTER_CS;
    currentcpuinfo->actual_sysenter_ESP=currentcpuinfo->vmcb->SYSENTER_ESP;
    currentcpuinfo->actual_sysenter_EIP=currentcpuinfo->vmcb->SYSENTER_EIP;

    currentcpuinfo->vmcb->DR7=originalstate->dr7;

    if (originalstate->originalLME)
    {
      //64-bit
      currentcpuinfo->vmcb->RSP=originalstate->rsp;
      currentcpuinfo->vmcb->RAX=originalstate->rax;
      currentcpuinfo->vmcb->RIP=originalstate->rip;

    }
    else
    {
      //32-bit (make sure unused bits are 0)
      currentcpuinfo->vmcb->RSP=(ULONG)originalstate->rsp;
      currentcpuinfo->vmcb->RAX=(ULONG)originalstate->rax;
      currentcpuinfo->vmcb->RIP=(ULONG)originalstate->rip;
    }



    if (gdt)
      unmapPhysicalMemory(gdt, originalstate->gdtlimit);

    if (ldt)
      unmapPhysicalMemory(ldt, ldtlimit);

    if (originalstate)
      unmapPhysicalMemory(originalstate, sizeof(OriginalState));
  }
  else
  {
    //no loadedinfo
    /*

    if (currentcpuinfo->cpunr)
    {
      UINT64 a,b,c,d;


      currentcpuinfo->vmcb->CR0=0x10;
      currentcpuinfo->vmcb->CR2=0;
      currentcpuinfo->vmcb->CR3=0;
      currentcpuinfo->vmcb->RFLAGS=2;
      currentcpuinfo->vmcb->EFER=0x1000;
      currentcpuinfo->vmcb->RIP=0;


      Segment_Attribs attrib;
      attrib.G=0;
      attrib.D_B=0;
      attrib.L=0;
      attrib.P=1;
      attrib.DPL=0;

      //cs:
      attrib.S=1;
      attrib.Segment_type=0b1010;
      currentcpuinfo->vmcb->cs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->cs_selector=0x1000;
      currentcpuinfo->vmcb->cs_base=0x10000;
      currentcpuinfo->vmcb->cs_limit=0xffff;
      {
      PPDPTE_PAE pml4entry;
      PPDPTE_PAE pagedirpointerentry;
      PPDE_PAE pagedirentry;
      PPTE_PAE pagetableentry;

      VirtualAddressToPageEntries(0, &pml4entry, &pagedirpointerentry, &pagedirentry, &pagetableentry);
      pagedirentry[0].RW=1;
      pagedirentry[1].RW=1;
      asm volatile ("": : :"memory");
    }
      *(unsigned char *)0x10000=0xf4; //hlt

      //data
      attrib.S=1;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ss_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ss_selector=0;
      currentcpuinfo->vmcb->ss_base=0;
      currentcpuinfo->vmcb->ss_limit=0xffff;

      currentcpuinfo->vmcb->ds_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ds_selector=0;
      currentcpuinfo->vmcb->ds_base=0;
      currentcpuinfo->vmcb->ds_limit=0xffff;

      currentcpuinfo->vmcb->es_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->es_selector=0;
      currentcpuinfo->vmcb->es_base=0;
      currentcpuinfo->vmcb->es_limit=0xffff;

      currentcpuinfo->vmcb->fs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->fs_selector=0;
      currentcpuinfo->vmcb->fs_base=0;
      currentcpuinfo->vmcb->fs_limit=0xffff;

      currentcpuinfo->vmcb->gs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->gs_selector=0;
      currentcpuinfo->vmcb->gs_base=0;
      currentcpuinfo->vmcb->gs_limit=0xffff;

      //ldtr:
      attrib.S=0;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ldtr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ldtr_selector=0;
      currentcpuinfo->vmcb->ldtr_base=0;
      currentcpuinfo->vmcb->ldtr_limit=0xffff;

      //tr:
      attrib.S=0;
      attrib.Segment_type=0b0011;
      currentcpuinfo->vmcb->tr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->tr_selector=0;
      currentcpuinfo->vmcb->tr_base=0;
      currentcpuinfo->vmcb->tr_limit=0xffff;


      currentcpuinfo->vmcb->gdtr_base=0;
      currentcpuinfo->vmcb->gdtr_limit=0xffff;

      currentcpuinfo->vmcb->idtr_base=0;
      currentcpuinfo->vmcb->idtr_limit=0xffff;


      a=1;
      _cpuid(&a,&b,&c,&d);

      currentcpuinfo->vmcb->RAX=0;
      currentcpuinfo->vmcb->RSP=0;


      setDR0(0);
      setDR1(0);
      setDR2(0);
      setDR3(0);

      currentcpuinfo->vmcb->DR6=0xffff0ff0;
      currentcpuinfo->vmcb->DR7=0x400;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
    }*/
  }





  sendstringf("Configured cpu %d\n", currentcpuinfo->cpunr);
  currentcpuinfo->vmxsetup=1;
  csLeave(&setupVMX_lock);

}


int vmx_enableProcBasedFeature(DWORD PBF)
{
  if (((IA32_VMX_PROCBASED_CTLS >> 32) & PBF) == PBF) //can it be set
  {
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | PBF);
    return 1;
  }
  else
    return 0;
}

int vmx_disableProcBasedFeature(DWORD PBF)
{
  QWORD PBCTLS=IA32_VMX_PROCBASED_CTLS;

  if (IA32_VMX_BASIC.default1canbe0)
    PBCTLS=readMSR(IA32_VMX_TRUE_PROCBASED_CTLS_MSR);


  if (PBCTLS & PBF) //can it be set to 0
    return 0; //nope
  else
  {
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) & (~PBF));
    return 1;
  }
}

int vmx_enablePinBasedFeature(DWORD PINBF)
{
  if (((IA32_VMX_PINBASED_CTLS >> 32) & PINBF) == PINBF) //can it be set
  {
    vmwrite(vm_execution_controls_pin, vmread(vm_execution_controls_pin) | PINBF);
    return 1;
  }
  else
    return 0;
}

int vmx_disablePinBasedFeature(DWORD PINBF)
{
  QWORD PINBCTLS=IA32_VMX_PINBASED_CTLS;

  if (IA32_VMX_BASIC.default1canbe0)
    PINBCTLS=readMSR(IA32_VMX_TRUE_PINBASED_CTLS_MSR);


  if (PINBCTLS & PINBF) //can it be set to 0
    return 0; //nope
  else
  {
    vmwrite(vm_execution_controls_pin, vmread(vm_execution_controls_pin) & (~PINBF));
    return 1;
  }
}

int vmx_enableNMIWindowExiting(void)
{
  if (vmx_enableProcBasedFeature(PBEF_NMI_WINDOW_EXITING))
  {
    //PBEF_NMI_WINDOW_EXITING can be set
    //"If the “virtual NMIs” VM-execution control is 0, the “NMI-window exiting” VM-execution control must be 0."
    //so virtual NMIs must be 1 as well
    if (vmx_enablePinBasedFeature(PINBEF_VIRTUAL_NMIS))
    {
      //"If the “NMI exiting” VM-execution control is 0, the “virtual NMIs” VM-execution control must be 0"
      //so also enable NMI exiting
      if (vmx_enablePinBasedFeature(PINBEF_NMI_EXITING))
        return 1;

      vmx_disablePinBasedFeature(PINBEF_VIRTUAL_NMIS);
    }

    vmx_disableProcBasedFeature(PBEF_NMI_WINDOW_EXITING);
  }

  return 0;
}

int vmx_disableNMIWindowExiting(void)
{
  int a,b,c;
  a=vmx_disableProcBasedFeature(PBEF_NMI_WINDOW_EXITING);
  b=vmx_disablePinBasedFeature(PINBEF_VIRTUAL_NMIS);
  c=vmx_disablePinBasedFeature(PINBEF_NMI_EXITING);

  return (a+b+c==3);
}

int vmx_addSingleSteppingReasonEx(pcpuinfo currentcpuinfo, int reason, void *data)
{
  if (currentcpuinfo->singleStepping.ReasonsPos>=currentcpuinfo->singleStepping.ReasonsLength) //realloc
  {
    currentcpuinfo->singleStepping.ReasonsLength=(currentcpuinfo->singleStepping.ReasonsLength+2)*2;
    currentcpuinfo->singleStepping.Reasons=realloc(currentcpuinfo->singleStepping.Reasons, currentcpuinfo->singleStepping.ReasonsLength * sizeof(SingleStepReason));
  }

  //always add to the end
  if (currentcpuinfo->singleStepping.ReasonsPos>=1)
  {
    sendstringf("Multiple single stepping reasons\n");
  }

  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Reason=reason;
  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Data=data;
  currentcpuinfo->singleStepping.ReasonsPos++;

  return currentcpuinfo->singleStepping.ReasonsPos-1;

}

int vmx_addSingleSteppingReason(pcpuinfo currentcpuinfo, int reason, int ID)
{
  if (currentcpuinfo->singleStepping.ReasonsPos>=currentcpuinfo->singleStepping.ReasonsLength) //realloc
  {
    currentcpuinfo->singleStepping.ReasonsLength=(currentcpuinfo->singleStepping.ReasonsLength+2)*2;
    currentcpuinfo->singleStepping.Reasons=realloc(currentcpuinfo->singleStepping.Reasons, currentcpuinfo->singleStepping.ReasonsLength * sizeof(SingleStepReason));
  }

  //always add to the end
  if (currentcpuinfo->singleStepping.ReasonsPos>=1)
  {
    sendstringf("Multiple single stepping reasons\n");
  }

  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].Reason=reason;
  currentcpuinfo->singleStepping.Reasons[currentcpuinfo->singleStepping.ReasonsPos].ID=ID;
  currentcpuinfo->singleStepping.ReasonsPos++;

  return currentcpuinfo->singleStepping.ReasonsPos-1;
}

int vmx_enableSingleStepMode(void)
{
  pcpuinfo c=getcpuinfo();
  //sendstringf("%d Enabling single step mode\n", c->cpunr);


  if (isAMD)
  {
   // sendstringf("%d CS:RIP=%x:%6 RCX=%d\n", c->cpunr, c->vmcb->cs_selector, c->vmcb->RIP);

    //break on external interrupts and exceptions
    c->vmcb->InterceptVINTR=1;
    c->vmcb->InterceptINTR=1;
    c->vmcb->InterceptExceptions=0x0000ffff;

    //perhaps enable INTERRUPT_SHADOW?

    //mark the intercepts as changed
    //sendstringf("b c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);
    c->vmcb->VMCB_CLEAN_BITS&=~(1<<0);
    c->vmcb->VMCB_CLEAN_BITS=0;
   //sendstringf("a c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);

    RFLAGS v;
    v.value=c->vmcb->RFLAGS;

    if (c->singleStepping.ReasonsPos==0) //first one
      c->singleStepping.PreviousTFState=v.TF;

    v.TF=1; //single step mode
    v.RF=1;
    if (v.IF)
      c->vmcb->INTERRUPT_SHADOW=1;

    //todo: intercept pushf/popf/iret and the original RF flag state (though for a single step that should have no effect

    c->vmcb->RFLAGS=v.value;
    c->singleStepping.Method=3; //Trap flag

    //turn of syscall, and when syscall is executed, capture the UD, re-enable it, but change the flags mask to keep the TF enabled, and the step after that adjust R11 so that the TF is gone and restore the flags mask.  Then continue as usual;
    if (c->singleStepping.ReasonsPos==0)
    {
      c->singleStepping.PreviousEFER=c->vmcb->EFER;
      c->singleStepping.PreviousFMASK=c->vmcb->SFMASK;
      c->singleStepping.LastInstructionWasSyscall=0;

      c->vmcb->EFER&=0xfffffffffffffffeULL;
      c->vmcb->VMCB_CLEAN_BITS&=~(1<< 5); //efer got changed
    }


    return 1;

  }
  else
  {
    sendstring("\n");

   /* if ((vmread(vm_entry_interruptioninfo) >> 31)==0)
      vmwrite(vm_guest_interruptability_state,2); //execute at least one instruction
    else
      sendstringf("Not setting the interruptability state\n");*/

    if (vmx_enableProcBasedFeature(PBEF_MONITOR_TRAP_FLAG))
    {
      sendstring("Using the monitor trap flag\n");
      c->singleStepping.Method=1;
      return 1;
    }
    else
    {
      c->singleStepping.Method=2;
      if (vmx_enableProcBasedFeature(PBEF_INTERRUPT_WINDOW_EXITING))
      {
        if ((vmread(vm_entry_interruptioninfo) >> 31)==0) //if no interrupt pending
          vmwrite(vm_guest_interruptability_state,2); //execute at least one instruction
        //else go to that interrupt that is pending and then stop

        sendstring("Using the interrupt window\n");
        return 1;
      }
    }
  }

  return 0;
}

int vmx_disableSingleStepMode(void)
{
  int r=0;
  pcpuinfo c=getcpuinfo();

  sendstringf("%d Disabling single step mode\n", c->cpunr);

  if (isAMD)
  {
    //shouldn't be needed but do it anyhow

    sendstringf("%d RFLAGS was %x\n", c->cpunr, c->vmcb->RFLAGS);


    RFLAGS v;
    v.value=c->vmcb->RFLAGS;
    v.TF=c->singleStepping.PreviousTFState;  // 0; //single step mode

    c->vmcb->RFLAGS=v.value;
    sendstringf("%d RFLAGS is %x\n", c->cpunr, c->vmcb->RFLAGS);



    c->singleStepping.Method=0;

    c->vmcb->InterceptVINTR=0;
    c->vmcb->InterceptINTR=0;
    c->vmcb->InterceptExceptions=(1<<1) | (1<<3); // todo: load current exceptions hooks


    //mark the intercepts as changed
    sendstringf("b c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);
    c->vmcb->VMCB_CLEAN_BITS&=~(1<<0);
    c->vmcb->VMCB_CLEAN_BITS=0;
    sendstringf("a c->vmcb->VMCB_CLEAN_BITS=%6\n",c->vmcb->VMCB_CLEAN_BITS);

    c->vmcb->EFER=c->singleStepping.PreviousEFER;
 