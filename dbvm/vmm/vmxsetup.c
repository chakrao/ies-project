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

  reg_traccessrights.SegmentAttr