/*
 * epthandler.c
 *
 *  Created on: Feb 2, 2018
 *      Author: erich
 */

#define MAXCLOAKLISTBEFORETRANFERTOMAP 40

//#define MEMORYCHECK

#include "epthandler.h"
#include "main.h"
#include "mm.h"
#include "vmxsetup.h"
#include "msrnames.h"
#include "common.h"
#include "vmpaging.h"
#include "vmcall.h"
#include "maps.h"
#include "list.h"
#include "vmeventhandler.h"
#include "displaydebug.h"
#include "nphandler.h"

void recordState(void *liststart, int datatype, int currentEntryNr, pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
void fillPageEventBasic(PageEventBasic *peb, VMRegisters *registers);

EPTWatchLogData lastSeenEPTWatch; //debugging ept
EPTWatchLogData lastSeenEPTWatchVerySure;


QWORD EPTMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage);

criticalSection eptWatchListCS={.name="eptWatchListCS", .debuglevel=2};
PEPTWatchEntry eptWatchList;
int eptWatchListSize;
int eptWatchListPos;


criticalSection CloakedPagesCS={.name="CloakedPagesCS", .debuglevel=2}; //1
PAddressList CloakedPagesList; //up to 40 entries can be found in 5 steps (worst case scenario)
PMapInfo CloakedPagesMap; //can be found in 5 steps, always (and eats memory) , so if CloakedPagesPos>40 then start using this (and move the old list over)
//todo: Create a MapList object that combines both into one




criticalSection ChangeRegBPListCS={.name="ChangeRegBPListCS", .debuglevel=2}; //2
ChangeRegBPEntry *ChangeRegBPList;
int ChangeRegBPListSize;
int ChangeRegBPListPos;

TraceOnBPEntry *TraceOnBP; //not NULL when active. (There can be only one active one at a time globally, as the trap flag can switch between cpu's)


criticalSection BrokenThreadListCS={.name="BrokenThreadListCS", .debuglevel=2}; //2
BrokenThreadEntry *BrokenThreadList;
int BrokenThreadListSize;
int BrokenThreadListPos;


#ifdef MEMORYCHECK
int checkmem(unsigned char *x, int len)
{
  int i;
  for (i=0; i<len; i++)
    if (x[i]!=0xce)
      return 1;

  return 0;

}
#endif


//#define EPTINTEGRITY

#ifdef EPTINTEGRITY
void checkpage(PEPT_PTE e)
{
  int i;
  for (i=0; i<512; i++)
  {
    if (e[i].ignored2 || e[i].ignored3 || e[i].reserved2 )
    {
      while (1) ;
    }
  }

  //and check that the PA isn't inside the BrokenThreadList[] array
}

#endif

void vpid_invalidate()
{
  INVVPIDDESCRIPTOR vpidd;
  vpidd.zero=0;
  vpidd.LinearAddress=0;
  vpidd.VPID=1;

  _invvpid(2, &vpidd);
}

void ept_invalidate()
{
  if (isAMD)
  {
    pcpuinfo c=getcpuinfo();
    c->vmcb->VMCB_CLEAN_BITS&=~(1 << 4);
    c->vmcb->TLB_CONTROL=1;


  }
  else
  {
    //Intel
    INVEPTDESCRIPTOR eptd;
    eptd.Zero=0;
    eptd.EPTPointer=getcpuinfo()->EPTPML4;

    if (has_EPT_INVEPTAllContext)
    {
      _invept(2, &eptd);
    }
    else
    if (has_EPT_INVEPTSingleContext)
    {
      _invept(1, &eptd);
    }
    else
    {
      _invept(2, &eptd);//fuck it
    }
  }

	//vpid_invalidate();
}


int shownfirst=0;
void ept_hideDBVMPhysicalAddresses_callbackIntel(QWORD VirtualAddress UNUSED, QWORD PhysicalAddress, int size UNUSED, PPTE_PAE entry UNUSED, pcpuinfo currentcpuinfo)
{

  if (shownfirst==0)
  {
    sendstringf("%6", PhysicalAddress);
    shownfirst=1;
  }


  QWORD eptentryAddress=EPTMapPhysicalMemory(currentcpuinfo,PhysicalAddress,1);
  PEPT_PTE eptentry=mapPhysicalMemory(eptentryAddress,8);

  eptentry->PFN=MAXPHYADDRMASK >> 13; //unallocated memory (using 13 as sometimes accessing the most significant bit of the allowed PA will crash a system)
  eptentry->MEMTYPE = 0;

  unmapPhysicalMemory(eptentry,8);

  ept_invalidate();
  currentcpuinfo->eptUpdated=1;
}

void ept_hideDBVMPhysicalAddresses_callbackAMD(QWORD VirtualAddress UNUSED, QWORD PhysicalAddress, int size UNUSED, PPTE_PAE entry UNUSED, pcpuinfo currentcpuinfo)
{
  QWORD npentryAddress=NPMapPhysicalMemory(currentcpuinfo,PhysicalAddress,1);
  PPTE_PAE npentry=mapPhysicalMemory(npentryAddress,8);
  npentry->PFN=MAXPHYADDRMASK >> 13;

  unmapPhysicalMemory((void *)npentry,8);

  ept_invalidate();
}


void ept_hideDBVMPhysicalAddresses(pcpuinfo currentcpuinfo)
{
  shownfirst=0;

  nosendchar[getAPICID()]=0;
  sendstringf("  ept_hideDBVMPhysicalAddresses()\n");
  MMENUMPAGESCALLBACK callback=isAMD?(MMENUMPAGESCALLBACK)ept_hideDBVMPhysicalAddresses_callbackAMD:(MMENUMPAGESCALLBACK)ept_hideDBVMPhysicalAddresses_callbackIntel;
  csEnter(&currentcpuinfo->EPTPML4CS);
  sendstringf("    Calling mmEnumAllPageEntries\n");
  mmEnumAllPageEntries(callback, 1, (void*)currentcpuinfo);
  sendstringf("    Returned from mmEnumAllPageEntries\n");
  csLeave(&currentcpuinfo->EPTPML4CS);
}

void ept_hideDBVMPhysicalAddressesAllCPUs()
//walk the dbvm pagetables and map each physical address found to a random address until VA BASE_VIRTUAL_ADDRESS+4096*PhysicalPageListSize;
//todo: If for some reason this takes too long and triggers a timeout, switch to per cpu
{

  nosendchar[getAPICID()]=0;
  sendstringf("ept_hideDBVMPhysicalAddressesAllCPUs()\n");

  pcpuinfo c=firstcpuinfo;

  while (c)
  {
    sendstringf("cpu %d:\n", c->cpunr);

    ept_hideDBVMPhysicalAddresses(c);
    c=c->next;
  }

  sendstringf("done\n");
}


void ept_reset_cb(QWORD address, void *data UNUSED)
{
  map_setEntry(CloakedPagesMap, address, NULL);
}

void ept_reset()
/*
 * Removes all watches/breakpoints
 */
{
  int i;
  csEnter(&eptWatchListCS);
  for (i=0; i<eptWatchListPos; i++)
    if (eptWatchList[i].Active)
      ept_watch_deactivate(i);

  csLeave(&eptWatchListCS);

  csEnter(&ChangeRegBPListCS);
  for (i=0; i<ChangeRegBPListPos; i++)
    if (ChangeRegBPList[i].Active)
      ept_cloak_removechangeregonbp(ChangeRegBPList[i].PhysicalAddress);

  csLeave(&ChangeRegBPListCS);

  csEnter(&CloakedPagesCS);

  if (CloakedPagesMap)
    map_foreach(CloakedPagesMap,ept_reset_cb);
  else
  if (CloakedPagesList)
  {
    while (CloakedPagesList->size)
      ept_cloak_deactivate(CloakedPagesList->list[0].address);
  }

  csLeave(&CloakedPagesCS);

}

BOOL ept_handleCloakEvent(pcpuinfo currentcpuinfo, QWORD Address, QWORD AddressVA)
/*
 * Checks if the physical address is cloaked, if so handle it and return 1, else return 0
 */
{
  int result=0;
  QWORD BaseAddress=Address & MAXPHYADDRMASKPB;
  PCloakedPageData cloakdata;

  if ((CloakedPagesList==NULL) && (CloakedPagesMap==NULL))
    return FALSE;

  if (isAMD) //AMD marks the page as no-execute only, no read/write block
  {
    NP_VIOLATION_INFO nvi;
    nvi.ErrorCode=currentcpuinfo->vmcb->EXITINFO1;
    if (nvi.ID==0)
      return FALSE; //not an execute pagefault. Not a cloak for AMD

    if (currentcpuinfo->NP_Cloak.ActiveRegion)
    {

      cloakdata=currentcpuinfo->NP_Cloak.ActiveRegion;
     // sendstringf("Inside a cloaked region using mode 1 (which started in %6) and an execute fault happened (CS:RIP=%x:%6)\n", currentcpuinfo->NP_Cloak.LastCloakedVirtualBase, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);

      //means we exited the cloaked page (or at the page boundary)

      csEnter(&CloakedPagesCS);
      NPMode1CloakSetState(currentcpuinfo, 0); //marks all pages back as executable and the stealthed page(s) back as no execute
      csLeave(&CloakedPagesCS);

      //check if it's a page boundary
     // QWORD currentexecbase=currentcpuinfo->vmcb->RIP & 0xffffffffffff000ULL;

      if  (((currentcpuinfo->vmcb->RIP<currentcpuinfo->NP_Cloak.LastCloakedVirtualBase) &&
          ((currentcpuinfo->vmcb->RIP+32)>=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase))
        ||
        ((currentcpuinfo->vmcb->RIP>=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase+4096) &&
         (currentcpuinfo->vmcb->RIP<=currentcpuinfo->NP_Cloak.LastCloakedVirtualBase+4096+32)))
      {
        sendstringf("Pageboundary. Do a single step with the cloaked page decloaked\n");

        //page boundary. Do a single step with the cloaked page executable
        *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
        cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->US=1;
        cloakdata->npentry[currentcpuinfo->cpunr]->EXB=0;

        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);
      }

      currentcpuinfo->NP_Cloak.ActiveRegion=NULL;
      ept_invalidate();


      return TRUE;
    }
  }


  csEnter(&CloakedPagesCS);
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, BaseAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, BaseAddress);

  if (cloakdata)
  {
    csEnter(&currentcpuinfo->EPTPML4CS);


    //it's a cloaked page
    //sendstringf("ept_handleCloakEvent on the target(CS:RIP=%x:%6)\n", currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);

    if (isAMD)
    {
      //AMD handling
      //swap the page and make it executable,
      *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
      cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->US=1;
      cloakdata->npentry[currentcpuinfo->cpunr]->EXB=0;

      if (cloakdata->CloakMode==0)
      {
        //do one step, and restore back to non-executable
        vmx_enableSingleStepMode();
        vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);
      }
      else
      {
        //mark all pages as no execute except this one (and any potential still waiting to step cloak events)
        currentcpuinfo->NP_Cloak.ActiveRegion=cloakdata;
        currentcpuinfo->NP_Cloak.LastCloakedVirtualBase=currentcpuinfo->vmcb->RIP & 0xffffffffffff000ULL;
        NPMode1CloakSetState(currentcpuinfo, 1);
      }

    }
    else
    {
      //Intel handling
      EPT_VIOLATION_INFO evi;
      evi.ExitQualification=vmread(vm_exit_qualification);

      int isMegaJmp=0;
      QWORD RIP;

      if (!isAMD)
        RIP=vmread(vm_guest_rip);

      //todo: keep a special list for physical address regions that can see 'the truth' (e.g ntoskrnl.exe and hal.dll on exported data pointers, but anything else will see the fake pointers)
      //todo2: inverse cloak, always shows the real data except the list of physical address regions provided

      //check for megajmp edits
      //megajmp: ff 25 00 00 00 00 <address>
      //So, if 6 bytes before the given address is ff 25 00 00 00 00 , it's a megajmp, IF the RIP is 6 bytes before the given address (and the bytes have changed from original)

      if ((AddressVA-RIP)==6)
      {
        //check if the bytes have been changed here
        DWORD offset=RIP & 0xfff;
        int size=min(14,0x1000-offset);

        unsigned char *new=(unsigned char *)((QWORD)cloakdata->Executable+offset);
        unsigned char *original=(unsigned char *)((QWORD)cloakdata->Data+offset);


        if (new[0]==0xff) //starts with 0xff, so very likely, inspect more
        {
          if (memcmp(new, original, size))
          {
            //the memory in this range got changed, check if it's a full megajmp
            unsigned char megajmpbytes[6]={0xff,0x25,0x00,0x00,0x00,0x00};

            if (memcmp(new, megajmpbytes, min(6,size))==0)
            {
              sendstring("Is megajmp");
              isMegaJmp=1;
            }

          }
        }
      }



      //Check if this page has had a MEGAJUMP code edit, if so, check if this is a megajump and in that case on executable
      if (evi.X) //looks like this cpu does not support execute only
        *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
      else
      {
        if (isMegaJmp==0)
        {
          //read/write the data
          *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressData;
        }
        else
        {
          //read the executable code
          *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable;
        }

      }
      cloakdata->eptentry[currentcpuinfo->cpunr]->WA=1;
      cloakdata->eptentry[currentcpuinfo->cpunr]->RA=1;
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=1;

      vmx_enableSingleStepMode();
      vmx_addSingleSteppingReasonEx(currentcpuinfo, 2,cloakdata);

      currentcpuinfo->eptCloak_LastOperationWasWrite=evi.W;
      currentcpuinfo->eptCloak_LastWriteOffset=Address & 0xfff;
    }



    result=TRUE;



    csLeave(&currentcpuinfo->EPTPML4CS);
  }

  //still here so not in the map (or no map used yet)

  csLeave(&CloakedPagesCS);

  ept_invalidate();


  return result;
}

int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo,  PCloakedPageData cloakdata)
{
  sendstringf("ept_handleCloakEventAfterStep\n");
  //back to execute only
  csEnter(&CloakedPagesCS);

  if (currentcpuinfo->eptCloak_LastOperationWasWrite)
  {
    //todo: apply the write as well


  }


  csEnter(&currentcpuinfo->EPTPML4CS);

  if (isAMD)
  {
    sendstringf("%d: ept_handleCloakEventAfterStep for AMD. cloakdata=%6\n", currentcpuinfo->cpunr, cloakdata);
    sendstringf("swapping the current page back with the data page\n", cloakdata);

    sendstringf("old npentry value = %6\n",*(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr]));

    *(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressData; //back to the non-executable state
    cloakdata->npentry[currentcpuinfo->cpunr]->EXB=1;

    cloakdata->npentry[currentcpuinfo->cpunr]->P=1;
    cloakdata->npentry[currentcpuinfo->cpunr]->RW=1;
    cloakdata->npentry[currentcpuinfo->cpunr]->US=1;


    sendstringf("new npentry value = %6\n",*(QWORD *)(cloakdata->npentry[currentcpuinfo->cpunr]));


  }
  else
  {
    *(QWORD *)(cloakdata->eptentry[currentcpuinfo->cpunr])=cloakdata->PhysicalAddressExecutable; //back to the executable state
    cloakdata->eptentry[currentcpuinfo->cpunr]->WA=0;
    cloakdata->eptentry[currentcpuinfo->cpunr]->RA=0;
    if (has_EPT_ExecuteOnlySupport)
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=1;
    else
      cloakdata->eptentry[currentcpuinfo->cpunr]->XA=0;
  }
  csLeave(&currentcpuinfo->EPTPML4CS);


  csLeave(&CloakedPagesCS);


  ept_invalidate();

  return 0;
}


int ept_cloak_activate(QWORD physicalAddress, int mode)
{
  int i;
  QWORD address;
  PCloakedPageData data;

  sendstringf("ept_cloak_activate(%6,%d)\n", physicalAddress, mode);

  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;
  csEnter(&CloakedPagesCS);

  //first run check
  if (CloakedPagesMap==NULL)
  {
    if (CloakedPagesList==NULL)
      CloakedPagesList=addresslist_create();

    if (CloakedPagesList->size>=MAXCLOAKLISTBEFORETRANFERTOMAP)
    {
      //convert the list to a map
      if (CloakedPagesMap==NULL) //should be
      {
        CloakedPagesMap=createPhysicalMemoryMap();
        for (i=0; i<CloakedPagesList->size; i++)
        {
          address=CloakedPagesList->list[i].address;
          data=(PCloakedPageData)CloakedPagesList->list[i].data;

          map_setEntry(CloakedPagesMap, address, data);
          //just copy, no need to reactivate
        }

        addresslist_destroy(CloakedPagesList);
        CloakedPagesList=NULL;
      }
    }
  }


  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);


  if (cloakdata)
  {
    //already cloaked
    csLeave(&CloakedPagesCS);
    return 1;
  }

  //new one, allocate and fill in a cloakdata structure
  int cpucount=getCPUCount();
  cloakdata=malloc(sizeof(CloakedPageData)+cpucount*sizeof(PEPT_PTE));
  zeromemory(cloakdata,sizeof(CloakedPageData)+cpucount*sizeof(PEPT_PTE));


  //fill in the data
  cloakdata->Executable=mapPhysicalMemoryGlobal(physicalAddress, 4096);
  cloakdata->Data=malloc(4096);

  copymem(cloakdata->Data, cloakdata->Executable, 4096);


  cloakdata->PhysicalAddressExecutable=physicalAddress;
  cloakdata->PhysicalAddressData=VirtualToPhysical(cloakdata->Data);

  //Intel. The page is unreadable. Reads cause a fault and then get handled by a single step, executes run the modified code with no exception (fastest way possible)
  //In case Execute but no read is not supported single step through the whole page (slow)
  //Mode is ignored

  //AMD: The page is readable but non-executable
  //On execute the page gets swapped out with the modified one and made executable
  //mode 0: For every address make it executable, do a single step, and then made back to non-executable (see execute watch)
  //mode 1: All other pages will be made non-executable (511 PTE's 511 PDE, 511 PDPTE and 511 PML4's need to be made non-executable: 2044 entries )
  //        until a npf happens.
  //        Boundary situation: Do a single step when at an instruction that spans both pages
  //may mode 2?:same as mode 1 but instead of adjusting 2044 entries swap the NP pointer to a pagesystem with only that one executable page. And on memory access outside map them in as usual (as non executable) unless it's a sidepage
  //            pro: Faster after a few runs. con: eats up a ton more memory

  // Downside compared to Intel: Cloaked pages can see their own page as edited

  cloakdata->CloakMode=mode;



  //map in the physical address descriptor for all CPU's as execute only
  pcpuinfo currentcpuinfo=firstcpuinfo;

  //lock ANY other CPU from triggering (a cpu could trigger before this routine is done)


  sendstringf("Cloaking memory (initiated by cpu %d) \n", getcpunr());

  while (currentcpuinfo)
  {
    int cpunr=currentcpuinfo->cpunr;
    sendstringf("cloaking cpu %d\n", cpunr);

    if (cpunr>=cpucount)
    {
      //'issue' with the cpucount or cpunumber
      cpucount=cpunr*2;
      cloakdata=realloc(cloakdata, cpucount*sizeof(PEPT_PTE));
    }

    csEnter(&currentcpuinfo->EPTPML4CS);

    currentcpuinfo->eptUpdated=1;


    QWORD PA;

    if (isAMD)
      PA=NPMapPhysicalMemory(currentcpuinfo, physicalAddress, 1);
    else
      PA=EPTMapPhysicalMemory(currentcpuinfo, physicalAddress, 1);

    sendstringf("%d Cloak: After mapping the page as a 4KB page\n", cpunr);

    cloakdata->eptentry[cpunr]=mapPhysicalMemoryGlobal(PA, sizeof(EPT_PTE));

    sendstringf("%d Cloak old entry is %6\n", cpunr,  *(QWORD*)(cloakdata->eptentry[cpunr]));


    if (isAMD)
    {
      //Make it non-executable, and make the data read be the fake data
      _PTE_PAE temp;
      temp=*((PPTE_PAE)&cloakdata->PhysicalAddressData); //read data

      temp.P=1;
      temp.RW=1;
      temp.US=1;
      temp.EXB=1; //disable execute

      *(PPTE_PAE)(cloakdata->eptentry[cpunr])=temp;
    }
    else
    {
      //make it nonreadable
      EPT_PTE temp=*(cloakdata->eptentry[cpunr]);
      if (has_EPT_ExecuteOnlySupport)
        temp.XA=1;
      else
      