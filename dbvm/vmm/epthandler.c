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
        temp.XA=0; //going to be slow

      temp.RA=0;
      temp.WA=0;

      *(cloakdata->eptentry[cpunr])=temp;
    }

    sendstringf("%d Cloak new entry is %6\n", cpunr, *(QWORD*)(cloakdata->eptentry[cpunr]));


    _wbinvd();
    currentcpuinfo->eptUpdated=1; //set this before unlock, so if a NP exception happens before the next vmexit is handled it knows not to remap it with full access

    csLeave(&currentcpuinfo->EPTPML4CS);

    currentcpuinfo=currentcpuinfo->next;
  }

  if (CloakedPagesMap)
    map_setEntry(CloakedPagesMap, physicalAddress, (void*)cloakdata);
  else
    addresslist_add(CloakedPagesList, physicalAddress, (void*)cloakdata);

  sendstringf("Invalidating ept\n");

  ept_invalidate();

  csLeave(&CloakedPagesCS);
  return 0;
}



int ept_cloak_deactivate(QWORD physicalAddress)
{
  int i;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  PCloakedPageData cloakdata;


  csEnter(&CloakedPagesCS);

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    //check if there is a changereg on bp
    csEnter(&ChangeRegBPListCS);
    for (i=0; i<ChangeRegBPListPos; i++)
    {
      if ((ChangeRegBPList[i].Active) && (ChangeRegBPList[i].cloakdata==cloakdata)) //delete this one first
        ept_cloak_removechangeregonbp(ChangeRegBPList[i].PhysicalAddress);
    }

    csLeave(&ChangeRegBPListCS);


    copymem(cloakdata->Executable, cloakdata->Data, 4096);
    pcpuinfo currentcpuinfo=firstcpuinfo;
    while (currentcpuinfo)
    {
    	if (isAMD)
    	{
    		_PTE_PAE temp=*((PPTE_PAE)&cloakdata->PhysicalAddressExecutable);
    		temp.P=1;
    		temp.RW=1;
    		temp.US=1;
    		temp.EXB=0;
    		*(cloakdata->npentry[currentcpuinfo->cpunr])=temp;
    	}
    	else
    	{
    		EPT_PTE temp=*(cloakdata->eptentry[currentcpuinfo->cpunr]);
    		temp.RA=1;
    		temp.WA=1;
    		temp.XA=1;
    		*(cloakdata->eptentry[currentcpuinfo->cpunr])=temp;
    	}
      _wbinvd();
      currentcpuinfo->eptUpdated=1;

      unmapPhysicalMemoryGlobal(cloakdata->eptentry[currentcpuinfo->cpunr], sizeof(EPT_PTE));
      currentcpuinfo=currentcpuinfo->next;
    }

    unmapPhysicalMemoryGlobal(cloakdata->Executable, 4096);

    free(cloakdata->Data);
    cloakdata->Data=NULL;

    free(cloakdata);
  }

  if (CloakedPagesMap)
    map_setEntry(CloakedPagesMap, physicalAddress, NULL);
  else
    addresslist_remove(CloakedPagesList, physicalAddress);




  csLeave(&CloakedPagesCS);

  ept_invalidate();

  //if there where cloak event events pending, then next time they violate, the normal handler will make it RWX on the address it should
  return (cloakdata!=NULL);
}

int ept_cloak_readOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD destination)
/* Called by vmcall */
{
  int error;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  QWORD pagefault;

  void *dest=mapVMmemory(currentcpuinfo, destination, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    void *src=mapPhysicalMemory(cloakdata->PhysicalAddressExecutable, 4096);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(src,4096);
  }
  else
  {
    registers->rax=1;
  }

  if (isAMD)
    currentcpuinfo->vmcb->RAX= registers->rax;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(dest,4096);


  if (isAMD)
  {
    if (AMD_hasNRIPS)
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
    else
      currentcpuinfo->vmcb->RIP+=3;
  }
  else
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  return 0;
}

int ept_cloak_writeOriginal(pcpuinfo currentcpuinfo,  VMRegisters *registers, QWORD physicalAddress, QWORD source)
{
  int error;
  physicalAddress=physicalAddress & MAXPHYADDRMASKPB;

  nosendchar[getAPICID()]=0;
  sendstring("ept_cloak_writeOriginal");

  QWORD pagefault;

  void *src=mapVMmemory(currentcpuinfo, source, 4096,&error, &pagefault);

  if (error==2)
    return raisePagefault(currentcpuinfo, pagefault);

  csEnter(&CloakedPagesCS);
  PCloakedPageData cloakdata;

  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalAddress);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalAddress);

  if (cloakdata)
  {
    void *dest=mapPhysicalMemory(cloakdata->PhysicalAddressExecutable, 4096);

    sendstringf("cloakdata->PhysicalAddressExecutable=%6\n", cloakdata->PhysicalAddressExecutable);
    sendstringf("cloakdata->PhysicalAddressData=%6\n", cloakdata->PhysicalAddressData);


    sendstringf("Writing to PA %6\n", cloakdata->PhysicalAddressExecutable);
    copymem(dest,src,4096);
    registers->rax=0;

    unmapPhysicalMemory(dest,4096);

  }
  else
    registers->rax=1;

  if (isAMD)
    currentcpuinfo->vmcb->RAX= registers->rax;

  csLeave(&CloakedPagesCS);

  unmapVMmemory(src,4096);


  if (isAMD)
  {
    if (AMD_hasNRIPS)
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
    else
      currentcpuinfo->vmcb->RIP+=3;
  }
  else
    vmwrite(vm_guest_rip,vmread(vm_guest_rip)+vmread(vm_exit_instructionlength));

  return 0;
}

int ept_cloak_traceonbp_getstatus(DWORD *count, DWORD *maxcount)
//return: 0=no trace configured. 1=trace configured but not started yet, 2=trace configured and started, 3=trace done
{
  int result=0;
  *count=0;
  *maxcount=0;
  csEnter(&CloakedPagesCS);
  if (TraceOnBP)
  {

    sendstringf("TraceOnBP->numberOfEntries=%d\n", TraceOnBP->numberOfEntries);
    sendstringf("TraceOnBP->count=%d\n", TraceOnBP->count);
    *count=TraceOnBP->numberOfEntries;
    *maxcount=TraceOnBP->count+TraceOnBP->numberOfEntries;

    sendstringf("*maxcount=%d\n", *maxcount);
    result=1;

    if (TraceOnBP->triggered)
    {
      result=2;
      if (TraceOnBP->finished)
      {
        result=3;
      }
    }
  }
  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_traceonbp_stoptrace() //stops it, but doesn't delete it
//0=no trace going on
//1=trace configured, but not triggered yet
//2=trace was configured and already triggered, but not finished yet
//3=trace was finished
{
  int result=0;
  csEnter(&CloakedPagesCS);
  if (TraceOnBP)
  {
    result=1;
    TraceOnBP->shouldquit=1;

    if (TraceOnBP->triggered==0)
    {
      unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
      executable[TraceOnBP->PhysicalAddress & 0xfff]=TraceOnBP->originalbyte;
    }
    else
      result=2;

    if (TraceOnBP->finished)
    {
      result=3;
    }
  }
  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_traceonbp_remove(int forcequit)
{
  if (TraceOnBP)
  {
    csEnter(&CloakedPagesCS);
    if (TraceOnBP)
    {
      if (TraceOnBP->triggered==FALSE)
      {
        //still needs to restore the byte
        unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
        executable[TraceOnBP->PhysicalAddress & 0xfff]=TraceOnBP->originalbyte;
      }
      else
      {
        if (TraceOnBP->finished==FALSE)
        {
          //trace is still going
          TraceOnBP->shouldquit=1;

          if (forcequit==0)
          {
            csLeave(&CloakedPagesCS);
            return 2; //can not disable yet (tell CE to try again in a bit)
          }
        }
      }

      free(TraceOnBP);
      TraceOnBP=NULL;

      csLeave(&CloakedPagesCS);
      return 1;
    }

    csLeave(&CloakedPagesCS);
  }

  return 0; //no trace to delete
}

int ept_cloak_traceonbp(QWORD physicalAddress, DWORD flags, DWORD tracecount)
{
  int result=1;
  if (ept_cloak_traceonbp_remove(0)==2) return 2;

  QWORD physicalBase=physicalAddress & MAXPHYADDRMASKPB;
  ept_cloak_activate(physicalBase,0); //just making sure

  sendstringf("ept_cloak_traceonbp for %6", physicalAddress);

  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalBase);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalBase);


  if (cloakdata)
  {
    //found it.  Create an int3 bp at that spot
    int offset=physicalAddress & 0xfff;
    unsigned char *executable=cloakdata->Executable;

    //

    int entrytype=0;
    int entrysize=sizeof(PageEventBasic);
    int logfpu=(flags & 1);
    int logstack=(flags & 2);
    int logsize;

    if ((logfpu==0) && (logstack==0))
    {
      entrytype=0;
      entrysize=sizeof(PageEventBasic);
    }
    else
    if ((logfpu==1) && (logstack==0))
    {
      entrytype=1;
      entrysize=sizeof(PageEventExtended);
    }
    else
    if ((logfpu==0) && (logstack==1))
    {
      entrytype=2;
      entrysize=sizeof(PageEventBasicWithStack);
    }
    else //fpu=1 and stack=1
    {
      entrytype=3;
      entrysize=sizeof(PageEventExtendedWithStack);
    }

    logsize=sizeof(TraceOnBPEntry)*2+entrysize*tracecount;

    sendstringf("Going to allocate %d bytes for the log...", logsize);

    TraceOnBP=malloc(logsize);
    if (TraceOnBP)
    {
      zeromemory(TraceOnBP, logsize);
      sendstringf("Success. Allocated at %6\n", TraceOnBP);
      TraceOnBP->PhysicalAddress=physicalAddress;
      TraceOnBP->triggered=0;
      TraceOnBP->finished=0;
      TraceOnBP->shouldquit=0;
      TraceOnBP->count=tracecount;
      TraceOnBP->cloakdata=cloakdata;
      TraceOnBP->originalbyte=executable[offset];
      TraceOnBP->datatype=entrytype;

      executable[offset]=0xcc; //int3 bp's will happen now (even on other CPU's)

      result=0;
    }
    else
      result=3; //not enough memory free
  }

  csLeave(&CloakedPagesCS);

  return result;


}

int ept_cloak_changeregonbp(QWORD physicalAddress, PCHANGEREGONBPINFO changereginfo)
{
  int result=1;

  nosendchar[getAPICID()]=0;
  sendstringf("ept_cloak_changeregonbp(%6,%6)\n", physicalAddress, changereginfo);

  sendstringf("Removing old changeregonbp\n");
  ept_cloak_removechangeregonbp(physicalAddress);

  QWORD physicalBase=physicalAddress & MAXPHYADDRMASKPB;

  sendstringf("Activating cloak at base %6 (if not yet active)\n", physicalBase);
  ept_cloak_activate(physicalBase,0); //just making sure

  sendstringf("ept_cloak_changeregonbp:\n");
  sendstringf("  changeRAX:%d\n", changereginfo->Flags.changeRAX);
  sendstringf("  changeRBX:%d\n", changereginfo->Flags.changeRBX);
  sendstringf("  changeRCX:%d\n", changereginfo->Flags.changeRCX);
  sendstringf("  changeRDX:%d\n", changereginfo->Flags.changeRDX);
  sendstringf("  changeRSI:%d\n", changereginfo->Flags.changeRSI);
  sendstringf("  changeRDI:%d\n", changereginfo->Flags.changeRDI);
  sendstringf("  changeRBP:%d\n", changereginfo->Flags.changeRBP);
  sendstringf("  changeRSP:%d\n", changereginfo->Flags.changeRSP);
  sendstringf("  changeRIP:%d\n", changereginfo->Flags.changeRIP);
  sendstringf("  changeR8:%d\n", changereginfo->Flags.changeR8);
  sendstringf("  changeR9:%d\n", changereginfo->Flags.changeR9);
  sendstringf("  changeR10:%d\n", changereginfo->Flags.changeR10);
  sendstringf("  changeR11:%d\n", changereginfo->Flags.changeR11);
  sendstringf("  changeR12:%d\n", changereginfo->Flags.changeR12);
  sendstringf("  changeR13:%d\n", changereginfo->Flags.changeR13);
  sendstringf("  changeR14:%d\n", changereginfo->Flags.changeR14);
  sendstringf("  changeR15:%d\n", changereginfo->Flags.changeR15);
  sendstringf("  changeCF:%d\n", changereginfo->Flags.changeCF);
  sendstringf("  changePF:%d\n", changereginfo->Flags.changePF);
  sendstringf("  changeAF:%d\n", changereginfo->Flags.changeAF);
  sendstringf("  changeZF:%d\n", changereginfo->Flags.changeZF);
  sendstringf("  changeSF:%d\n", changereginfo->Flags.changeSF);
  sendstringf("  changeOF:%d\n", changereginfo->Flags.changeOF);
  sendstringf("  newCF:%d\n", changereginfo->Flags.newCF);
  sendstringf("  newPF:%d\n", changereginfo->Flags.newPF);
  sendstringf("  newAF:%d\n", changereginfo->Flags.newAF);
  sendstringf("  newZF:%d\n", changereginfo->Flags.newZF);
  sendstringf("  newSF:%d\n", changereginfo->Flags.newSF);
  sendstringf("  newOF:%d\n", changereginfo->Flags.newOF);

  sendstringf("  newRAX:%d\n", changereginfo->newRAX);
  sendstringf("  newRBX:%d\n", changereginfo->newRBX);
  sendstringf("  newRCX:%d\n", changereginfo->newRCX);
  sendstringf("  newRDX:%d\n", changereginfo->newRDX);
  sendstringf("  newRSI:%d\n", changereginfo->newRSI);
  sendstringf("  newRDI:%d\n", changereginfo->newRDI);
  sendstringf("  newRBP:%d\n", changereginfo->newRBP);
  sendstringf("  newRSP:%d\n", changereginfo->newRSP);
  sendstringf("  newRIP:%d\n", changereginfo->newRIP);
  sendstringf("  newR8:%d\n", changereginfo->newR8);
  sendstringf("  newR9:%d\n", changereginfo->newR9);
  sendstringf("  newR10:%d\n", changereginfo->newR10);
  sendstringf("  newR11:%d\n", changereginfo->newR11);
  sendstringf("  newR12:%d\n", changereginfo->newR12);
  sendstringf("  newR13:%d\n", changereginfo->newR13);
  sendstringf("  newR14:%d\n", changereginfo->newR14);
  sendstringf("  newR15:%d\n", changereginfo->newR15);


  csEnter(&CloakedPagesCS);

  PCloakedPageData cloakdata;
  if (CloakedPagesMap)
    cloakdata=map_getEntry(CloakedPagesMap, physicalBase);
  else
    cloakdata=addresslist_find(CloakedPagesList, physicalBase);


  if (cloakdata)
  {
    //found it.  Create an int3 bp at that spot
    int ID=-1;
    int offset=physicalAddress & 0xfff;
    unsigned char *executable=cloakdata->Executable;

    //
    csEnter(&ChangeRegBPListCS);
    int j;
    for (j=0; j<ChangeRegBPListPos; j++)
    {
      if (ChangeRegBPList[j].Active==0)
      {
        ID=j;
        break;
      }
    }
    if (ID==-1)
    {
      ID=ChangeRegBPListPos;
      ChangeRegBPListPos++;
      if (ChangeRegBPListPos>=ChangeRegBPListSize) //realloc the list
      {
        ChangeRegBPListSize=(ChangeRegBPListSize+2)*2;
        ChangeRegBPList=realloc(ChangeRegBPList, sizeof(ChangeRegBPEntry)*ChangeRegBPListSize);
      }
    }


    ChangeRegBPList[ID].PhysicalAddress=physicalAddress;
    ChangeRegBPList[ID].originalbyte=executable[offset];
    ChangeRegBPList[ID].changereginfo=*changereginfo;
    ChangeRegBPList[ID].cloakdata=cloakdata;
    ChangeRegBPList[ID].Active=1;

    executable[offset]=0xcc; //int3 bp's will happen now (even on other CPU's)

    csLeave(&ChangeRegBPListCS);
    result=0;
  }

  csLeave(&CloakedPagesCS);

  return result;
}

int ept_cloak_removechangeregonbp(QWORD physicalAddress)
{
  int i;
  int result=1;
  csEnter(&CloakedPagesCS);
  csEnter(&ChangeRegBPListCS);
  for (i=0; i<ChangeRegBPListPos; i++)
  {
    if ((ChangeRegBPList[i].Active) && (ChangeRegBPList[i].PhysicalAddress==physicalAddress))
    {
      unsigned char *executable=(unsigned char *)ChangeRegBPList[i].cloakdata->Executable;
      executable[physicalAddress & 0xfff]=ChangeRegBPList[i].originalbyte;
      ChangeRegBPList[i].Active=0;

      /*  _wbinvd();
      vpid_invalidate();
      ept_invalidate();*/
      result=0;
    }
  }

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}

BOOL ept_handleHardwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  int result=FALSE;
  if (TraceOnBP)
  {
    QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);
    csEnter(&CloakedPagesCS);

    nosendchar[getAPICID()]=0;
    sendstringf("%6: ept_handleHardwareBreakpoint:\n", RIP);

    if (TraceOnBP && (TraceOnBP->triggered))
    {
      regDR6 dr6;
      QWORD cr3;
      QWORD fsbase,gsbase, kernelgsbase;
      kernelgsbase=readMSR(0xc0000102);
      int isDuetoSingleStep;

      if (isAMD)
      {
        dr6.DR6=currentcpuinfo->vmcb->DR6;
        cr3=currentcpuinfo->vmcb->CR3;
        fsbase=currentcpuinfo->vmcb->fs_base;
        gsbase=currentcpuinfo->vmcb->gs_base;
        isDuetoSingleStep=dr6.BS;
        //kernelgsbase=currentcpuinfo->vmcb->KernelGsBase; //maybe?
      }
      else
      {
        dr6.DR6=getDR6();

        cr3=vmread(vm_guest_cr3);
        fsbase=vmread(vm_guest_fs_base);
        gsbase=vmread(vm_guest_gs_base);

        isDuetoSingleStep=(vmread(vm_exit_qualification) & 0x4000)!=0;

      }



      sendstringf("Checking state:\n");
      sendstringf("DR6=%8  DR6.BS=%d isDuetoSingleStep=%d\n", dr6.DR6, dr6.BS, isDuetoSingleStep);
      sendstringf("TraceOnBP->triggeredcr3=%8\n" , TraceOnBP->triggeredcr3);
      sendstringf("TraceOnBP->triggeredfsbase=%8\n" , TraceOnBP->triggeredfsbase);
      sendstringf("TraceOnBP->triggeredgsbase=%8\n" , TraceOnBP->triggeredgsbase);
      sendstringf("TraceOnBP->cr3=%8\n" , cr3);
      sendstringf("TraceOnBP->fsbase=%8\n" , fsbase);
      sendstringf("TraceOnBP->gsbase=%8\n" , gsbase);
      sendstringf("TraceOnBP->gsbasekernel=%8\n" , kernelgsbase);


      if ((isDuetoSingleStep) && (TraceOnBP->triggeredcr3==cr3) && (TraceOnBP->triggeredfsbase==fsbase) && (TraceOnBP->triggeredgsbase==gsbase))
      {

        recordState(&TraceOnBP->pe, TraceOnBP->datatype, TraceOnBP->numberOfEntries, currentcpuinfo, vmregisters, fxsave);
        TraceOnBP->numberOfEntries++;


        TraceOnBP->count--;
        if (TraceOnBP->count<=0)
          TraceOnBP->shouldquit=1;



        //setup resume state
        RFLAGS flags;
        flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
        flags.RF=1; //resume, but leave the TF flag

        if (TraceOnBP->shouldquit==0)
        {
          sendstringf("Setting TF\n");
          flags.TF=1;
        }
        else
        {
          sendstringf("Finishing trace\n");
          flags.TF=0;
          TraceOnBP->finished=1;
        }

        dr6.BS=0;
        if (isAMD)
        {
          currentcpuinfo->vmcb->RFLAGS=flags.value;
          currentcpuinfo->vmcb->DR6=dr6.DR6;
        }
        else
        {
          sendstringf("bla\n");
          flags.RF=1;
          vmwrite(vm_guest_rflags, flags.value);
          if (flags.TF)
          {
            //dr6.BS=1;
           // vmwrite(vm_pending_debug_exceptions, (1<<14)); //set the TF flag in pending debug registers

          }
          else
          {
            //dr6.BS=0;
           //vmwrite(vm_pending_debug_exceptions, vmread(vm_pending_debug_exceptions) & ~(1<<14)); //unset the single step flag
          }

          setDR6(dr6.DR6);
        }

        result=TRUE;
      }
      else
        sendstringf("unexpected hardware breakpoint while tracing. skipping\n");
    }
    else
      sendstringf("tracing hasn't started. skipping\n");

    csLeave(&CloakedPagesCS);
  }
  else
    sendstring("no tracing going on. skipping\n");

  return result;
}

BOOL ept_handleFrozenThread(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave, int id)
//pre: brokenThreadListCS should be locked during this call
{
  int result=TRUE;
  RFLAGS v;
  v.value=BrokenThreadList[id].state.basic.FLAGS;
  QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);

  nosendchar[getAPICID()]=0;
  //sendstringf("ept_handleFrozenThread: RIP=%6\n",RIP);

  BrokenThreadList[id].state.basic.Count++;//heartbeat to show it's still triggering the BP
  if (BrokenThreadList[id].continueMethod)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("continueMethod is not 0\n");




    //restore the state according to the saved state (could have been changed) and do a single step or run  (this also undoes the state used value in rax, which is good)
    if (isAMD)
    {
      currentcpuinfo->vmcb->RIP=BrokenThreadList[id].state.basic.RIP;
      currentcpuinfo->vmcb->RAX=BrokenThreadList[id].state.basic.RAX;
      currentcpuinfo->vmcb->RSP=BrokenThreadList[id].state.basic.RSP;

      v.RF=1; //tell the watch handler to skip this if it returns at the same spot again
      currentcpuinfo->vmcb->RFLAGS=v.value;
    }
    else
    {
      vmregisters->rax=BrokenThreadList[id].state.basic.RAX;
      vmwrite(vm_guest_rip,BrokenThreadList[id].state.basic.RIP);
      vmwrite(vm_guest_rsp, BrokenThreadList[id].state.basic.RSP);

      vmwrite(vm_guest_interruptability_state,1); //tell the watch handler to skip this if it returns at the same spot again
    }
    vmregisters->rbx=BrokenThreadList[id].state.basic.RBX;
    vmregisters->rcx=BrokenThreadList[id].state.basic.RCX;
    vmregisters->rdx=BrokenThreadList[id].state.basic.RDX;
    vmregisters->rsi=BrokenThreadList[id].state.basic.RSI;
    vmregisters->rdi=BrokenThreadList[id].state.basic.RDI;
    vmregisters->rbp=BrokenThreadList[id].state.basic.RBP;
    vmregisters->r8=BrokenThreadList[id].state.basic.R8;
    vmregisters->r9=BrokenThreadList[id].state.basic.R9;
    vmregisters->r10=BrokenThreadList[id].state.basic.R10;
    vmregisters->r11=BrokenThreadList[id].state.basic.R11;
    vmregisters->r12=BrokenThreadList[id].state.basic.R12;
    vmregisters->r13=BrokenThreadList[id].state.basic.R13;
    vmregisters->r14=BrokenThreadList[id].state.basic.R14;
    vmregisters->r15=BrokenThreadList[id].state.basic.R15;

    *fxsave=BrokenThreadList[id].state.fpudata;

    if (BrokenThreadList[id].continueMethod==1)
    {
      sendstringf("This is a single step, so setting single step mode\n");
      //set single stepping
      vmx_enableSingleStepMode();
      vmx_addSingleSteppingReason(currentcpuinfo, SSR_STEPANDBREAK, id); //restore rip back to int3 bp after the step

      BrokenThreadList[id].watchid=-1; //set it as single stepping
    }
    else
    {
      BrokenThreadList[id].inuse=0; //continue (on purpuse)
      BrokenThreadList[id].continueMethod=0;

      sendstringf("Just continue.  It should continue at %2:%6\n",BrokenThreadList[id].state.basic.CS, BrokenThreadList[id].state.basic.RIP);
      if (isAMD)
      {
        sendstringf("It will continue at %2:%6\n",(unsigned char)currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP);
      }
      else
      {
        sendstringf("It will continue at %2:%6\n",(unsigned char)vmread(vm_guest_cs), vmread(vm_guest_rip));
      }


      //do one instruction at least
      if (isAMD)
      {
        if (v.IF)
          currentcpuinfo->vmcb->INTERRUPT_SHADOW=1;
      }
      else
      {
        vmwrite(vm_guest_interruptability_state,1); //blocking by sti
      }



    }
  }
  else
  {
    //RFLAGS v2;
    //v2.value=currentcpuinfo->vmcb->RFLAGS;

    //sendstringf("%d: Still frozen at %6  CR8=%x stored: IF=%d RF=%d current: IF=%d rd=%d INTERRUPT_SHADOW=%d EFER=%x FMASK=%x\n", currentcpuinfo->cpunr, BrokenThreadList[id].state.basic.RIP, getCR8(), v.IF, v.RF, v2.IF, v2.RF, currentcpuinfo->vmcb->INTERRUPT_SHADOW,
    //    currentcpuinfo->vmcb->EFER,
    //    currentcpuinfo->vmcb->SFMASK);


  }

  return result;
}

BOOL ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  //check if it is a cloaked instruction
  int i;
  int result=FALSE;
  QWORD RIP=isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip);

  nosendchar[getAPICID()]=0;


  //convert RIP into a physical address  (note that RIP has not been decreased by 1 yet)


  int notpaged;
  QWORD PA=getPhysicalAddressVM(currentcpuinfo, RIP, &notpaged);

 // sendstringf("ept_handleSoftwareBreakpoint. RFLAGS=%x\n", RIP, PA);

  if (notpaged==0) //should be since it's a software interrupt...
  {
    //sendstringf("paged\n");
    csEnter(&BrokenThreadListCS);
    if (BrokenThreadList && BrokenThreadListPos)
    {
      int shouldHaveBeenHandled=0;
      //sendstringf("Checking the broken threadlist");
      for (i=0; i<BrokenThreadListPos; i++)
      {
        if (BrokenThreadList[i].inuse)
        {
          QWORD cr3;
          QWORD rip,rax;

          if (isAMD)
          {
            cr3=currentcpuinfo->vmcb->CR3;
            rip=currentcpuinfo->vmcb->RIP;
            rax=currentcpuinfo->vmcb->RAX;
          }
          else
          {
            cr3=vmread(vm_guest_cr3);
            rip=vmread(vm_guest_rip);
            rax=vmregisters->rax;
          }

          //rsp might be a good detection point as well


          //warning: In windows, kernelmode gsbase changes depending on the cpu so can not be used as identifier then

          //check if it's matches this thread
          if ((cr3==BrokenThreadList[i].state.basic.CR3) && ((rip==BrokenThreadList[i].KernelModeLoop) || (rip==BrokenThreadList[i].UserModeLoop)))
          {
            shouldHaveBeenHandled=1;

            if ((QWORD)rax!=(QWORD)i) continue; //rax should match the brokenthreadlist id

            result=ept_handleFrozenThread(currentcpuinfo, vmregisters, fxsave, i);
            break;
          }
        }
      }

/*
      if ((!result) && (shouldHaveBeenHandled))
      {
        while (1);
      }
      */

    }
    csLeave(&BrokenThreadListCS);
    if (result) return result; //it was a frozen thread


    csEnter(&CloakedPagesCS);

    //if (TraceOnBP)
    //  sendstringf("TraceOnBP->PhysicalAddres=%6  PA=%6\n", TraceOnBP->PhysicalAddress, PA);


    if (TraceOnBP && (TraceOnBP->PhysicalAddress==PA))
    {

      if (TraceOnBP->triggered)
      {
        sendstringf("already triggered\n");
        csLeave(&CloakedPagesCS);
        return TRUE; //try again (something else got it first and likely restored the byte)
      }

      //todo: if option is to step through interrupts use vmx_enableSingleStepMode() and just follow this cpu instead of process

      //for now, just set stepping (which is visible to interrupts and pushf in that code)
      sendstringf("setting TF\n");

      RFLAGS flags;
      flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
      flags.TF=1;


      if (isAMD)
        currentcpuinfo->vmcb->RFLAGS=flags.value;
      else
        vmwrite(vm_guest_rflags, flags.value);


      int offset=TraceOnBP->PhysicalAddress & 0xfff;
      unsigned char *executable=(unsigned char *)TraceOnBP->cloakdata->Executable;
      executable[offset]=TraceOnBP->originalbyte;

      //save the first state

      recordState(&TraceOnBP->pe, TraceOnBP->datatype, TraceOnBP->numberOfEntries, currentcpuinfo, vmregisters, fxsave);
      TraceOnBP->numberOfEntries++;
      TraceOnBP->count--;


      TraceOnBP->triggered=1;
      TraceOnBP->triggeredcr3=isAMD?currentcpuinfo->vmcb->CR3:vmread(vm_guest_cr3);
      TraceOnBP->triggeredgsbase=isAMD?currentcpuinfo->vmcb->gs_base:vmread(vm_guest_gs_base);
      TraceOnBP->triggeredfsbase=isAMD?currentcpuinfo->vmcb->fs_base:vmread(vm_guest_fs_base);

      sendstringf("TraceOnBP->triggeredcr3=%6\n", TraceOnBP->triggeredcr3);
      sendstringf("TraceOnBP->triggeredfsbase=%6\n", TraceOnBP->triggeredfsbase);
      sendstringf("TraceOnBP->triggeredgsbase=%6\n", TraceOnBP->triggeredgsbase);

      csLeave(&CloakedPagesCS);

      sendstringf("returning true\n");
      sendstringf("currentcpuinfo->vmcb->InterceptExceptions=%6\n", currentcpuinfo->vmcb->InterceptExceptions);
      return TRUE;
    }



    csEnter(&ChangeRegBPListCS);
    for (i=0; i<ChangeRegBPListPos; i++)
    {
      if (ChangeRegBPList[i].PhysicalAddress==PA)
      {
        if (ChangeRegBPList[i].Active)
        {
          QWORD oldRIP=RIP;
          //it's a match
          //Todo: Only change if the processID matches  (todo: Add a getProcessID option provided by the OS based caller)
          //For now, make sure that the physical page is not shared, or that the register change is compatible with different processes (e.g kernelmode only, or a Flag change)

          //change regs

          if (ChangeRegBPList[i].changereginfo.Flags.changeRAX)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RAX=ChangeRegBPList[i].changereginfo.newRAX;
            else
              vmregisters->rax=ChangeRegBPList[i].changereginfo.newRAX;
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeRBX) vmregisters->rbx=ChangeRegBPList[i].changereginfo.newRBX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRCX) vmregisters->rcx=ChangeRegBPList[i].changereginfo.newRCX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRDX) vmregisters->rdx=ChangeRegBPList[i].changereginfo.newRDX;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRSI) vmregisters->rsi=ChangeRegBPList[i].changereginfo.newRSI;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRDI) vmregisters->rdi=ChangeRegBPList[i].changereginfo.newRDI;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRBP) vmregisters->rbp=ChangeRegBPList[i].changereginfo.newRBP;
          if (ChangeRegBPList[i].changereginfo.Flags.changeRSP)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RSP=ChangeRegBPList[i].changereginfo.newRSP;
            else
              vmwrite(vm_guest_rsp, ChangeRegBPList[i].changereginfo.newRSP);
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeRIP)
          {
            if (isAMD)
              currentcpuinfo->vmcb->RIP=ChangeRegBPList[i].changereginfo.newRIP;
            else
              vmwrite(vm_guest_rip, ChangeRegBPList[i].changereginfo.newRIP);
          }
          if (ChangeRegBPList[i].changereginfo.Flags.changeR8)  vmregisters->r8=ChangeRegBPList[i].changereginfo.newR8;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR9)  vmregisters->r9=ChangeRegBPList[i].changereginfo.newR9;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR10) vmregisters->r10=ChangeRegBPList[i].changereginfo.newR10;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR11) vmregisters->r11=ChangeRegBPList[i].changereginfo.newR11;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR12) vmregisters->r12=ChangeRegBPList[i].changereginfo.newR12;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR13) vmregisters->r13=ChangeRegBPList[i].changereginfo.newR13;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR14) vmregisters->r14=ChangeRegBPList[i].changereginfo.newR14;
          if (ChangeRegBPList[i].changereginfo.Flags.changeR15) vmregisters->r15=ChangeRegBPList[i].changereginfo.newR15;

          if (ChangeRegBPList[i].changereginfo.changeFP)
          {
            int r;
            for (r=0; r<8; r++)
              if (ChangeRegBPList[i].changereginfo.changeFP & (1<<r))
              {
                copymem((void*)((QWORD)(&fxsave->FP_MM0)+10*r), (void*)((QWORD)(&fxsave->FP_MM0)+10*r),10);
              }

          }


          if (ChangeRegBPList[i].changereginfo.changeXMM)
          {
            int r;
            for (r=0; r<15; r++)
            {
              BYTE mask=(ChangeRegBPList[i].changereginfo.changeXMM >> (4*r)) & 0xf;
              if (mask)
              {
                DWORD *destparts=(DWORD *)((QWORD)(&fxsave->XMM0)+16*r);
                DWORD *sourceparts=(DWORD *)((QWORD)(&ChangeRegBPList[i].changereginfo.newXMM0)+16*r);
                int p;

                for (p=0; p<4; p++)
                {
                  if (mask & (1 << p))
                    destparts[p]=sourceparts[p];
                }
              }
            }

          }



          RFLAGS flags;
          flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
          if (ChangeRegBPList[i].changereginfo.Flags.changeCF) flags.CF=ChangeRegBPList[i].changereginfo.Flags.newCF;
          if (ChangeRegBPList[i].changereginfo.Flags.changePF) flags.PF=ChangeRegBPList[i].changereginfo.Flags.newPF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeAF) flags.AF=ChangeRegBPList[i].changereginfo.Flags.newAF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeZF) flags.ZF=ChangeRegBPList[i].changereginfo.Flags.newZF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeSF) flags.SF=ChangeRegBPList[i].changereginfo.Flags.newSF;
          if (ChangeRegBPList[i].changereginfo.Flags.changeOF) flags.OF=ChangeRegBPList[i].changereginfo.Flags.newOF;
          if (isAMD)
            currentcpuinfo->vmcb->RFLAGS=flags.value;
          else
            vmwrite(vm_guest_rflags, flags.value);


          //continue:
          if ((ChangeRegBPList[i].changereginfo.Flags.changeRIP==0) || (ChangeRegBPList[i].changereginfo.newRIP==oldRIP))
          {
            //RIP did not change

            //restore the original byte
            int offset=ChangeRegBPList[i].PhysicalAddress & 0xfff;


            unsigned char *executable=(unsigned char *)ChangeRegBPList[i].cloakdata->Executable;
            executable[offset]=ChangeRegBPList[i].originalbyte;

            //setup single step mode
            vmx_enableSingleStepMode();
            vmx_addSingleSteppingReason(currentcpuinfo, 3,i); //change reg on bp, restore int3 bp

            //no interrupts for one instruction (no other interrupts are pending, it was an int3 that caused this)
            if (isAMD)
            {
              //
              currentcpuinfo->vmcb->INTERRUPT_SHADOW=1;
            }
            else
            {
              vmwrite(vm_guest_interruptability_state,2);
            }

            /* on systems with no exec only support, this means there will be 2 single step reasons.
             * One for the breakpoint restore, and one to set the read disable back
             */

          }
          result=TRUE;
          break;
        }
        else
        {
          //probably a stale breakpoint event that was waiting for the spinlock (what are the changes that there was a 0xcc at the exact same spot a previous bp was set)
          //try again
          //todo: keep a try again counter
          result=TRUE;
        }
      }
    }

    csLeave(&ChangeRegBPListCS);
    csLeave(&CloakedPagesCS);
  }
  else
    sendstringf("Unreadable memory address for an int3 bp....\n");

  return result;
}

int ept_handleStepAndBreak(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave, int brokenthreadid)
{

  //first check if you can break here. If not, goodbye (todo:step till you can)
  nosendchar[getAPICID()]=0;
  sendstringf("ept_handleStepAndBreak\n");
  DWORD CR8=getCR8();
  RFLAGS flags;
  flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);


  if ((CR8==0) && (flags.IF)) //if interruptable with no mask (on windows called passive mode) (not 100% if on win32, but who uses that...)
  {
    int kernelmode=0;
    if (isAMD)
    {
      Segment_Attribs csattrib;
      csattrib.SegmentAttrib=currentcpuinfo->vmcb->cs_attrib;
      kernelmode=csattrib.DPL==0;
    }
    else
    {
      Access_Rights csar;
      csar.AccessRights=vmread(vm_guest_cs_access_rights);
      kernelmode=csar.DPL==0;
    }

    csEnter(&BrokenThreadListCS);

    if (isAMD) //normally this gets reset after the single step handler. But it needs to be reset here already
    {
      flags.TF=currentcpuinfo->singleStepping.PreviousTFState;
      currentcpuinfo->vmcb->RFLAGS=flags.value;
    }


    QWORD newRIP=0;

    if ((brokenthreadid<0) || (brokenthreadid>=BrokenThreadListPos))
      while (1);


    if (BrokenThreadList[brokenthreadid].continueMethod==1) //single step
      newRIP=kernelmode?BrokenThreadList[brokenthreadid].KernelModeLoop:BrokenThreadList[brokenthreadid].UserModeLoop; //anything else, will be a run

    if (newRIP) //e.g if no kernelmode is provided, skip kernelmode
    {
      //save the current state
      fillPageEventBasic(&BrokenThreadList[brokenthreadid].state.basic, vmregisters);
      BrokenThreadList[brokenthreadid].state.fpudata=*fxsave;

      //adjust RIP and RAX  (rip points to the parking spot, RAX contains the specific brokenthreadid (gets undone on resume anyhow)
      vmregisters->rax=brokenthreadid;
      if (isAMD)
      {
        currentcpuinfo->vmcb->RIP=newRIP;
        currentcpuinfo->vmcb->RAX=brokenthreadid;
      }
      else
        vmwrite(vm_guest_rip, newRIP);

      BrokenThreadList[brokenthreadid].continueMethod=0;
    }
    else
    {
      //delete
      BrokenThreadList[brokenthreadid].inuse=2; //mark it as lost
      BrokenThreadList[brokenthreadid].continueMethod=0;
    }

    csLeave(&BrokenThreadListCS);
  }
  else
  {
    csEnter(&BrokenThreadListCS);
    sendstringf("Can not be broken due to interrupt state. Deleting stepping mode\n");
    BrokenThreadList[brokenthreadid].inuse=2; //lost
    BrokenThreadList[brokenthreadid].continueMethod=0;
    csLeave(&BrokenThreadListCS);


    //else can't be broken here. bye bye
  }

  return 0;
}

int ept_getBrokenThreadListCount(void)
{
  return BrokenThreadListPos;
}



int ept_getBrokenThreadEntryShort(int id, int *WatchID, int *Status, QWORD *CR3, QWORD *FSBASE, QWORD *GSBASE, QWORD *GSBASE_KERNEL, DWORD *CS, QWORD *RIP, QWORD *heartbeat)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      *WatchID=BrokenThreadList[id].watchid;
      *Status=BrokenThreadList[id].inuse | (BrokenThreadList[id].continueMethod << 8);

      *CR3=BrokenThreadList[id].state.basic.CR3;
      *FSBASE=BrokenThreadList[id].state.basic.FSBASE;
      *GSBASE=BrokenThreadList[id].state.basic.GSBASE;
      *GSBASE_KERNEL=BrokenThreadList[id].state.basic.GSBASE_KERNEL;
      *CS=BrokenThreadList[id].state.basic.CS;
      *RIP=BrokenThreadList[id].state.basic.RIP;
      *heartbeat=BrokenThreadList[id].state.basic.Count;
    }
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;
}

int ept_getBrokenThreadEntryFull(int id, int *watchid, int *status, PPageEventExtended entry)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      //0..7:1=ok. 2=lost it
      //8..15: continuemethod (if not 0, still waiting to precess)
      *status=BrokenThreadList[id].inuse | (BrokenThreadList[id].continueMethod << 8);  //257=0x101 (inuse,continuemethod=step)   513=0x201  (inuse,run)

      *entry=BrokenThreadList[id].state;
      *watchid=BrokenThreadList[id].watchid;
    }
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;
}

int ept_setBrokenThreadEntryFull(int id, PPageEventExtended entry)
{
  int result=0;
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
      BrokenThreadList[id].state=*entry;
    else
      result=2;
  }
  else
    result=1;

  csLeave(&BrokenThreadListCS);
  return result;

}

int ept_resumeBrokenThread(int id, int continueMethod)
{
  int result=0;
  sendstringf("ept_resumeBrokenThread(%d,%d)\n",id, continueMethod);
  csEnter(&BrokenThreadListCS);
  if ((id>=0) && (id<BrokenThreadListPos))
  {
    if (BrokenThreadList[id].inuse)
    {
      if (BrokenThreadList[id].inuse==2)
      {
        sendstringf("This thread was abandoned. Releasing it's spot\n");

        //just release it
        BrokenThreadList[id].inuse=0;
        result=4;
      }
      else
      {
        if (BrokenThreadList[id].continueMethod==0)
        {
          sendstringf("Setting broken thread %d to continueMethod %d\n", id, continueMethod);
          BrokenThreadList[id].continueMethod=continueMethod;
          BrokenThreadList[id].watchid=-1;
        }
        else
        {
          sendstringf("already set to continue\n");
          result=3; //already set to continue
        }
      }
    }
    else
    {
      sendstringf("ID (%d) not in use\n", id);
      result=2; //not in use
    }
  }
  else
  {
    sendstringf("ID (%d) out of range\n", id);
    result=1; //out of range
  }

  csLeave(&BrokenThreadListCS);

  return result;
}



int ept_handleSoftwareBreakpointAfterStep(pcpuinfo currentcpuinfo UNUSED,  int ID)
{
  int result=0;
  csEnter(&CloakedPagesCS);
  csEnter(&ChangeRegBPListCS);
  if (ChangeRegBPList[ID].Active)//Just hope that you didn't quickly delete and then register a whole new breakpoint, as this will fuck you up
  {

    QWORD PA=ChangeRegBPList[ID].PhysicalAddress;
    QWORD PABase=PA & MAXPHYADDRMASKPB;
    int offset=PA-PABase;

    unsigned char *executable=(unsigned char*)ChangeRegBPList[ID].cloakdata->Executable;
    executable[offset]=0xcc; //set the breakpoint back
    result=0;
  }
  //else it got deleted before the step finished or total memory corruption that blanked out several memory regions

  csLeave(&ChangeRegBPListCS);
  csLeave(&CloakedPagesCS);

  return result;
}



/*
 * WATCH
 */


int getFreeWatchID()
/*
 * scan through the watches for an unused spot, if not found, reallocate the list
 * pre: The watchlistCS has been locked
 */
{
  int i,j;
  sendstringf("+getFreeWatchID\n");
  for (i=0; i<eptWatchListPos; i++)
  {
    if (eptWatchList[i].Active==0)
    {
      sendstringf("Found a non active entry at index %d\n", i);
      return i;
    }
  }

  //still here
  if (eptWatchListPos<eptWatchListSize)
  {
    sendstringf("eptWatchListPos(%d)<eptWatchListSize(%d)\n", eptWatchListPos, eptWatchListSize);
    return eptWatchListPos++;
  }

  sendstringf("Reallocating the list\n");

  //still here, realloc
  i=eptWatchListSize;
  eptWatchListSize=(eptWatchListSize+2)*2;
  eptWatchList=realloc(eptWatchList, eptWatchListSize*sizeof(EPTWatchEntry));

  for (j=i; j<eptWatchListSize; j++)
    eptWatchList[j].Active=0;

  eptWatchListPos++;

  return i;
}

void saveStack(pcpuinfo currentcpuinfo, unsigned char *stack) //stack is 4096 bytes
{
  int error;
  QWORD pagefaultaddress;
  int size=4096;
  QWORD rsp=isAMD?currentcpuinfo->vmcb->RSP:vmread(vm_guest_rsp);



  zeromemory(stack, 4096);
  //copy it but don't care about pagefaults (if there is a pagefault I 'could' trigger a pf and then wait and try again, but fuck it, it's not 'that' important
  unsigned char *gueststack=(unsigned char *)mapVMmemoryEx(currentcpuinfo, rsp, 4096, &error, &pagefaultaddress, 1);

  if (error)
  {
    if (error==2)
      size=pagefaultaddress-rsp;
    else
      return;
  }

  copymem(stack, gueststack, size);
  unmapVMmemory(gueststack, size);
}

void fillPageEventBasic(PageEventBasic *peb, VMRegisters *registers)
{

  peb->GSBASE_KERNEL=readMSR(IA32_GS_BASE_KERNEL_MSR);
  if (isAMD)
  {
    pcpuinfo c=getcpuinfo();

    peb->VirtualAddress=0;
    peb->PhysicalAddress=c->vmcb->EXITINFO2;
    peb->CR3=c->vmcb->CR3;
    peb->FSBASE=c->vmcb->fs_base;
    peb->GSBASE=c->vmcb->gs_base;

    peb->FLAGS=c->vmcb->RFLAGS;
    peb->RAX=c->vmcb->RAX;
    peb->RBX=registers->rbx;
    peb->RCX=registers->rcx;
    peb->RDX=registers->rdx;
    peb->RSI=registers->rsi;
    peb->RDI=registers->rdi;
    peb->R8=registers->r8;
    peb->R9=registers->r9;
    peb->R10=registers->r10;
    peb->R11=registers->r11;
    peb->R12=registers->r12;
    peb->R13=registers->r13;
    peb->R14=registers->r14;
    peb->R15=registers->r15;
    peb->RBP=registers->rbp;
    peb->RSP=c->vmcb->RSP;
    peb->RIP=c->vmcb->RIP;
    peb->DR0=getDR0();
    peb->DR1=getDR1();
    peb->DR2=getDR2();
    peb->DR3=getDR3();
    peb->DR6=c->vmcb->DR6;
    peb->DR7=c->vmcb->DR7;
    peb->CS=c->vmcb->cs_selector;
    peb->DS=c->vmcb->ds_selector;
    peb->ES=c->vmcb->es_selector;
    peb->SS=c->vmcb->ss_selector;
    peb->FS=c->vmcb->fs_selector;
    peb->GS=c->vmcb->gs_selector;

  }
  else
  {

    peb->VirtualAddress=vmread(vm_guest_linear_address);
    peb->PhysicalAddress=vmread(vm_guest_physical_address);
    peb->CR3=vmread(vm_guest_cr3);
    peb->FSBASE=vmread(vm_guest_fs_base);
    peb->GSBASE=vmread(vm_guest_gs_base);
    peb->FLAGS=vmread(vm_guest_rflags);
    peb->RAX=registers->rax;
    peb->RBX=registers->rbx;
    peb->RCX=registers->rcx;
    peb->RDX=registers->rdx;
    peb->RSI=registers->rsi;
    peb->RDI=registers->rdi;
    peb->R8=registers->r8;
    peb->R9=registers->r9;
    peb->R10=registers->r10;
    peb->R11=registers->r11;
    peb->R12=registers->r12;
    peb->R13=registers->r13;
    peb->R14=registers->r14;
    peb->R15=registers->r15;
    peb->RBP=registers->rbp;
    peb->RSP=vmread(vm_guest_rsp);
    peb->RIP=vmread(vm_guest_rip);

    peb->DR0=getDR0();
    peb->DR1=getDR1();
    peb->DR2=getDR2();
    peb->DR3=getDR3();

    peb->DR6=getDR6();
    peb->DR7=vmread(vm_guest_dr7);
    peb->CS=vmread(vm_guest_cs);
    peb->DS=vmread(vm_guest_ds);
    peb->ES=vmread(vm_guest_es);
    peb->SS=vmread(vm_guest_ss);
    peb->FS=vmread(vm_guest_fs);
    peb->GS=vmread(vm_guest_gs);
  }
  peb->Count=0;
}

void recordState(void *liststart, int datatype, int currentEntryNr, pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave)
{

  sendstringf("recordState(%p, %d, %d, %p, %p, %p)",liststart, datatype, currentEntryNr, currentcpuinfo, vmregisters, fxsave);
  int logentrysize=0;
  switch (datatype)
  {
    case PE_BASIC:
      logentrysize=sizeof(PageEventBasic);
      PageEventBasic *peb=(PageEventBasic *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic(peb, vmregisters); //physical and linear are ignored if a tracer log
      break;

    case PE_EXTENDED:
      logentrysize=sizeof(PageEventExtended);
      PageEventExtended *pee=(PageEventExtended *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)pee, vmregisters);
      pee->fpudata=*fxsave;
      break;

    case PE_BASICSTACK:
      logentrysize=sizeof(PageEventBasicWithStack);
      PageEventBasicWithStack *pebws=(PageEventBasicWithStack *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)pebws, vmregisters);
      saveStack(currentcpuinfo, pebws->stack);
      break;

    case PE_EXTENDEDSTACK:
      logentrysize=sizeof(PageEventExtendedWithStack);
      PageEventExtendedWithStack *peews=(PageEventExtendedWithStack *)((QWORD)(liststart)+currentEntryNr*logentrysize);
      fillPageEventBasic((PageEventBasic*)peews, vmregisters);
      saveStack(currentcpuinfo, peews->stack);
      peews->fpudata=*fxsave;
      break;
  }
  if (datatype<0)
    return;
}



int ept_isWatchIDPerfectMatch(QWORD address, int ID)
{
  return ((eptWatchList[ID].Active) &&
          (
             (address>=eptWatchList[ID].PhysicalAddress) &&
             (address<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size)
           )
          );
}

int ept_isWatchIDMatch(QWORD address, int ID)
/*
 * pre: address is already page aligned
 */
{
  return ((eptWatchList[ID].Active) && ((eptWatchList[ID].PhysicalAddress & 0xfffffffffffff000ULL) == address));
}

int ept_getWatchID(QWORD address)
/*
 * returns -1 if not in a page being watched
 * Note that there can be multiple active on the same page
 */
{
  int i;
  //sendstringf("ept_getWatchID(%6)\n", address);
  address=address & 0xfffffffffffff000ULL;
  for (i=0; i<eptWatchListPos; i++)
    if (ept_isWatchIDMatch(address, i))
      return i;

  return -1;
}




BOOL ept_handleWatchEvent(pcpuinfo currentcpuinfo, VMRegisters *registers, PFXSAVE64 fxsave, QWORD PhysicalAddress)
//Used by Intel and AMD
{
  EPT_VIOLATION_INFO evi;
  NP_VIOLATION_INFO nvi;

  int ID;
  int logentrysize;
  int i;

  if (eptWatchListPos==0)
    return FALSE;

  if (isAMD)
  {


    nvi.ErrorCode=currentcpuinfo->vmcb->EXITINFO1;
    if (nvi.ID)
    {
      //instruction fetch.  Apparently, PA is not exact and on a 16 byte radius or worse
      sendstringf("ept_handleWatchEvent execute (ID) on AMD.  RIP=%6 PA=%6\n", currentcpuinfo->vmcb->RIP, PhysicalAddress);

      PhysicalAddress=(PhysicalAddress & 0xfffffffffffff000ULL) | (currentcpuinfo->vmcb->RIP & 0xfff);

      sendstringf("changed PhysicalAddress to %6\n", PhysicalAddress);



    }
  }
  else
  {
    evi.ExitQualification=vmread(vm_exit_qualification);

  }

  csEnter(&eptWatchListCS);

  ID=ept_getWatchID(PhysicalAddress);


  if (ID==-1)
  {
    csLeave(&eptWatchListCS);
    return FALSE;
  }

  if (isAMD)
    lastSeenEPTWatch.data=nvi.ErrorCode;

  else
    lastSeenEPTWatch.data=evi.ExitQualification;

  lastSeenEPTWatch.physicalAddress=PhysicalAddress;
  lastSeenEPTWatch.initialID=ID;


  QWORD RIP;
  QWORD RSP;

  sendstring("EPT/NP event and there is a watchlist entry\n");
  sendstringf("ept_getWatchID returned %d\n", ID);


  if (isAMD)
  {
    RIP=currentcpuinfo->vmcb->RIP;
    RSP=currentcpuinfo->vmcb->RSP;
  }
  else
  {
    RIP=vmread(vm_guest_rip);
    RSP=vmread(vm_guest_rsp);
  }

  lastSeenEPTWatch.skipped=-1;
  lastSeenEPTWatch.rip=RIP;

  QWORD PhysicalAddressBase=PhysicalAddress & 0xfffffffffffff000ULL;



  //nosendchar[getAPICID()]=0;
  sendstringf("Handling something that resembles watch ID %d\n", ID);


  //figure out which access it is really (in case of multiple on the same page)

  for (i=ID; i<eptWatchListPos; i++)
  {
    if (ept_isWatchIDMatch(PhysicalAddressBase, i))
    {
      if (eptWatchList[ID].Type==EPTW_WRITE)
      {
        //must be a write operation error
        if (((!isAMD) && (evi.W) && (evi.WasWritable==0)) || (isAMD && nvi.W))  //write operation and writable was 0
        {
          ID=i;

          if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
            break;
        }
      }
      else if (eptWatchList[ID].Type==EPTW_READWRITE)
      {
        //must be a read or write operation
        if ((isAMD && nvi.P==0) || ((!isAMD) && (((evi.W) && (evi.WasWritable==0)) || ((evi.R) && (evi.WasReadable==0)))) ) //write operation and writable was 0 or read and readable was 0
        {
          ID=i;
          if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
            break;
        }
      }
      else
      {
          if ((isAMD && nvi.ID) || ((!isAMD) && (evi.X) && (evi.WasExecutable==0))) //execute operation and executable was 0
          {
            ID=i;

            if (ept_isWatchIDPerfectMatch(PhysicalAddress, i))
              break;
          }
      }
    }
  }


  //nosendchar[getAPICID()]=0;

  lastSeenEPTWatch.actualID=ID;
  sendstringf("%d: handling watch ID %d\n", currentcpuinfo->cpunr, ID);
  sendstringf("%d: RIP=%6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RIP);


  //todo: release the eptWatchListCS and obtain only the log

  //ID is now set to the most logical watch(usually there is no conflicts, and even if there is, no biggie. But still)

  PPTE_PAE npte;
  PEPT_PTE epte;

  lastSeenEPTWatch.cacheIssue=0;
  if (isAMD)
  {
    npte=(PPTE_PAE)currentcpuinfo->eptWatchList[ID];
    if ((npte->EXB==0) && (npte->P) && (npte->RW))
    {
      sendstringf("This entry was already marked with full access (check caches) (AMD)\n");
      lastSeenEPTWatch.cacheIssue=1;
    }
  }
  else
  {
    epte=currentcpuinfo->eptWatchList[ID];
    if ((epte->XA) && (epte->RA) && (epte->WA))
    {
      sendstringf("This entry was already marked with full access (check caches)\n");
      lastSeenEPTWatch.cacheIssue=1;
    }
  }


  if ((eptWatchList[ID].Options & EPTO_DBVMBP) && (PhysicalAddress>=eptWatchList[ID].PhysicalAddress) && (PhysicalAddress<eptWatchList[ID].PhysicalAddress+eptWatchList[ID].Size))
  {
    nosendchar[getAPICID()]=0;
    sendstringf("%d: EPTO_DBVMBP hit (RIP=%6)\n", currentcpuinfo->cpunr, isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip));
    //This is the specific address that was being requested
    //if the current state has interrupts disabled or masked (cr8<>0) then skip (todo: step until it is)

    DWORD CR8=getCR8();
    RFLAGS flags;
    flags.value=isAMD?currentcpuinfo->vmcb->RFLAGS:vmread(vm_guest_rflags);
    int is;
    int canBreak=(CR8==0) && (flags.IF); //interruptable with no mask (on windows called passive mode)

    if (isAMD)
    {
      sendstringf("CR8=%6 IF=%d RF=%d\n", CR8,fl