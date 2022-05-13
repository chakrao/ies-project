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
          if (ChangeRegBPList[i].changereginfo.Flags.changeRCX) vmreg