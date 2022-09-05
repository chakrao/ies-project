/*
 * nphandler.c
 *
 *  Created on: Apr 29, 2020
 *      Author: eric
 */

#include "vmmhelper.h"
#include "vmxsetup.h"
#include "mm.h"
#include "nphandler.h"
#include "epthandler.h"
#include "main.h"
#include "common.h"

//guest NP is mapped at 0xFFFFFF0000000000, which causes the following mappings (unique per CPU)
PPML4             np_pml4table=     (PPML4)0xffffffffffffe000ULL;
PPDPTE_PAE np_pagedirptrtables=(PPDPTE_PAE)0xffffffffffc00000ULL;
PPDE_PAE      np_pagedirtables=  (PPDE_PAE)0xffffffff80000000ULL;
PPTE_PAE         np_pagetables=  (PPTE_PAE)0xffffff0000000000ULL;

void NPMode1CloakRestoreCallback(QWORD address UNUSED, CloakedPageData *data)
{
  pcpuinfo c=getcpuinfo();
  *(QWORD *)(data->npentry[c->cpunr])=data->PhysicalAddressData;
  data->npentry[c->cpunr]->P=1;
  data->npentry[c->cpunr]->RW=1;
  data->npentry[c->cpunr]->US=1;

  data->npentry[c->cpunr]->EXB=1; //back to non executable
}


void NPMode1CloakSetState(pcpuinfo currentcpuinfo, int state)
//1 means ALL pages except the current cloaked region are no-execute, 0 means ALL pages are executable again (and afterwards apply cloaks)
{
  //mark all other pages as no-execute
  QWORD BaseAddress=currentcpuinfo->NP_Cloak.ActiveRegion->PhysicalAddressExecutable;

  //sendstringf("NPMode1CloakSetState for address %6 (for %x:%6)\n", BaseAddress, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->cs_selector);
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  VirtualAddressToIndexes(BaseAddress, &pml4index, &pagedirptrindex, &pagedirindex, &pagetableindex);

  UINT64 PageDirPtrIndexFull=(pml4index << 9)+pagedirptrindex;
  UINT64 PageDirIndexFull=(PageDirPtrIndexFull << 9)+pagedirindex;
  UINT64 PageTableIndexFull=(PageDirIndexFull << 9)+pagetableindex;

  //sendstringf("PageDirPtrIndexFull=%d\n", PageDirPtrIndexFull);
  //sendstringf("PageDirIndexFull=%d\n", PageDirIndexFull);
  //sendstringf("PageTableIndexFull=%d\n", PageTableIndexFull);

  csEnter(&currentcpuinfo->EPTPML4CS);


  //No PS check, if cloak is used assume that the page is mapped as a 4KB page

  int i;
  for (i=0; i<512; i++)
  {
    //PML4:
    if (np_pml4table[i].P)
    {
      if (i==pml4index)
      {
        QWORD i2;
        QWORD i2s=(QWORD)pml4index << 9;

        for (i2=i2s; i2<i2s+512; i2++)
        {
          //PageDirPtr
          if (np_pagedirptrtables[i2].P)
          {
            if (i2==PageDirPtrIndexFull)
            {
              QWORD i3;
              QWORD i3s=((QWORD)PageDirPtrIndexFull << 9);
              for (i3=i3s; i3<i3s+512; i3++)
              {
                //PageDir
                if (np_pagedirtables[i3].P)
                {
                  if (i3==PageDirIndexFull)
                  {
                    QWORD i4;
                    QWORD i4s=((QWORD)PageDirIndexFull << 9);
                    for (i4=i4s; i4<i4s+512; i4++)
                    {
                      //Page table
                      if (np_pagetables[i4].P)
                      {
                        if (i4==PageTableIndexFull)
                          np_pagetables[i4].EXB=0; //make this code executable, the code further down will mark it as non-execute if it has to
                        else
                          np_pagetables[i4].EXB=state;
                      }
                    }
                  }
                  else
                    np_pagedirtables[i3].EXB=state;
                }
              }
            }
            else
              np_pagedirptrtables[i2].EXB=state;
          }
        }
      }
      else
        np_pml4table[i].EXB=state;
    }

  }

  if (state==0)
  {
    //All memory is executable again. reprotect the cloaked regions

    if (CloakedPagesMap)
    {
      map_foreach(CloakedPagesMap, (MAPCALLBACK)NPMode1CloakRestoreCallback);
    }
    else
    {
      int i;
      for (i=0; i<CloakedPagesList->size; i++)
        NPMode1CloakRestoreCallback(CloakedPagesList->list[i].address, CloakedPagesList->list[i].data);
    }


  }


  if (((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF)
    currentcpuinfo->vmcb->INTERRUPT_SHADOW=1; //if slow, then execute at least one single instruction


  csLeave(&currentcpuinfo->EPTPML4CS);
}

QWORD NPMapPhysicalMemory(pcpuinfo currentcpuinfo, QWORD physicalAddress, int forcesmallpage)
/*
 * Maps the physical address into the NP map.
 * Returns the physical address of the NP entry describing this page
 */
{
  int pml4index;
  int pagedirptrindex;
  int pagedirindex;
  int pagetableindex;
  int swappedguesttables;
  int madepresent=0;
  VirtualAddressToIndexes(physicalAddress, &pml4index, &pagedirptrindex, &pagedirindex, &pag