/*
mm.c:
This will manage the memory allocs and free's
Just used for basic initialization allocation, frees shouldn't happen too often

*/

#include "mm.h"
#include "main.h"
#include "multicore.h"
#include "common.h"
#include "vmmhelper.h"
#include "displaydebug.h"

//#define sendstringf(s,x...)
//#define sendstring(s)


/*
 * new mm
 *
 */

#define BASE_VIRTUAL_ADDRESS 0x1000000000ULL

//MAPPEDMEMORY is the range of virtual memory allocated for individual CPU threads mapping
#define MAPPEDMEMORY 0x08000000000ULL

//GLOBALMAPPEDMEMORY is the virtual memory allocated for the whole system for mapping
#define GLOBALMAPPEDMEMORY 0x07000000000ULL

//for virtual memory allocs
criticalSection AllocCS={.name="AllocCS", .debuglevel=2};
criticalSection GlobalMapCS={.name="GlobalMapCS", .debuglevel=2};


PageAllocationInfo *AllocationInfoList=(PageAllocationInfo *)BASE_VIRTUAL_ADDRESS;
int PhysicalPageListSize=1; //size in pages
int PhysicalPageListMaxSize=64; //if the size goes over this, reallocate the list

//for mapping

//each cpu gets one of these
/*
char *MappedAllocationInfoList=NULL;  //no need to bitfuck here, there's not going to be THAT much each byte is 1 page
int MappedAllocationInfoListSize=0; //size in pages
int MappedAllocationInfoListMax=0; //if the size goes over this, reallocate the list
*/

//host cr3 is mapped at 0xFFFFFF8000000000, which causes the following mappings:
PPDPTE_PAE        pml4table=(PPDPTE_PAE)0xfffffffffffff000ULL;
PPDPTE_PAE pagedirptrtables=(PPDPTE_PAE)0xffffffffffe00000ULL;
PPDE_PAE      pagedirtables=  (PPDE_PAE)0xffffffffc0000000ULL;
PPTE_PAE         pagetables=  (PPTE_PAE)0xffffff8000000000ULL;



QWORD FirstFreeAddress;

unsigned char MAXPHYADDR=0; //number of bits a physical address can be made up of
QWORD MAXPHYADDRMASK=  0x0000000fffffffffULL; //mask to AND with a physical address to strip invalid bits
QWORD MAXPHYADDRMASKPB=0x0000000ffffff000ULL; //same as MAXPHYADDRMASK but also aligns it to a page boundary


//alloc(not 2) keeps a list of allocs and their sizes.  This linked list (allocated using alloc2) is used to keep track of those allocs. Sorted by base
typedef struct
{
  unsigned long long base;
  ULONG size;
  /*
   *if size=0 then this block has actually been freed and can be reused (for an address that falls between the previous and next)
   *This reduces shifting operations when an item in the center gets freed and speeds up lots of small alloc/free routine
   *
   */
} MemlistItem2, *PMemlistItem2;


MemlistItem2 *AllocList=NULL;
int AllocListMax=0;
int AllocListPos=0;

UINT64 TotalAvailable;


void* contiguousMemory; //alloc once memory. Doesn't allow free yet
int contiguousMemoryPagesFree;


void free2(void *address, unsigned int size);


void* allocateContiguousMemory(int pagecount)
{
  void *result=NULL;
  csEnter(&AllocCS);

  sendstringf("allocateContiguousMemory(%d)\n", pagecount);
  sendstringf("contiguousMemoryPagesFree=%d\n", contiguousMemoryPagesFree);
  sendstringf("contiguousMemory=%p\n", contiguousMemory);


  if (contiguousMemoryPagesFree>=pagecount)
  {
    result=contiguousMemory;

    (*(QWORD*)&contiguousMemory)+=4096*pagecount;
    contiguousMemoryPagesFree-=pagecount;
  }
  else
  {

    if (contiguousMemory==NULL)
    {
      result=malloc2(pagecount*4096); //assume that on systems without contiguousMemory it is already mapped as one contiguous block to begin with

      QWORD a=VirtualToPhysical(result);
      int i;
      for (i=1;i<pagecount;i++)
      {
        QWORD b=VirtualToPhysical((void*)((QWORD)result)+4096);

        if (b!=a+4096)
        {
          nosendchar[getAPICID()]=0;
          sendstringf("Failure allocating contiguous memory (not contiguous)\n");
          while (1);
        }
        a=b;
      }
    }
    else
    {
      nosendchar[getAPICID()]=0;
      sendstringf("contiguousMemoryPagesFree<pagecount");
      while (1)
      {
        sendstringf("contiguousMemoryPagesFree<pagecount");

        outportb(0x80,0x01);
        outportb(0x80,0x10);
      }
    }
  }



  csLeave(&AllocCS);

  return result;
}

/*
 * There was a time where the memory manager would free blocks it shouldn't. this is a test for that scenario)
void wtftest(void)
{
  pcpuinfo c=getcpuinfo();
  int i;

  for (i=0; i<c->eptWatchlistLength; i++)
  {
    if (c->eptWatchlist[i].Active)
    {
      EPT_PTE x;

      try
      {
        PEPTWatchEntry pe=c->eptWatchlist;
        EPTWatchEntry e=pe[i];
        _invlpg((QWORD)e.EPTEntry);
        x=*(e.EPTEntry);

        if (x.Accessed)
          sendstringf("Valid and accesses\n");
      }
      except
      {
        nosendchar[getAPICID()]=0;
        sendstringf("EPTEntry for a watch got invalidated\n");
        while (1);
      }
      tryend

    }
  }
}
*/

void* mapMemory(void *destination, void *source, int size)
/*
 * This function will setup paging at destination matching the source
 */
{
  //get the physical addresses of the source
  int i;
  int offset=(QWORD)source & 0xfff;
  int totalsize=size+offset;

  void *result=(void *)((QWORD)destination+offset);

  int pagecount=totalsize / 4096;
  if (size & 0xfff)
    pagecount++;

  //just being lazy atm, this can be optimized a lot
  for (i=0; i<pagecount; i++)
  {
    //VirtualToPhysical()
    //getOrAllocatePageTableEntryForAddress()
    PPDE_PAE destinationpte=getPageTableEntryForAddressEx(destination,1);
    PPDE_PAE sourcepte=getPageTableEntryForAddress(source);

    *