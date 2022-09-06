
#include "main.h"
#include "vmpaging.h"
#include "vmmhelper.h"
#include "vmeventhandler.h"
#include "osspecific.h"
#include "common.h"
#include "mm.h"
#include "vmcall.h"
#include "msrnames.h"
#include "ultimap.h"
#include "vmxsetup.h"
#include "multicore.h"
#include "apic.h"

#include "vbe3.h"
#include "psod32.h"
#include "eptstructs.h"
#include "epthandler.h"
#include "displaydebug.h"


//#pragma GCC push_options
//#pragma GCC optimize ("O0")

void psod(void)
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

  int x=call32bit((DWORD)(QWORD)PSOD32BitHandler);

  //tell other cpu's to stop

  sendstringf("call32bit((DWORD)PSOD32BitHandler) returned with %d\n", x);

  //disable PIC interrupts

 // jtagbp();


  //VBE enables interrupts..
  BYTE old21=inportb(0x21);
  BYTE olda1=inportb(0xa1);
  QWORD oldcr8=getCR8();
  outportb(0x21,0xff);
  outportb(0xa1,0xff);
  setCR8(0xf);

  if (initializeVBE3())
  {
    int i;
    WORD *vm;
    VBE_ControllerInfo ci;
    zeromemory(&ci, sizeof(ci));
    ci.VbeSignature=0x32454256;
    if (VBE_GetControllerInfo(&ci))
    {
      sendstringf("VBE_GetControllerInfo returned success\n");
      sendstringf("  ci.Capabilities=%8\n", ci.Capabilities);

      unsigned char *s=VBEPtrToAddress(ci.OemStringPtr);
      if (s)
      {
        sendstring("  ci.OemString=");
        sendstring((char *)s);
        sendstring("\n");
      }


      sendstringf("  VideoModePointer at %6\n", VBEPtrToAddress(ci.VideoModePtr));
      vm=VBEPtrToAddress(ci.VideoModePtr);


      int bestmode=0;
      //find a mode I like
      for (i=0; vm[i]!=0xffff; i++)
      {
        VBE_ModeInfo mi;
        sendstringf("    %x : \n", vm[i]);

        if (VBE_GetModeInfo(vm[i], &mi))
        {
          sendstringf("      %d x %d x %d (mode=%x PA=%8)\n", mi.XResolution, mi.YResolution, mi.BitsPerPixel, mi.ModeAttributes, mi.PhysBasePtr);

          if ((mi.XResolution>600) && (mi.XResolution<800) && (mi.YResolution>400) && (mi.YResolution<600) )
          {
            if (bestmode)
            {
              VBE_ModeInfo other;
              VBE_GetModeInfo(bestmode, &other);
              if (mi.BitsPerPixel>other.BitsPerPixel) //better color
                bestmode=vm[i];
            }
            else
              bestmode=vm[i];
          }
        }
        else
          sendstringf("      No mode info\n");
      }
      VBE_CRTCInfo crti;
      zeromemory(&crti, sizeof(crti));

      int statesize=VBE_GetStateStoreSize(); //not working
      void *state;

      if (statesize)
      {
        sendstringf("statesize=%d bytes\n", statesize);
        state=malloc(statesize);

        VBE_SaveState(state, statesize);
      }
      else
        sendstringf("No statesize\n");

      sendstringf("Picked mode %x\n", bestmode);

      if (VBE_SetMode(bestmode | (1<<14),&crti))
      {
        VBE_ModeInfo mi;
        VBE_GetModeInfo(bestmode, &mi);


        //blank the other pages
        for (i=1; i<mi.LinNumberOfImagePages; i++)
        {
          VBE_SetDrawPage(i);
          VBE_SetPenColor(0xffff00);
          VBE_DrawBox(0,0,mi.XResolution-1, mi.YResolution-1);
        }

        VBE_SetDrawPage(0);

        VBE_SetPenColor(0x00ffff);
        VBE_DrawBox(0,0,mi.XResolution-1, mi.YResolution-1);

        VBE_SetPenColor(0x0000ff);
        VBE_DrawBox(20,40,mi.XResolution-1-20, mi.YResolution-1-40);

        VBE_ResetStart();


        /*
        while (1)
          _pause();
          */

      }

      if (statesize)
      {
        VBE_RestoreState(state, statesize);
        displayline("Does this still work?\n");
      }
    }

  }

  __asm("cli");
  outportb(0x21,old21);
  outportb(0xa1,olda1);
  setCR8(oldcr8);
}

QWORD readMSRSafe(DWORD msr)
{
  QWORD r;
  try
  {
    r=readMSR(msr);
  }
  except
  {
    r=0;
  }
  tryend

  return r;
}

void writeMSRSafe(DWORD msr, QWORD value)
{
  try
  {
    writeMSR(msr, value);
  }
  except
  {