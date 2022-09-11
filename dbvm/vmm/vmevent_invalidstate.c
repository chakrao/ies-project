#include "vmevent_invalidstate.h"
#include "realmodeemu.h"
#include "pmodeemu.h"
#include "vmreadwrite.h"
#include "mm.h"
#include "vmxsetup.h"

int fixInterruptabilityState(void)
{
  DWORD is=vmread(vm_guest_interruptability_state);
  DWORD originalis=is;
  RFLAGS guestrflags;
  VMEntry_interruption_information entryinterrupt;
  guestrflags.value=vmread(vm_guest_rflags);

  int handled=0;


  is=is & 0x1f; //remove reserved bits

  if ((guestrflags.IF==0) && (is & (1<<0))) //block by sti may not be enabled when IF is 0
    is=is & 0x1e; //disable block by sti



  if ((is & 3)==3)
    //both block by STI and block by mov ss are enabled
    is=is & 0x1e; //disable block by sti

  entryinterrupt.interruption_information=vmread(vm_entry_interruptioninfo);
  if (entryinterrupt.valid)
  {
    if (entryinterrupt.type==0) //external interrupt entry must have the both sti and ss to 0
      is = is & 0x1c;

    if (entryinterrupt.type==2) //nmi
      is = is & 0x1d; //disable blick by ss
  }


  vmwrite(vm_guest_interruptability_state, is);

  return is!=originalis;
}


VMSTATUS handleInvalidEntryState(pcpuinfo currentcpuinfo,VMRegisters *vmregisters)
{
  RFLAGS guestrflags;
  guestrflags.value=vmread(vm_guest_rflags);

  outportb(0x80,0xce);

#ifdef DELAYEDSERIAL
  useserial=1;
#endif

  outportb(0x80,0xc0);
  enableserial();

  nosendchar[getAPICID()]=0;

 // outportb(0x80,0xc1);

  sendstring("Handling invalid entry state\n\r");
  //fix interruptability state (common bug when emulating and fixing it won't cause a problem)

  //check if it must have BT debug state

 // outportb(0x80,0xc2);

 // while (1) outportb(0x80,0xd4); //todo: remove on release


  if (((guestrflags.TF) || (vmread(vm_guest_IA32_DEBUGCTL) & (1<<1))) && ((vmread(vm_pending_debug_exceptions) & (1<<14))==0))
  {
    //TF or BTF is set
    /*
    while (1)
      outportb(0x80,0xc3);*/

    sendstring("Setting pending debug exception\n\r");
    vmwrite(vm_pending_debug_exceptions, vmread(vm_pending_debug_exceptions) | (1<<14) );
    return VM_OK;
  }

  if (fixInterruptabilityState())
  {
    /*
    while (1)
      outportb(0x80,0xc4);*/


    return VM_OK;
  }

  if (!IS64BITCODE(currentcpuinfo))
  {
    if (vmread(vm_guest_rip)>0xffffffff)
    {
      vmwrite(vm_guest_rip,vmread(vm_guest_rip)&0xffffffff);
      return VM_OK;
    }
  }


  if (ISREALMODE(currentcpuinfo))
  {
    int result;



    outportb(0x80,0xc5);



    sendstring("Inside realmode. Trying to emulate instructions\n\r");
    result=emulateRealMode(currentcpuinfo, vmregisters)==0;

    sendstringf("emulateRealMode(...) returned %d\n\r",result);

    if (result==TRUE)
    {
      /*
      while (1)
        outportb(0x80,0xc6);*/
      sendstring("emulation was handled properly\n");

      return VM_OK; //handled at least one instruction
    }
    else
      sendstring("emulation was a total failure. Not one instruction got emulated. Trying to fix the state");


    //emulateRealMode failed
    if (ISREALMODE(currentcpuinfo)) //still realmode ? (most likely, but possible it isn't anymore once enough has been emulated)
    {
      if (hasUnrestrictedSupport)
      {






        QWORD cr0=vmread(vm_guest_cr0);
        if (cr0 & CR0_PG)
        {
          sendstringf("paging on in realmode\n");
          while (1) outportb(0x80,0xe0); //paging on in realmode
        }

        if (cr0 & CR0_PE)
        {
          sendstringf("protected mode in realmode\n");
          while (1) outportb(0x80,0xe0); //protected mode in realmode
        }

        if (vmread(vm_entry_controls) & VMENTRYC_IA32E_MODE_GUEST)
        {
          sendstringf("ia32e mode entry in realmode\n");
          while (1) outportb(0x80,0xe1); //ia32e mode entry in realmode
        }

        RFLAGS rflags;
        rflags.value=(QWORD)vmread(vm_guest_rflags);
        if (rflags.VM) while (1) outportb(0x80,0xe2);
        if (rflags.VIF) while (1) outportb(0x80,0xe2);
        if (rflags.VIP) while (1) outportb(0x80,0xe2);
        if (vmread(vm_guest_tr) & (1 << 2)) while (1) outportb(0x80,0xe3); //tri.ti flag must be 0

        Access_Rights ar;
        Access_Rights ss;
        QWORD limit;
        ss.AccessRights=vmread(vm_guest_ss_access_rights);
        ar.AccessRights=vmread(vm_guest_ldtr_access_rights);
        if ((!ar.unusable) && (vmread(vm_guest_ldtr) & (1<<2))) while (1) outportb(0x80,0xe4); //if ldtr is usable, TI flag must be 0

        ar.AccessRights=vmread(vm_guest_cs_access_rights);
        switch (ar.Segment_type)
        {
          case 3:
          case 9:
          case 11:
          case 13:
          case 15:
            break;

          default:
            while (1) outportb(0x80,0xe5); //cs access right type must be 3,9,11,13 or 15
        }
        if (ar.S==0) while (1) outportb(0x80,0xe8);
        if (ar.P==0) while (1) outportb(0x80,0xed);
        if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xee);
        if ((ar.Segment_type==3) && (ar.DPL!=0)) while (1) outportb(0x80,0xe9);
        if (((ar.Segment_type==9) || (ar.Segment_type==11)) && (ar.DPL!=ss.DPL)) while (1) outportb(0x80,0xea);
        if (((ar.Segment_type==13) || (ar.Segment_type==15)) && (ar.DPL>ss.DPL)) while (1) outportb(0x80,0xeb);

        limit=(QWORD)vmread(vm_guest_cs_limit);
        if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xef);
        if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf0);



        if (!ss.unusable)
        {
          switch (ss.Segment_type)
          {
            case 3:
            case 7:
              break;

            default:
              while (1) outportb(0x80,0xe6); //ss access right type must be 3 or 7
          }

          if (ss.S==0) while (1) outportb(0x80,0xe8);
          if (ss.P==0) while (1) outportb(0x80,0xed);
          if (ss.reserved || ss.reserved_2) while (1) outportb(0x80,0xee);
          if (ss.DPL!=0) while (1) outportb(0x80,0xec);

          limit=(QWORD)vmread(vm_guest_ss_limit);
          if (((limit & 0xfff)!=0xfff) && (ss.G!=0)) while (1) outportb(0x80,0xef);
          if ((limit & 0xFFF00000) && (ss.G==0)) while (1) outportb(0x80,0xf0);


        }

        ar.AccessRights=vmread(vm_guest_ds_access_rights);
        if (!ar.unusable)
        {
          if ((ar.Segment_type & 1)==0) while (1) outportb(0x80,0xe7); //segment register was usable but not accessed
          if ((ar.Segment_type & (1<<3)) && (((ar.Segment_type & (1<<1))==0))) while (1) outportb(0x80,0xe7); //code segment but type is unreadable
          if (ar.S==0) while (1) outportb(0x80,0xe8);
          if (ar.P==0) while (1) outportb(0x80,0xed);
          if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xee);

          limit=(QWORD)vmread(vm_guest_ds_limit);
          if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xef);
          if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf0);
        }

        ar.AccessRights=vmread(vm_guest_es_access_rights);
        if (!ar.unusable)
        {
          if ((ar.Segment_type & 1)==0) while (1) outportb(0x80,0xe7); //segment register was usable but not accessed
          if ((ar.Segment_type & (1<<3)) && (((ar.Segment_type & (1<<1))==0))) while (1) outportb(0x80,0xe7); //code segment but type is unreadable
          if (ar.S==0) while (1) outportb(0x80,0xe8);
          if (ar.P==0) while (1) outportb(0x80,0xed);
          if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xee);

          limit=(QWORD)vmread(vm_guest_es_limit);
          if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xef);
          if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf0);
        }

        ar.AccessRights=vmread(vm_guest_fs_access_rights);
        if (!ar.unusable)
        {
          if ((ar.Segment_type & 1)==0) while (1) outportb(0x80,0xe7); //segment register was usable but not accessed
          if ((ar.Segment_type & (1<<3)) && (((ar.Segment_type & (1<<1))==0))) while (1) outportb(0x80,0xe7); //code segment but type is unreadable
          if (ar.S==0) while (1) outportb(0x80,0xe8);
          if (ar.P==0) while (1) outportb(0x80,0xed);
          if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xee);

          limit=(QWORD)vmread(vm_guest_fs_limit);
          if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xef);
          if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf0);
        }

        ar.AccessRights=vmread(vm_guest_gs_access_rights);
        if (!ar.unusable)
        {
          if ((ar.Segment_type & 1)==0) while (1) outportb(0x80,0xe7); //segment register was usable but not accessed
          if ((ar.Segment_type & (1<<3)) && (((ar.Segment_type & (1<<1))==0))) while (1) outportb(0x80,0xe7); //code segment but type is unreadable
          if (ar.S==0) while (1) outportb(0x80,0xe8);
          if (ar.P==0) while (1) outportb(0x80,0xed);
          if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xee);

          limit=(QWORD)vmread(vm_guest_gs_limit);
          if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xef);
          if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf0);
        }


        ar.AccessRights=vmread(vm_guest_tr_access_rights);
        if ((ar.Segment_type!=3) && (ar.Segment_type!=11)) while (1) outportb(0x80,0xf1);
        if (ar.S!=0) while (1) outportb(0x80,0xf2);
        if (ar.P!=1) while (1) outportb(0x80,0xf3);
        if (ar.reserved || ar.reserved_2) while (1) outportb(0x80,0xf4);
        limit=(QWORD)vmread(vm_guest_tr_limit);
        if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xf5);
        if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xf6);
        if (ar.unusable)  while (1) outportb(0x80,0xf7);

        ar.AccessRights=vmread(vm_guest_ldtr_access_rights);
        if (ar.unusable==0)
        {
          if (ar.Segment_type!=2) while (1) outportb(0x80,0xf8);
          if (ar.S!=0) while (1) outportb(0x80,0xf9);
          if (ar.P!=1) while (1) outportb(0x80,0xfa);
          limit=(QWORD)vmread(vm_guest_ldtr_access_rights);
          if (((limit & 0xfff)!=0xfff) && (ar.G!=0)) while (1) outportb(0x80,0xfb);
          if ((limit & 0xFFF00000) && (ar.G==0)) while (1) outportb(0x80,0xfc);
        }

        if (vmread(vm_guest_idt_limit)>65536) while (1) outportb(0x80,0xfd);

        if (vmread(vm_guest_rip)>(QWORD)0xffffffff) while (1) outportb(0x80,0xfe);

        QWORD rf=vmread(vm_guest_rflags);
        if (((rf & (1<<1))==0) || (rf & (1<<3)) || (rf & (1<<5)) || (rf & (1<<15)) || (rf & 0xFFFFFFFFFFC00000ULL) )  while (1) outportb(0x80,0xff);

        int activitystate=vmread(vm_guest_activity_state);



        int interruptability_state=vmread(vm_guest_interruptability_state);
        if (activitystate>3) while (1) outportb(0x80,0x10);
        if ((activitystate==1) && (ss.DPL!=0)) while (1) outportb(0x80,0x11);  //hlt and dpl>0


        if ((interruptability_state & 3) && (activitystate!=0)) while (1) outportb(0x80,0x12); //not active and block by sti or ss

        VMEntry_interruption_information ii;
        ii.interruption_information=vmread(vm_entry_interruptioninfo);
        if (ii.valid) while (1) outportb(0x80,0x13); //not gonna deal with this right now

        if (interruptability_state & 0xFFFFFFE0) while (1) outportb(0x80,0x14); //invalid bits set
        if ((interruptability_state & 3)==3) while (1) outportb(0x80,0x15); //blocking by sti AND ss

        if ((rflags.IF==0) && (interruptability_state & 1))  while (1) outportb(0x80,0x16); //block by sti not possible

        if (interruptability_state & (1<<2)) while (1) outportb(0x80,0x17); //no blocking by smi

        cr0=(QWORD)vmread(vm_guest_cr0);
        QWORD cr0fixed0=(QWORD)readMSR(0x486);
        QWORD cr0fixed1=(QWORD)readMSR(0x487);

        cr0fixed0&=(QWORD)0xFFFFFFFF7FFFFFFE;//unrestricted so pg and pe can be 0
        if ((cr0 & cr0fixed0)!=cr0fixed0) while (1) outportb(0x80,0x18);
        if ((cr0 & cr0fixed1)!=cr0) while (1) outportb(0x80,0x19);

        if ((cr0 & (1<<29)) && ((cr0 & (1<<30))==0))  while (1) outportb(0x80,0x19); //NW==1, CD==0


        QWORD cr4=(QWORD)vmread(vm_guest_cr4);
        QWORD cr4fixed0=(QWORD)readMSR(0x488);
        QWORD cr4fixed1=(QWORD)readMSR(0x489);

        if ((cr4 & cr4fixed0)!=cr4fixed0) while (1) outportb(0x80,0x20);
        if ((cr4 & cr4fixed1)!=cr4) while (1) outportb(0x80,0x21);

