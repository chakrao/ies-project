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

        ar.AccessRights=vmread(vm_guest_fs_access_right