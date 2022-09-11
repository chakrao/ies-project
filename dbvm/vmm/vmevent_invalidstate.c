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
    if (ISREALMODE(currentcpuinfo)) //still realmode ? (most likely, but possible it isn't anymor