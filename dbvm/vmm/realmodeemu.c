
/*
 * Basic emulation for the case where the system enters an invalid real mode situation
 * With luck enough is emulated till a valid state is entered for normal VM-x handling, if not, the state will be 'fixed', with all the trouble that brings
 */

#include "common.h"
#include "main.h"
#include "realmodeemu.h"
#include "vmmhelper.h"
#include "vmpaging.h"
#include "vmreadwrite.h"
#include "distorm.h"

#include "vmeventhandler.h"
#include "vmcall.h"

#ifndef DEBUG
#define sendstringf(s,x...)
#define sendstring(s)
#endif

ULONG handleMODRM(VMRegisters *vmregisters, PINSTRUCTIONDATA id);
int getOpperand(PINSTRUCTIONDATA id);
int setSegment(int segmentnr, WORD value);
UINT64 getOpperandValue(VMRegisters *vmregisters, int opperand, _registerType registerType);
int emulateRMinterrupt(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, int intnr);
int emulateHLT(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int getparityflag(unsigned char value);

int opcode_CALLE8(pcpuinfo currentcpuinfo, PINSTRUCTIONDATA id)
{
  int error;
  UINT64 pagefaultaddress;

  WORD newip;

  if (!id->opperandsize)
  {
    WORD *stack=(WORD *)mapVMmemory(currentcpuinfo, vmread(vm_guest_ss_base)+(vmread(vm_guest_rsp) & 0xffff)-2, 2, &error, &pagefaultaddress);
    signed short int offset=*(signed short int *)&id->instruction[id->size];
    sendstringf("%x:%x CALL near relative offset=%d\n\r",vmread(vm_guest_cs), vmread(vm_guest_rip), offset);
    id->size+=2;


    //push current eip+id->size
    stack[0]=vmread(vm_guest_rip)+id->size;

    vmwrite(vm_guest_rsp, (vmread(vm_guest_rsp) & 0xffff0000) + (WORD)(vmread(vm_guest_rsp)-2));


    //change eip
    newip=vmread(vm_guest_rip);
    newip+=id->size;
    newip+=offset;

    vmwrite(vm_guest_rip,newip);

    id->size=0;

    unmapVMmemory(stack,2);
    return 2; //handled, but don't change eip, I already did it
  }
  else