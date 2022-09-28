#ifndef VMPAGING_H_
#define VMPAGING_H_

#include "vmmhelper.h"


void *nonpagedEmulationPagedir; //nonpaged memory emulation pagedir (pml4)
void *nonpagedEmulationPagedir32BitPAE;
void *nonpagedEmulationPagedir32Bit;

QWORD nonpaged