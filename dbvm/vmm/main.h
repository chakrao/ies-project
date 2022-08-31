#ifndef MAIN_H_
#define MAIN_H_

#include "common.h"
#include "vmmhelper.h"



void reboot(int skipAPTermination);
void apentryvmx();


void startvmx(pcpuinfo currentcpuinfo);
void CheckCRCValues(void);

extern void vmcall_amd(void);
extern void vmcall_intel(void);

extern void *vmcall_instr; //holds a pointer to either vmcall_amd or vmcall_intel
extern int vmcalltest_asm(void);
extern int vmcall_setintredirects(void);
extern QWORD _vmcall(void* data);

extern void _pause(void);
extern UINT64 _vmread(ULONG index);
extern int _vmread2(ULONG index, UINT64 *result);
extern void _vmwrite(ULONG index,UINT64 value);
extern int _vmwrite2(ULONG index, UINT64 result);
extern int _vmclear(unsigned long long address);
extern int _vmptrld(unsigned long long address);
extern int _vmxon(unsigned long long address);
extern int _vmlaunch(void);
extern void _vmresume(void);
extern void _vmxoff(void);
extern void cLIDT(void *idtloader);
extern unsigned long long readMSR(ULONG msr);
extern void writeMSR(ULONG msr, UINT64 newvalue);
extern void _xsetbv(ULONG xcr, UINT64 value);
extern int stopautomation(void);
extern int hascpuid(void);
extern UINT64 getCR0(void);
extern UINT64 getCR2(void);
extern UINT64 getCR3(void);
extern UINT64 getCR4(void);
extern UINT64 getCR8(void);
extern UINT64 getDR0(void);
extern UINT64 setDR0(UINT64 newdr0);
extern UINT64 getDR1(void);
extern UINT64 setDR1(UINT64 newdr0);
extern UINT64 getDR2(void);
extern UINT64 setDR2(UINT64 newdr0);
extern UINT64 getDR3(void);
extern UINT64 setDR3(UINT64 newdr0);
extern UINT64 getDR6(void);
extern UINT64 setDR6(UINT64 newdr6);
extern UINT64 getDR7(void);
extern UINT64 setDR7(UINT64 newdr7);
extern void setIDT(UINT64 base, WORD size);
extern void setGDT(UINT64 base, WORD size);
extern UINT64 getIDTbase(void);
extern UINT64 getGDTbase(void);
extern WORD getIDTsize(void);
extern WORD getGDTsize(void);

//minor mistake fix and I hate renaming the function
#define getGDTlimit getGDTsize

extern UINT64 getRFLAGS(void);
extern void setRFLAGS(UINT64 rflags);
extern void loadTaskRegister(ULONG selector);
extern WORD getTaskRegister(void);
extern ULONG setCR0(UINT64 newcr0);
exter