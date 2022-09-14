BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern vmm_entry
extern cinthandler
extern menu
extern memorylist
extern clearScreen
extern getAPICID



GLOBAL amain
GLOBAL vmmstart
GLOBAL pagedirlvl4
GLOBAL isAP
GLOBAL loadedOS
GLOBAL bootdisk
GLOBAL nakedcall
GLOBAL nextstack
GLOBAL _vmread
GLOBAL _vmwrite
GLOBAL _vmread2
GLOBAL _vmwrite2

GLOBAL _vmclear
GLOBAL _vmptrld
GLOBAL _vmxon
GLOBAL _vmxoff
GLOBAL _vmlaunch
GLOBAL _vmresume

GLOBAL getGDTbase
GLOBAL getIDTbase
GLOBAL getGDTsize
GLOBAL getIDTsize
GLOBAL setGDT
GLOBAL setIDT

GLOBAL vmmentrycount
GLOBAL initcs

GLOBAL extramemory
GLOBAL extramemorysize

GLOBAL contiguousmemoryPA
GLOBAL contiguousmemorysize

GLOBAL dbvmversion
GLOBAL exportlist

%define VMCALL db 0x0f, 0x01, 0xc1 ;vmcall

;everything here is in virtual memory, paging has already been setup properly

GLOBAL _start
_start:
amain:

jmp short afterinitvariables
times 16-($-$$) db 0x90 ;pad with nop's till a nice 16-byte alignment



loadedOS:           dq 0 ;physical address of the loadedOS section
vmmstart:           dq 0 ;physical address of virtual address 00400000 (obsoletish...)
pagedirlvl4:        dq 0 ;virtual address of the pml4 page (the memory after this page is free)
nextstack:          dq 0 ;start of stack for the next cpu
extramemory:        dq 0 ;physical address of a contiguous block of physical memory available to DBVM
extramemorysize:    dq 0 ;number of pages in extramemory
contiguousmemoryPA: dq 0 ;physical address of a contiguous block of physical memory available for device access
contiguousmemorysize:dq 0 ;number of pages left in contiguousmemoryPA
dbvmversion:        dq 11
exportlist:         dq 0
;uefibooted:         dq 0 ;if set it means this has to launch the AP cpu's as well

initcs: dd 0 ;critical section to block entering cpus.  Each CPU sets up the stack for the next CPU (so there will always be one too many)
vmmentrycount: dd 0  ;The number of times 0x00400000 has been executed (in short, the number of CPU's launched)

;lasttsc: dq 0

afterinitvariables:

lock add dword [vmmentrycount],1

initcs_trylock:
lo