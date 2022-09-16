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
lock bts dword [initcs],0 ;put the value of bit nr 0 into CF, and then set it to 1
jnc launchcpu  ;if it was 0, launch the cpu

;it's locked, wait
initcs_waitloop:
pause
cmp dword [initcs],0
je  initcs_trylock ;it is 0, try to lock
jmp initcs_waitloop


launchcpu:

;db 0xf1 ;debug test
mov rax,[nextstack] ;setup the stack
mov rsp,rax

and rsp,-0x10; //,0xfffffffffffffff0 ;should not be needed as it 'should' be aligned to begin with


mov rax,cr4
or rax,0x200 ;enable fxsave
mov cr4,rax

cmp qword [loadedOS],0
jne afterbootvarcollection

cmp byte [isAP],0
jne afterbootvarcollection



;save the 64-bit regs (mainly for the upper bits)

mov [0x7100],rax
mov [0x7108],rbx
mov [0x7110],rcx
mov [0x7118],rdx
mov [0x7120],rsi
mov [0x7128],rdi
mov [0x7130],rbp
mov [0x7138],r8
mov [0x7140],r9
mov [0x7148],r10
mov [0x7150],r11
mov [0x7158],r12
mov [0x7160],r13
mov [0x7168],r14
mov [0x7170],r15



afterbootvarcollection:
call vmm_entry

vmm_entry_exit:
jmp vmm_entry_exit

dq 0
dq 0


align 16,db 0
isAP:              	dd 0
bootdisk:           dd 0



global vmcall_amd
vmcall_amd:
  vmmcall
  ret

global vmcall_intel
vmcall_intel:
  vmcall
  ret

global vmcall_instr
vmcall_instr: dq vmcall_intel

global vmcalltest_asm
vmcalltest_asm:
  sub rsp,8
  sub rsp,12

  mov dword [rsp],12
  mov dword [rsp+4],0xfedcba98
  mov dword [rsp+8],0

  ;xchg bx,bx
  mov rax,rsp
  mov rdx,0x76543210
  call [vmcall_instr]

  add rsp,8+12
  ret


extern Password1
extern Password3

global _vmcall
_vmcall:
  sub rsp,8
  mov rax,rdi  ;data
  mov rdx,[Password1]  ;password1
  mov rcx,[Password3]
  call [vmcall_instr]
  add rsp,8
  ret




global vmcall_setintredirects
vmcall_setintredirects:
;also int3 and int14
  sub rsp,8
  sub rsp,0x20

  mov dword [rsp],0x1c ;size of struct
  mov dword [rsp+4],0xfedcba98 ;p2
  mov dword [rsp+8],9 ;VMCALL_REDIRECTINT1

  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler1
  xor eax,eax
  mov ax,cs
  mov dword [rsp+0x1c], eax


   ;int3
  mov rax,rsp
  mov rdx,0x76543210 ;p1
  call [vmcall_instr]

  mov rax,rsp
  mov dword [rsp+8],24 ;VMCALL_REDIRECTINT3
  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler3
  call [vmcall_instr]

  mov rax,rsp
  mov dword [rsp+8],22 ;VMCALL_REDIRECTINT14
  mov dword [rsp+0xc],1 ;idt redirect instead of intredirect
  mov qword [rsp+0x14], inthandler14
  call [vmcall_instr]


  add rsp,8+0x20
  ret

global SaveExtraHostState
;void SaveExtraHostState(VMCB_PA)
SaveExtraHostState:
  ;xchg bx,bx
  xchg rax,rdi
  vmsave
  xchg rax,rdi
  ret

struc vmxloop_amd_stackframe
  saved_r15:      resq 1
  saved_r14:      resq 1
  saved_r13:      resq 1
  saved_r12:      resq 1
  saved_r11:      resq 1
  saved_r10:      resq 1
  saved_r9:       resq 1
  saved_r8:       resq 1
  saved_rbp:      resq 1
  saved_rsi:      resq 1
  saved_rdi:      resq 1
  saved_rdx:      resq 1
  saved_rcx:      resq 1
  saved_rbx:      resq 1
  saved_rax:      resq 1
  saved_fsbase:   resq 1
  fxsavespace:    resb 512 ;fxsavespace must be aligned

  psavedstate:    resq 1 ;saved param4
  vmcb_hostsave_PA: resq 1 ;saved param3
  vmcb_PA:        resq 1 ;saved param2
  currentcpuinfo: resq 1 ;saved param1
  ;At entry RSP points here
  returnaddress:  resq 1


endstruc
extern vmexit_amd

align 16
global vmxloop_amd
vmxloop_amd:
;xchg bx,bx ;break by bochs

sub rsp, vmxloop_amd_stackframe_size

mov [rsp+currentcpuinfo],rdi
mov [rsp+vmcb_PA], rsi
mov [rsp+vmcb_hostsave_PA], rdx
mov [rsp+psavedstate], rcx

clgi ;no more interrupts from this point on. (Not even some special interrupts)


mov rax,rcx
cmp rax,0
je notloadedos_amd

;setup the initial state
mov rbx,[rax+0x08]
mov rcx,[rax+0x10]
mov rdx,[rax+0x18]
mov rsi,[rax+0x20]
mov rdi,[rax+0x28]
mov rbp,[rax+0x30]
mov r8,[rax+0x40]
mov r9,[rax+0x48]
mov r10,[rax+0x50]
mov r11,[rax+0x58]
mov r12,[rax+0x60]
mov r13,[rax+0x68]
mov r14,[rax+0x70]
mov r15,[rax+0x78]

jmp vmrun_loop


notloadedos_amd:
;init to startup state (or offloados state)

xor rax,rax
mov rbx,rax
mov rcx,rax
mov rdx,rax
mov rdi,rax
mov rbp,rax
mov r8, rax
mov r9, rax
mov r10,rax
mov r11,rax
mov r12,rax
mov r13,rax
mov r14,rax
mov r15,rax
mov rsi,rax


vmrun_loop:
;xchg bx,bx
mov rax,[rsp+vmcb_hostsave_PA]
vmsave

mov rax,[rsp+vmcb_PA]  ;for those wondering, RAX is stored in the vmcb->RAX field, not here
vmload
vmrun ;rax
vmsave

mov rax,[rsp+vmcb_hostsave_PA]
vmload ;this way I don't have to fuck with the fsbase msr


db 0x48
fxsave [rsp+fxsavespace]
movdqa [rsp+fxsavespace+0xA0], xmm0
movdqa [rsp+fxsavespace+0xB0], xmm1
movdqa [rsp+fxsavespace+0xC0], xmm2
movdqa [rsp+fxsavespace+0xD0], xmm3
movdqa [rsp+fxsavespace+0xE0], xmm4
movdqa [rsp+fxsa