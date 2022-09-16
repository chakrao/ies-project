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
movdqa [rsp+fxsavespace+0xF0], xmm5
movdqa [rsp+fxsavespace+0x100], xmm6
movdqa [rsp+fxsavespace+0x110], xmm7
movdqa [rsp+fxsavespace+0x120], xmm8
movdqa [rsp+fxsavespace+0x130], xmm9
movdqa [rsp+fxsavespace+0x140], xmm10
movdqa [rsp+fxsavespace+0x150], xmm11
movdqa [rsp+fxsavespace+0x160], xmm12
movdqa [rsp+fxsavespace+0x170], xmm13
movdqa [rsp+fxsavespace+0x180], xmm14
movdqa [rsp+fxsavespace+0x190], xmm15

mov [rsp+saved_r15],r15
mov [rsp+saved_r14],r14
mov [rsp+saved_r13],r13
mov [rsp+saved_r12],r12
mov [rsp+saved_r11],r11
mov [rsp+saved_r10],r10
mov [rsp+saved_r9],r9
mov [rsp+saved_r8],r8
mov [rsp+saved_rbp],rbp
mov [rsp+saved_rsi],rsi
mov [rsp+saved_rdi],rdi
mov [rsp+saved_rdx],rdx
mov [rsp+saved_rcx],rcx
mov [rsp+saved_rbx],rbx
mov [rsp+saved_rax],rax



mov rdi,[rsp+currentcpuinfo]
lea rsi,[rsp+saved_r15] ;vmregisters
lea rdx,[rsp+fxsavespace] ;fxsave

call vmexit_amd

;check return. If everything ok restore and jump to vmrun_loop
cmp eax,1
je vmrun_exit


db 0x48
fxrstor [rsp+fxsavespace]
movdqa xmm0, [rsp+fxsavespace+0xA0]
movdqa xmm1, [rsp+fxsavespace+0xB0]
movdqa xmm2, [rsp+fxsavespace+0xC0]
movdqa xmm3, [rsp+fxsavespace+0xD0]
movdqa xmm4, [rsp+fxsavespace+0xE0]
movdqa xmm5, [rsp+fxsavespace+0xF0]
movdqa xmm6, [rsp+fxsavespace+0x100]
movdqa xmm7, [rsp+fxsavespace+0x110]
movdqa xmm8, [rsp+fxsavespace+0x120]
movdqa xmm9, [rsp+fxsavespace+0x130]
movdqa xmm10, [rsp+fxsavespace+0x140]
movdqa xmm11, [rsp+fxsavespace+0x150]
movdqa xmm12, [rsp+fxsavespace+0x160]
movdqa xmm13, [rsp+fxsavespace+0x170]
movdqa xmm14, [rsp+fxsavespace+0x180]
movdqa xmm15, [rsp+fxsavespace+0x190]

mov r15,[rsp+saved_r15]
mov r14,[rsp+saved_r14]
mov r13,[rsp+saved_r13]
mov r12,[rsp+saved_r12]
mov r11,[rsp+saved_r11]
mov r10,[rsp+saved_r10]
mov r9,[rsp+saved_r9]
mov r8,[rsp+saved_r8]
mov rbp,[rsp+saved_rbp]
mov rsi,[rsp+saved_rsi]
mov rdi,[rsp+saved_rdi]
mov rdx,[rsp+saved_rdx]
mov rcx,[rsp+saved_rcx]
mov rbx,[rsp+saved_rbx]


jmp vmrun_loop



vmrun_exit:
add rsp,vmxloop_amd_stackframe_size-8
ret


global doVMRUN
;------------------------------------------------------------------------;
;QWORD doVMRUN(QWORD VMCB_PA, VMRegisters *vmregisters, QWORD dbvmhost_PA, QWORD emulatedhost_PA);
;------------------------------------------------------------------------;
doVMRUN:
;1=rdi=VMCB_PA
;2=rsi=vmregisters
;3=rdx=dbvmhost_PA
;4=rcx=emulatedhost_PA
xchg bx,bx

sub rsp, vmxloop_amd_stackframe_size-8

;store the host state
mov [rsp+saved_rbx],rbx
mov [rsp+saved_rcx],rcx
mov [rsp+saved_rdx],rdx
mov [rsp+saved_rbp],rbp
mov [rsp+saved_rsi],rsi
mov [rsp+saved_rdi],rdi
mov [rsp+saved_r8],r8
mov [rsp+saved_r9],r9
mov [rsp+saved_r10],r10
mov [rsp+saved_r11],r11
mov [rsp+saved_r12],r12
mov [rsp+saved_r13],r13
mov [rsp+saved_r14],r14
mov [rsp+saved_r15],r15

;dbvm doesn't need to store the fxstate. (has nothing stored there anyhow)

mov rax,[rsp+saved_rsi] ;vmregisters (amd stackframe)
mov r15,[rax+saved_r15]
mov r14,[rax+saved_r14]
mov r13,[rax+saved_r13]
mov r12,[rax+saved_r12]
mov r11,[rax+saved_r11]
mov r10,[rax+saved_r10]
mov r9,[rax+saved_r9]
mov r8,[rax+saved_r8]
mov rbp,[rax+saved_rbp]
mov rsi,[rax+saved_rsi]
mov rdi,[rax+saved_rdi]
mov rdx,[rax+saved_rdx]
mov rcx,[rax+saved_rcx]
mov rbx,[rax+saved_rbx]

mov rax,[rsp+saved_rdx] ;dbvmhost_pa
vmsave ;store the current state

mov rax,[rsp+saved_rcx] ;host_pa
vmload ;load the state of the host right before vmrun

mov rax,[rsp+saved_rdi] ;emulated guest vmcb pa
vmrun
mov rax,[rsp+saved_rcx]  ;save the new state to the previous host vmcb (it will be reposible for calling vmsave next)
vmsave

;restore dbvm state
mov rax,[rsp+saved_rdx]
vmload


;store the guest state
mov rax,[rsp+saved_rsi]
db 0x48
fxsave [rax+fxsavespace] ;save fpu

mov [rax+saved_rbx],rbx
mov [rax+saved_rcx],rcx
mov [rax+saved_rdx],rdx
mov [rax+saved_rsi],rsi
mov [rax+saved_rdi],rdi
mov [rax+saved_rbp],rbp
mov [rax+saved_r8],r8
mov [rax+saved_r9],r9
mov [rax+saved_r10],r10
mov [rax+saved_r11],r11
mov [rax+saved_r12],r12
mov [rax+saved_r13],r13
mov [rax+saved_r14],r14
mov [rax+saved_r15],r15

mov rax,[rsp+saved_rax]
mov rbx,[rsp+saved_rbx]
mov rcx,[rsp+saved_rcx]
mov rdx,[rsp+saved_rdx]
mov rsi,[rsp+saved_rsi]
mov rdi,[rsp+saved_rdi]
mov rbp,[rsp+saved_rbp]
mov r8,[rsp+saved_r8]
mov r9,[rsp+saved_r9]
mov r10,[rsp+saved_r10]
mov r11,[rsp+saved_r11]
mov r12,[rsp+saved_r12]
mov r13,[rsp+saved_r13]
mov r14,[rsp+saved_r14]
mov r15,[rsp+saved_r15]

add rsp,vmxloop_amd_stackframe_size-8
ret



global vmxloop
extern vmexit
;-------------------------;
;int vmxloop(cpuinfo *cpu, UINT64 *rax);
;-------------------------;
vmxloop: ;esp=return address, edi = cpuinfo structure pointer, rsi=mapped loadedOS eax base
;0


pushfq   ;8

push rax ;16
push rbx ;24
push rcx ;32
push rdx ;40
push rdi ;48
push rsi ;56
push rbp ;64
push r8  ;72
push r9  ;80
push r10 ;88
push r11 ;96
push r12 ;112
push r13 ;120
push r14 ;128
push r15 ;136

mov rax,0x6c14
vmwrite rax,rsp ;host_esp

mov rax,0x6c16
mov rdx,vmxloop_vmexit
vmwrite rax,rdx  ;host_eip

cmp rsi,0
je notloadedOS

osoffload:
;xchg bx,bx
mov rax,[rsi]
mov rbx,[rsi+0x08]
mov rcx,[rsi+0x10]
mov rdx,[rsi+0x18]
mov rdi,[rsi+0x28]
mov rbp,[rsi+0x30]
mov r8,[rsi+0x40]
mov r9,[rsi+0x48]
mov r10,[rsi+0x50]
mov r11,[rsi+0x58]
mov r12,[rsi+0x60]
mov r13,[rsi+0x68]
mov r14,[rsi+0x70]
mov r15,[rsi+0x78]

mov rsi,[rsi+0x20]

jmp aftersetup

notloadedOS:
xor rax,rax
mov rbx,rax
mov rcx,rax
mov rdx,rax
mov rdi,rax
mov rsi,1 ;for the skipAPTerminationWait parameter for reboot
mov rbp,rax
mov r8, rax
mov r9, rax
mov r10,rax
mov r11,rax
mov r12,rax
mov r13,rax
mov r14,rax
mov r15,rax

aftersetup:
vmlaunch
;just continued through, restore state

%ifdef JTAG
db 0xf1 ;jtag breakpoint
%endif

nop
nop ;just making sure as for some reason kvm's gdb continues here, instead of the previous instruction
nop

pop r15 ;128
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax ;8

jc vmxloop_fullerr
jz vmxloop_halferr
jmp vmxloop_weirderr


vmxloop_fullerr:
mov eax,1
popfq ;(esp-0)
ret

vmxloop_halferr:
mov eax,2
popfq ;(esp-0)
ret

vmxloop_weirderr:
mov eax,3
popfq ;(esp-0)
ret

align 16
vmxloop_vmexit:
;cli
;ok, this should be executed

;cmp dword [fs:0x14],0
;je isbootcpu



;isbootcpu:

;save registers

;db 0xf1


sub rsp,15*8

mov [rsp+14*8],rax
mov [rsp+11*8],rdx


mov [rsp],r15
mov [rsp+1*8],r14
mov [rsp+2*8],r13
mov [rsp+3*8],r12
mov [rsp+4*8],r11
mov [rsp+5*8],r10
mov [rsp+6*8],r9
mov [rsp+7*8],r8
mov [rsp+8*8],rbp
mov [rsp+9*8],rsi
mov [rsp+10*8],rdi
mov [rsp+12*8],rcx
mov [rsp+13*8],rbx


;set host into a 'valid' state
mov rbp,rsp


fucker:
mov rdi,[rbp+128+ 72] ; param1:currentcpuinfo (rdi of the original host registers, so past the guest registers, inside the host save state)
mov rsi,rbp ; param2: pointer to the guest registers (stored on stack)

cmp rdi,0
jne notfucker

;xchg bx,bx
wbinvd

mov rbx,0x681e  ;RIP
vmread rax,rbx

mov rbx,0x6808  ;CS
vmread rbx,rbx



mov rdi,[rsp+128+ 72] ; param1:currentcpuinfo (rdi of the original host registers, so past the guest registers, inside the host save state)
mov rsi,rsp ; param2: pointer to the guest registers (stored on stack)


notfucker:
;sub rbp,8

;xchg bx,bx ;boxhs bp

and rsp,-0x10 ;0xfffffffffffffff0;
sub rsp,512
db 0x48
fxsave [rsp]
mov rdx,rsp ;param 3, pointer to fxsave

sub rsp,32

;xchg bx,bx

call vmexit

add rsp,32
db 0x48
fxrstor [rsp]


mov rsp,rbp

cmp ax,0xce00
je vmxloop_guestlaunch

cmp ax,0xce01
je vmxloop_guestresume

cmp al,1  ;returnvalue of 1 = quit vmx
jae vmxloop_exitvm


;returned 0, so

;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

;and resume
vmresume

;never executed unless on error
;restore state of vmm


pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax ;skip rax, rax contains the result
popfq ;restore flags (esp)
mov rax,3
ret

vmxloop_guestlaunch:
;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

vmlaunch


;db 0xf1 ;debug

;never executed unless on error
;restore state of vmm


mov dword [fs:0x10],0xce00 ;exitreason 0xce00
jmp vmxloop_vmexit

vmxloop_guestresume:
;restore vmx registers (esp-36)
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
pop rax

vmresume

db 0xf1 ;debug

;never executed unless on error
mov dword [fs:0x10],0xce01 ;exitreason 0xce01  (resume fail)
jmp vmxloop_vmexit

vmxloop_exitvm:  ;(esp-68)
;user quit or couldn't be handled
xor eax,eax  ;0, so ok



vmxloop_exit: ;(esp)
add rsp,120  ;128=eax=eflags=error, 136=ebx=eflags, 120=
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rbp
pop rsi
pop rdi
pop rdx
pop rcx
pop rbx
add rsp,8 ;;skip rax, rax contains the result
popfq ;restore flags (esp)
ret


db 0xcc
db 0xcc
db 0xcc

;---------------------;
;void setRFLAGS(void);
;---------------------;
global setRFLAGS
setRFLAGS:
push rdi
popfq
ret
;---------------------;
;ULONG getRFLAGS(void);
;---------------------;
global getRFLAGS
getRFLAGS:
pushfq
pop rax
ret

db 0xcc
db 0xcc
db 0xcc

;-----------------------------------;
;void setIDT(UINT64 base, WORD size);
;-------------