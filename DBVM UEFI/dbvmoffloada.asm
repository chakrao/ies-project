BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

struc GDTDesc
 g_Limit resw 1
 g_Base resq 1
endstruc

struc OState
.cpucount   resq 1
.originalEFER   resq 1
.originalLME  resq 1
.idtbase    resq 1
.idtlimit   resq 1
.gdtbase    resq 1
.gdtlimit   resq 1
.cr0      resq 1
.cr2      resq 1
.cr3      resq 1
.cr4      resq 1
.dr7      resq 1
.rip      resq 1
.rax      resq 1
.rbx      resq 1
.rcx      resq 1
.rdx      resq 1
.rsi      resq 1
.rdi      resq 1
.rbp      resq 1
.rsp      resq 1
.r8       resq 1
.r9       resq 1
.r10      resq 1
.r11      resq 1
.r12      resq 1
.r13      resq 1
.r14      resq 1
.r15      resq 1
.rflags     resq 1
.cs       resq 1
.ss       resq 1
.ds       resq 1
.es       resq 1
.fs       resq 1
.gs       resq 1
.tr       resq 1
.ldt      resq 1
.cs_AccessRights  resq 1
.ss_AccessRights  resq 1
.ds_AccessRights  resq 1
.es_AccessRights  resq 1
.fs_AccessRights  resq 1
.gs_AccessRights  resq 1
.cs_Limit resq 1
.ss_Limit resq 1
.ds_Limit resq 1
.es_Limit resq 1
.fs_Limit resq 1
.gs_Limit resq 1
.fsbase     resq 1
.gsbase     resq 1
endstruc



EXTERN NewGDTDescriptor
EXTERN NewGDTDescriptorVA
EXTERN DBVMPML4PA
EXTERN TemporaryPagingSetupPA
EXTERN enterVMM2PA
EXTERN originalstatePA
EXTERN enterVMM2

EXTERN originalstate
EXTERN vmmPA
EXTERN InitStackPA

GLOBAL doSystemTest
doSystemTest:
  sub rsp,8+4*8
  mov [rsp+00h],rbx
  mov [rsp+08h],rcx
  mov [rsp+10h],rdx
  mov rax,dr7
  mov [rsp+18h],rax

  mov rax,0x402
  mov dr7,rax
  mov rax,dr7
  cmp rax,0x402
  je pass1

  mov rax,1
  jmp doSystemTest_exit


pass1:
  cpuid
  mov rax,dr7
  cmp rax,0x402
  je pass2

  ;fail test 2
  mov rax,2
  jmp doSystemTest_exit

pass2:
  xor rax,rax

doSystemTest_exit:
  mov r