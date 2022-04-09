BITS 64

;param passing in 64-bit (linux ABI, NOT windows)
;1=rdi
;2=rsi
;3=rdx
;4=rcx

extern password1
extern password2
extern password3

GLOBAL testfunction

testfunction:
  mov rax,cr3
  ret


GLOBAL brk

brk:
  db 0xcc
  ret


global timeCheck
timeCheck:
  ;rdi is a pointer to an array of 5 qwords
  sub rsp,64+8
  mov [rsp+0],r8
  mov [rsp+0x08],r9
  mov [rsp+0x10],r10
  mov [rsp+0x18],r11
  mov [rsp+0x20],r12


  rdtsc
  mov r8d,edx
  shl r8,32
  or r8d,eax

  rdtsc
  mov r9d,edx
  shl r9,32
  or r9d,eax

  rdtsc
  mov r10d,edx
  shl r10,32
  or r10d,eax

  rdtsc
  mov r11d,edx
  shl r11,32
  or r11d,eax

  rdtsc
  mov r12d,edx
  shl r12,32
  mov r12d,eax

  mov [rdi],r8
  mov [rdi+0x8],r9
  mov [rdi+0x10],r10
  mov [rdi+0x18],r11
  mov [rdi+0x20],r12


  mov r8,[rsp+0]
  mov r9,[rsp+0x08]
  mov r10,[rsp+0x10]
  mov r11,[rsp+0x18]
  mov r12,[rsp+0x20]

  add rsp,64+8
  ret

global readMSR
readMSR:
  xchg ecx,edi
  rdmsr ;return goes into edx