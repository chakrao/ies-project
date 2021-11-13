# gas comment with ``gnu'' style quotes

/* some directive tests */

   .byte 0xff
   .byte 1, 2, 3
   .short 1, 2, 3
   .word 1, 2, 3
   .long 1, 2, 3
   .int 1, 2, 3
   .align 8
   .byte 1
/* .align 16, 0x90 gas is too clever for us with 0x90 fill */
   .balign 4, 0x92
   .align 16, 0x91 /* 0x91 tests the non-clever behaviour */
   .skip 3
   .skip 15, 0x90
   .string "hello\0world"
/* Macro expansion should work like with C, the #n shouldn't be parsed
   as asm line comment */
#define __stringify(n) #n
#define stringify(n) __stringify(n)
   .skip 8,0x90
   .asciz stringify(BLA)
   .skip 8,0x90

# 28 "asmtest.S"        # a line directive (and a line comment)
        movl %eax, %ebx # some more asm comment
/* some label tests */
L1:
        movl %eax, %ebx
        mov 0x10000, %eax
L2:
        movl $L2 - L1, %ecx
var1:
        nop ; nop ; nop ; nop

        mov var1, %eax

/* instruction tests */
movl %eax, %ebx
mov 0x10000, %eax
mov 0x10000, %ax
mov 0x10000, %al
mov %al, 0x10000
                
mov $1, %edx
mov $1, %dx
mov $1, %cl
movb $2, 0x100(%ebx,%edx,2)
movw $2, 0x100(%ebx,%edx,2)
movl $2, 0x100(%ebx,%edx,2)
movl %eax, 0x100(%ebx,%edx,2)
movl 0x100(%ebx,%edx,2), %edx
movw %ax, 0x100(%ebx,%edx,2)

movw $0x1122,%si
movl $0x112233,%edx
movl $0x80000000, %esi
movl $-0x7fffffff, %edi
#ifdef __x86_64__
mov $0x11223344,%rbx
movq $0x11223344,%rbx
mov $0x1122334455,%rbx
movq $0x1122334455,%rbx
movl $0x11334455,(%rbx)
#endif

mov %eax, 0x12(,%edx,2)
        
#ifdef __i386__
mov %cr3, %edx
mov %ecx, %cr3
movl %cr3, %eax
movl %tr3, %eax
movl %db3, %ebx
movl %dr6, %eax
#else
mov %cr3, %rdx
mov %rcx, %cr3
movq %cr3, %rax
movq %db3, %rbx
movq %dr6, %rax
mov %cr8, %rsi
mov %rdi, %cr8
#endif
movl %fs, %ecx
movl %ebx, %fs

#ifdef __x86_64__
movq %r8, %r9
movq %r10, %r11
movq %r12, %r13
movq %r14, %r15
movq %rax, %r9
movq %r15, %rsi
inc %r9b
dec %r10w
not %r11d
negq %r12
decb %r13b
incw %r14w
notl %r15d
#endif

     movsbl 0x1000, %eax
     movsbw 0x1000, %ax
     movswl 0x1000, %eax

     movzbl 0x1000, %eax
     movzbw 0x1000, %ax
     movzwl 0x1000, %eax
            
     movzb 0x1000, %eax
     movzb 0x1000, %ax
                
     mov $0x12345678,%eax

#ifdef __x86_64__
     movzb 0x1000, %rax
     movzbq 0x1000, %rbx
     movsbq 0x1000, %rdx
     movzwq 0x1000, %rdi
     movswq 0x1000, %rdx
     movslq %eax, %rcx
     mov $0x12345678,%rax
     mov $0x12345678,%rdx
     mov $0x12345678,%r10
     mov $0x123456789abcdef0,%rax
     mov $0x123456789abcdef0,%rcx
     mov $0x123456789abcdef0,%r11
#endif
        
#ifdef __i386__
  pushl %eax
  push %eax
  push %cs
#else
  pushq %rax
  push %rax
#endif
  pushw %ax
  push %gs
  push $1
  push $100
  push 0x42(%eax)
  pop 0x43(%esi)
                                                
#ifdef __i386__
  popl %eax
  pop %eax
  pop %ds
#else
  popq %rax
  pop %rax
#endif
  popw %ax
  pop %fs
          
  xchg %eax, %ecx
  xchg %edx, %eax
  xchg %bx, 0x10000
  xchg 0x10000, %ebx
  xchg 0x10000, %dl

  in $100, %al               
  in $100, %ax               
  in $100, %eax
  in %dx, %al
  in %dx, %ax               
  in %dx, %eax
  inb %dx
  inw %dx               
  inl %dx

  out %al, $100                       
  out %ax, $100                       
  out %eax, $100                       

  /* NOTE: gas is bugged here, so size must be added */
  outb %al, %dx                       
  outw %ax, %dx                       
  outl %eax, %dx                       

  leal 0x1000(%ebx), %ecx
  lea 0x1000(%ebx), %ecx

#ifdef __i386__
  les 0x2000, %eax
  lds 0x2000, %ebx
  lss 0x2000, %edx
#endif
  lfs 0x2000, %ecx
  lgs 0x2000, %edx

addl $0x123, %eax
add $0x123, %ebx
add $-16, %ecx
add $-0x123, %esi
add $1, %bx
add $1, %ebx
add $-1, %bx
add $-1, %ebx
add $127, %bx
addl $127, %ebx
addl $-128, %ebx
addl $-128, %ebx
addl $-129, %ebx
addl $128, %ebx
addl $255, %ebx
addl $256, %ebx
andb $0xf, %ah
andb $-15, %cl
xorb $127, %dh
cmpb $42, (%eax)
addl $0x123, 0x100
addl $0x123, 0x100(%ebx)
addl $0x123, 0x100(%ebx,%edx,2)
addl $0x123, 0x100(%esp)
addl $0x123, (3*8)(%esp)
addl $0x123, (%ebp)
addl $0x123, (%esp)
cmpl $0x123, (%esp)

#ifdef __x86_64__
xor %bl,%ah
xor %bl,%r8b
xor %r9b,%bl
xor %sil,%cl
add %eax,(%r8d)
add %ebx,(%r9)
add %edx,(%r10d,%r11d)
add %ecx,(%r12,%r13)
add %esi,(%r14,%r15,4)
add %edi,0x1000(%rbx,%r12,8)
add %r11,0x1000(%ebp,%r9d,8)
movb $12, %ah
movb $13, %bpl
movb $14, %dil
movb $15, %r12b
#endif

add %eax, (%ebx)
add (%ebx), %eax
                
or %dx, (%ebx)
or (%ebx), %si
        
add %cl, (%ebx)
add (%ebx), %dl

    inc %edx
    incl 0x10000
    incb 0x10000
    dec %dx
  
  test $1, %al
  test $1, %cl

  testl $1, 0x1000
  testb $1, 0x1000
  testw $1, 0x1000
  test %eax, %ebx
  test %eax, 0x1000
  test 0x1000, %edx

    not %edx
    notw 0x10000
    notl 0x10000
    notb 0x10000

    neg %edx
    negw 0x10000
    negl 0x10000
    negb 0x10000

    imul %ecx
    mul %edx
    mulb %cl

    imul %eax, %ecx
    imul 0x1000, %cx
    imul $10, %eax, %ecx
    imul $10, %ax, %cx
    imul $10, %eax
    imul $0x1100000, %eax
    imul $1, %eax
    
    idivw 0x1000
    div %ecx
    div %bl
    div %ecx, %eax

and $15,%bx
and $-20,%edx

shl %edx
shl $10, %edx
shl %cl, %edx

shld $1, %eax, %edx
shld %cl, %eax, %edx
shld %eax, %edx

shrd $1, %eax, %edx
shrd %cl, %eax, %edx
shrd %eax, %edx

L4:
call 0x1000
call L4
#ifdef __i386__
call *%eax
#else
call *%rax
#endif
call *0x1000
call func1

.global L5,L6

L5:
L6:

#ifdef __i386__
lcall $0x100, $0x1000
#else
lcall *0x100
lcall *(%rax)
#endif

jmp 0x1000
jmp *(%edi)
#ifdef __i386__
jmp *%eax
#else
jmp *%rax
#endif
jmp *0x1000

#ifdef __i386__
ljmp $0x100, $0x1000
#else
ljmp *0x100
ljmp *(%rdi)
ljmpl *(%esi)
ljmpw *(%esi)
#endif

ret
ret $10
#ifdef __i386__
retl
retl $10
#e