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
mov