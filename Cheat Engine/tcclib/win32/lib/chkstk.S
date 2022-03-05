/* ---------------------------------------------- */
/* chkstk86.s */

#ifdef __leading_underscore
# define _(s) _##s
#else
# define _(s) s
#endif

/* ---------------------------------------------- */
#ifndef __x86_64__
/* ---------------------------------------------- */

.globl _(__chkstk)
_(__chkstk):
    xchg    (%esp),%ebp     /* store ebp, get ret.addr */
    push    %ebp            /* push ret.addr */
    lea     4(%esp),%ebp    /* setup frame ptr */
    push    %ecx            /* save ecx */
    mov     %ebp,%ecx
P0:
    sub     $4096,%ecx
    test    %eax,(%ecx)
    sub     $4096,%eax
    cmp     $4096,%eax
    jge     P0
    sub     %eax,%ecx
    test    %eax,(%ecx)

    mov     %esp,%eax
    mov     %ecx,%esp
    mov     (%eax),%ecx     /* restore ecx */
    jmp     *4(%eax)

/* ---------------------------------------------- */
#else
/* ---------------------------------------------- */

.globl _(__chkstk)
_(__