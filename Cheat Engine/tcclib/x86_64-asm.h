     DEF_ASM_OP0(clc, 0xf8) /* must be first OP0 */
     DEF_ASM_OP0(cld, 0xfc)
     DEF_ASM_OP0(cli, 0xfa)
     DEF_ASM_OP0(clts, 0x0f06)
     DEF_ASM_OP0(cmc, 0xf5)
     DEF_ASM_OP0(lahf, 0x9f)
     DEF_ASM_OP0(sahf, 0x9e)
     DEF_ASM_OP0(pushfq, 0x9c)
     DEF_ASM_OP0(popfq, 0x9d)
     DEF_ASM_OP0(pushf, 0x9c)
     DEF_ASM_OP0(popf, 0x9d)
     DEF_ASM_OP0(stc, 0xf9)
     DEF_ASM_OP0(std, 0xfd)
     DEF_ASM_OP0(sti, 0xfb)
     DEF_ASM_OP0(aaa, 0x37)
     DEF_ASM_OP0(aas, 0x3f)
     DEF_ASM_OP0(daa, 0x27)
     DEF_ASM_OP0(das, 0x2f)
     DEF_ASM_OP0(aad, 0xd50a)
     DEF_ASM_OP0(aam, 0xd40a)
     DEF_ASM_OP0(cbw, 0x6698)
     DEF_ASM_OP0(cwd, 0x6699)
     DEF_ASM_OP0(cwde, 0x98)
     DEF_ASM_OP0(cdq, 0x99)
     DEF_ASM_OP0(cbtw, 0x6698)
     DEF_ASM_OP0(cwtl, 0x98)
     DEF_ASM_OP0(cwtd, 0x6699)
     DEF_ASM_OP0(cltd, 0x99)
     DEF_ASM_OP0(cqto, 0x4899)
     DEF_ASM_OP0(int3, 0xcc)
     DEF_ASM_OP0(into, 0xce)
     DEF_ASM_OP0(iret, 0xcf)
     DEF_ASM_OP0(rsm, 0x0faa)
     DEF_ASM_OP0(hlt, 0xf4)
     DEF_ASM_OP0(wait, 0x9b)
     DEF_ASM_OP0(nop, 0x90)
     DEF_ASM_OP0(pause, 0xf390)
     DEF_ASM_OP0(xlat, 0xd7)

     /* strings */
ALT(DEF_ASM_OP0L(cmpsb, 0xa6, 0, OPC_BWLX))
ALT(DEF_ASM_OP0L(scmpb, 0xa6, 0, OPC_BWLX))

ALT(DEF_ASM_OP0L(insb, 0x6c, 0, OPC_BWL))
ALT(DEF_ASM_OP0L(outsb, 0x6e, 0, OPC_BWL))

ALT(DEF_ASM_OP0L(lodsb, 0xac, 0, OPC_BWLX))
ALT(DEF_ASM_OP0L(slodb, 0xac, 0, OPC_BWLX))

ALT(DEF_ASM_OP0L(movsb, 0xa4, 0, OPC_BWLX))
ALT(DEF_ASM_OP0L(smovb, 0xa4, 0, OPC_BWLX))

ALT(DEF_ASM_OP0L(scasb, 0xae, 0, OPC_BWLX))
ALT(DEF_ASM_OP0L(sscab, 0xae, 0, OPC_BWLX))

ALT(DEF_ASM_OP0L(stosb, 0xaa, 0, OPC_BWLX))
ALT(DEF_ASM_OP0L(sstob, 0xaa, 0, OPC_BWLX))

     /* bits */

ALT(DEF_ASM_OP2(bsfw, 0x0fbc, 0, OPC_MODRM | OPC_WLX, OPT_REGW | OPT_EA, OPT_REGW))
ALT(DEF_ASM_OP2(bsrw, 0x0fbd, 0, OPC_MODRM | OPC_WLX, OPT_REGW | OPT_EA, OPT_REGW))

ALT(DEF_ASM_OP2(btw, 0x0fa3, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_REGW | OPT_EA))
ALT(DEF_ASM_OP2(btw, 0x0fba, 4, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW | OPT_EA))

ALT(DEF_ASM_OP2(btsw, 0x0fab, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_REGW | OPT_EA))
ALT(DEF_ASM_OP2(btsw, 0x0fba, 5, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW | OPT_EA))

ALT(DEF_ASM_OP2(btrw, 0x0fb3, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_REGW | OPT_EA))
ALT(DEF_ASM_OP2(btrw, 0x0fba, 6, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW | OPT_EA))

ALT(DEF_ASM_OP2(btcw, 0x0fbb, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_REGW | OPT_EA))
ALT(DEF_ASM_OP2(btcw, 0x0fba, 7, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW | OPT_EA))

     /* prefixes */
     DEF_ASM_OP0(lock, 0xf0)
     DEF_ASM_OP0(rep, 0xf3)
     DEF_ASM_OP0(repe, 0xf3)
     DEF_ASM_OP0(repz, 0xf3)
     DEF_ASM_OP0(repne, 0xf2)
     DEF_ASM_OP0(repnz, 0xf2)

     DEF_ASM_OP0(invd, 0x0f08)
     DEF_ASM_OP0(wbinvd, 0x0f09)
     DEF_ASM_OP0(cpuid, 0x0fa2)
     DEF_ASM_OP0(wrmsr, 0x0f30)
     DEF_ASM_OP0(rdtsc, 0x0f31)
     DEF_ASM_OP0(rdmsr, 0x0f32)
     DEF_ASM_OP0(rdpmc, 0x0f33)

     DEF_ASM_OP0(syscall, 0x0f05)
     DEF_ASM_OP0(sysret, 0x0f07)
     DEF_ASM_OP0L(sysretq, 0x480f07, 0, 0)
     DEF_ASM_OP0(ud2, 0x0f0b)

     /* NOTE: we took the same order as gas opcode definition order */
/* Right now we can't express the fact that 0xa1/0xa3 can't use $eax and a 
   32 bit moffset as operands.
ALT(DEF_ASM_OP2(movb, 0xa0, 0, OPC_BWLX, OPT_ADDR, OPT_EAX))
ALT(DEF_ASM_OP2(movb, 0xa2, 0, OPC_BWLX, OPT_EAX, OPT_ADDR)) */
ALT(DEF_ASM_OP2(movb, 0x88, 0, OPC_MODRM | OPC_BWLX, OPT_REG, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP2(movb, 0x8a, 0, OPC_MODRM | OPC_BWLX, OPT_EA | OPT_REG, OPT_REG))
/* The moves are special: the 0xb8 form supports IM64 (the only insn that
   does) with REG64.  It doesn't support IM32 with REG64, it would use
   the full movabs form (64bit immediate).  For IM32->REG64 we prefer
   the 0xc7 opcode.  So disallow all 64bit forms and code the rest by hand. */
ALT(DEF_ASM_OP2(movb, 0xb0, 0, OPC_REG | OPC_BWLX, OPT_IM, OPT_REG))
ALT(DEF_ASM_OP2(mov,  0xb8, 0, OPC_REG, OPT_IM64, OPT_REG64))
ALT(DEF_ASM_OP2(movq, 0xb8, 0, OPC_REG, OPT_IM64, OPT_REG64))
ALT(DEF_ASM_OP2(movb, 0xc6, 0, OPC_MODRM | OPC_BWLX, OPT_IM, OPT_REG | OPT_EA))

ALT(DEF_ASM_OP2(movw, 0x8c, 0, OPC_MODRM | OPC_WLX, OPT_SEG, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP2(movw, 0x8e, 0, OPC_MODRM | OPC_WLX, OPT_EA | OPT_REG, OPT_SEG))

ALT(DEF_ASM_OP2(movw, 0x0f20, 0, OPC_MODRM | OPC_WLX, OPT_CR, OPT_REG64))
ALT(DEF_ASM_OP2(movw, 0x0f21, 0, OPC_MODRM | OPC_WLX, OPT_DB, OPT_REG64))
ALT(DEF_ASM_OP2(movw, 0x0f22, 0, OPC_MODRM | OPC_WLX, OPT_REG64, OPT_CR))
ALT(DEF_ASM_OP2(movw, 0x0f23, 0, OPC_MODRM | OPC_WLX, OPT_REG64, OPT_DB))

ALT(DEF_ASM_OP2(movsbw, 0x660fbe, 0, OPC_MODRM, OPT_REG8 | OPT_EA, OPT_REG16))
ALT(DEF_ASM_OP2(movsbl, 0x0fbe, 0, OPC_MODRM, OPT_REG8 | OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(movsbq, 0x0fbe, 0, OPC_MODRM, OPT_REG8 | OPT_EA, OPT_REGW))
ALT(DEF_ASM_OP2(movswl, 0x0fbf, 0, OPC_MODRM, OPT_REG16 | OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(movswq, 0x0fbf, 0, OPC_MODRM, OPT_REG16 | OPT_EA, OPT_REG))
ALT(DEF_ASM_OP2(movslq, 0x63, 0, OPC_MODRM, OPT_REG32 | OPT_EA, OPT_REG))
ALT(DEF_ASM_OP2(movzbw, 0x0fb6, 0, OPC_MODRM | OPC_WLX, OPT_REG8 | OPT_EA, OPT_REGW))
ALT(DEF_ASM_OP2(movzwl, 0x0fb7, 0, OPC_MODRM, OPT_REG16 | OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(movzwq, 0x0fb7, 0, OPC_MODRM, OPT_REG16 | OPT_EA, OPT_REG))

ALT(DEF_ASM_OP1(pushq, 0x6a, 0, 0, OPT_IM8S))
ALT(DEF_ASM_OP1(push, 0x6a, 0, 0, OPT_IM8S))
ALT(DEF_ASM_OP1(pushw, 0x666a, 0, 0, OPT_IM8S))
ALT(DEF_ASM_OP1(pushw, 0x50, 0, OPC_REG | OPC_WLX, OPT_REG64))
ALT(DEF_ASM_OP1(pushw, 0x50, 0, OPC_REG | OPC_WLX, OPT_REG16))
ALT(DEF_ASM_OP1(pushw, 0xff, 6, OPC_MODRM | OPC_WLX, OPT_REG64 | OPT_EA))
ALT(DEF_ASM_OP1(pushw, 0x6668, 0, 0, OPT_IM16))
ALT(DEF_ASM_OP1(pushw, 0x68, 0, OPC_WLX, OPT_IM32))
ALT(DEF_ASM_OP1(pushw, 0x06, 0, OPC_WLX, OPT_SEG))

ALT(DEF_ASM_OP1(popw, 0x58, 0, OPC_REG | OPC_WLX, OPT_REG64))
ALT(DEF_ASM_OP1(popw, 0x58, 0, OPC_REG | OPC_WLX, OPT_REG16))
ALT(DEF_ASM_OP1(popw, 0x8f, 0, OPC_MODRM | OPC_WLX, OPT_REGW | OPT_EA))
ALT(DEF_ASM_OP1(popw, 0x07, 0, OPC_WLX, OPT_SEG))

ALT(DEF_ASM_OP2(xchgw, 0x90, 0, OPC_REG | OPC_WLX, OPT_REGW, OPT_EAX))
ALT(DEF_ASM_OP2(xchgw, 0x90, 0, OPC_REG | OPC_WLX, OPT_EAX, OPT_REGW))
ALT(DEF_ASM_OP2(xchgb, 0x86, 0, OPC_MODRM | OPC_BWLX, OPT_REG, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP2(xchgb, 0x86, 0, OPC_MODRM | OPC_BWLX, OPT_EA | OPT_REG, OPT_REG))

ALT(DEF_ASM_OP2(inb, 0xe4, 0, OPC_BWL, OPT_IM8, OPT_EAX))
ALT(DEF_ASM_OP1(inb, 0xe4, 0, OPC_BWL, OPT_IM8))
ALT(DEF_ASM_OP2(inb, 0xec, 0, OPC_BWL, OPT_DX, OPT_EAX))
ALT(DEF_ASM_OP1(inb, 0xec, 0, OPC_BWL, OPT_DX))

ALT(DEF_ASM_OP2(outb, 0xe6, 0, OPC_BWL, OPT_EAX, OPT_IM8))
ALT(DEF_ASM_OP1(outb, 0xe6, 0, OPC_BWL, OPT_IM8))
ALT(DEF_ASM_OP2(outb, 0xee, 0, OPC_BWL, OPT_EAX, OPT_DX))
ALT(DEF_ASM_OP1(outb, 0xee, 0, OPC_BWL, OPT_DX))

ALT(DEF_ASM_OP2(leaw, 0x8d, 0, OPC_MODRM | OPC_WLX, OPT_EA, OPT_REG))

ALT(DEF_ASM_OP2(les, 0xc4, 0, OPC_MODRM, OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(lds, 0xc5, 0, OPC_MODRM, OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(lss, 0x0fb2, 0, OPC_MODRM, OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(lfs, 0x0fb4, 0, OPC_MODRM, OPT_EA, OPT_REG32))
ALT(DEF_ASM_OP2(lgs, 0x0fb5, 0, OPC_MODRM, OPT_EA, OPT_REG32))

     /* arith */
ALT(DEF_ASM_OP2(addb, 0x00, 0, OPC_ARITH | OPC_MODRM | OPC_BWLX, OPT_REG, OPT_EA | OPT_REG)) /* XXX: use D bit ? */
ALT(DEF_ASM_OP2(addb, 0x02, 0, OPC_ARITH | OPC_MODRM | OPC_BWLX, OPT_EA | OPT_REG, OPT_REG))
ALT(DEF_ASM_OP2(addb, 0x04, 0, OPC_ARITH | OPC_BWLX, OPT_IM, OPT_EAX))
ALT(DEF_ASM_OP2(addw, 0x83, 0, OPC_ARITH | OPC_MODRM | OPC_WLX, OPT_IM8S, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP2(addb, 0x80, 0, OPC_ARITH | OPC_MODRM | OPC_BWLX, OPT_IM, OPT_EA | OPT_REG))

ALT(DEF_ASM_OP2(testb, 0x84, 0, OPC_MODRM | OPC_BWLX, OPT_REG, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP2(testb, 0x84, 0, OPC_MODRM | OPC_BWLX, OPT_EA | OPT_REG, OPT_REG))
ALT(DEF_ASM_OP2(testb, 0xa8, 0, OPC_BWLX, OPT_IM, OPT_EAX))
ALT(DEF_ASM_OP2(testb, 0xf6, 0, OPC_MODRM | OPC_BWLX, OPT_IM, OPT_EA | OPT_REG))

ALT(DEF_ASM_OP1(incb, 0xfe, 0, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))
ALT(DEF_ASM_OP1(decb, 0xfe, 1, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))

ALT(DEF_ASM_OP1(notb, 0xf6, 2, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))
ALT(DEF_ASM_OP1(negb, 0xf6, 3, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))

ALT(DEF_ASM_OP1(mulb, 0xf6, 4, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))
ALT(DEF_ASM_OP1(imulb, 0xf6, 5, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))

ALT(DEF_ASM_OP2(imulw, 0x0faf, 0, OPC_MODRM | OPC_WLX, OPT_REG | OPT_EA, OPT_REG))
ALT(DEF_ASM_OP3(imulw, 0x6b, 0, OPC_MODRM | OPC_WLX, OPT_IM8S, OPT_REGW | OPT_EA, OPT_REGW))
ALT(DEF_ASM_OP2(imulw, 0x6b, 0, OPC_MODRM | OPC_WLX, OPT_IM8S, OPT_REGW))
ALT(DEF_ASM_OP3(imulw, 0x69, 0, OPC_MODRM | OPC_WLX, OPT_IMW, OPT_REGW | OPT_EA, OPT_REGW))
ALT(DEF_ASM_OP2(imulw, 0x69, 0, OPC_MODRM | OPC_WLX, OPT_IMW, OPT_REGW))

ALT(DEF_ASM_OP1(divb, 0xf6, 6, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))
ALT(DEF_ASM_OP2(divb, 0xf6, 6, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA, OPT_EAX))
ALT(DEF_ASM_OP1(idivb, 0xf6, 7, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA))
ALT(DEF_ASM_OP2(idivb, 0xf6, 7, OPC_MODRM | OPC_BWLX, OPT_REG | OPT_EA, OPT_EAX))

     /* shifts */
ALT(DEF_ASM_OP2(rolb, 0xc0, 0, OPC_MODRM | OPC_BWLX | OPC_SHIFT, OPT_IM8, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP2(rolb, 0xd2, 0, OPC_MODRM | OPC_BWLX | OPC_SHIFT, OPT_CL, OPT_EA | OPT_REG))
ALT(DEF_ASM_OP1(rolb, 0xd0, 0, OPC_MODRM | OPC_BWLX | OPC_SHIFT, OPT_EA | OPT_REG))

ALT(DEF_ASM_OP3(shldw, 0x0fa4, 0, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP3(shldw, 0x0fa5, 0, OPC_MODRM | OPC_WLX, OPT_CL, OPT_REGW, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP2(shldw, 0x0fa5, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP3(shrdw, 0x0fac, 0, OPC_MODRM | OPC_WLX, OPT_IM8, OPT_REGW, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP3(shrdw, 0x0fad, 0, OPC_MODRM | OPC_WLX, OPT_CL, OPT_REGW, OPT_EA | OPT_REGW))
ALT(DEF_ASM_OP2(shrdw, 0x0fad, 0, OPC_MODRM | OPC_WLX, OPT_REGW, OPT_EA | OPT_REGW))

ALT(DEF_ASM_OP1(call, 0xff, 2, OPC_MODRM, OPT_INDIR))
ALT(DEF_ASM_OP1(call, 0xe8, 0, 0, OPT_DISP))
ALT(DEF_ASM_OP1(jmp, 0xff, 4, OPC_MODRM, OPT_INDIR))
ALT(DEF_ASM_OP1(jmp, 0xeb, 0, 0, OPT_DISP8))

ALT(DEF_ASM_OP1(lcall, 0xff, 3, OPC_MODRM, OPT_EA))
ALT(DEF_ASM_OP1(ljmp, 0xff, 5, OPC_MODRM, OPT_EA))
    DEF_ASM_OP1(ljmpw, 0x66ff, 5, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(ljmpl, 0xff, 5, OPC_MODRM, OPT_EA)

ALT(DEF_ASM_OP1(int, 0xcd, 0, 0, OPT_IM8))
ALT(DEF_ASM_OP1(seto, 0x0f90, 0, OPC_MODRM | OPC_TEST, OPT_REG8 | OPT_EA))
ALT(DEF_ASM_OP1(setob, 0x0f90, 0, OPC_MODRM | OPC_TEST, OPT_REG8 | OPT_EA))
    DEF_ASM_OP2(enter, 0xc8, 0, 0, OPT_IM16, OPT_IM8)
    DEF_ASM_OP0(leave, 0xc9)
    DEF_ASM_OP0(ret, 0xc3)
    DEF_ASM_OP0(retq, 0xc3)
ALT(DEF_ASM_OP1(retq, 0xc2, 0, 0, OPT_IM16))
ALT(DEF_ASM_OP1(ret, 0xc2, 0, 0, OPT_IM16))
    DEF_ASM_OP0(lret, 0xcb)
ALT(DEF_ASM_OP1(lret, 0xca, 0, 0, OPT_IM16))

ALT(DEF_ASM_OP1(jo, 0x70, 0, OPC_TEST, OPT_DISP8))
    DEF_ASM_OP1(loopne, 0xe0, 0, 0, OPT_DISP8)
    DEF_ASM_OP1(loopnz, 0xe0, 0, 0, OPT_DISP8)
    DEF_ASM_OP1(loope, 0xe1, 0, 0, OPT_DISP8)
    DEF_ASM_OP1(loopz, 0xe1, 0, 0, OPT_DISP8)
    DEF_ASM_OP1(loop, 0xe2, 0, 0, OPT_DISP8)
    DEF_ASM_OP1(jecxz, 0x67e3, 0, 0, OPT_DISP8)

     /* float */
     /* specific fcomp handling */
ALT(DEF_ASM_OP0L(fcomp, 0xd8d9, 0, 0))

ALT(DEF_ASM_OP1(fadd, 0xd8c0, 0, OPC_FARITH | OPC_REG, OPT_ST))
ALT(DEF_ASM_OP2(fadd, 0xd8c0, 0, OPC_FARITH | OPC_REG, OPT_ST, OPT_ST0))
ALT(DEF_ASM_OP2(fadd, 0xdcc0, 0, OPC_FARITH | OPC_REG, OPT_ST0, OPT_ST))
ALT(DEF_ASM_OP2(fmul, 0xdcc8, 0, OPC_FARITH | OPC_REG, OPT_ST0, OPT_ST))
ALT(DEF_ASM_OP0L(fadd, 0xdec1, 0, OPC_FARITH))
ALT(DEF_ASM_OP1(faddp, 0xdec0, 0, OPC_FARITH | OPC_REG, OPT_ST))
ALT(DEF_ASM_OP2(faddp, 0xdec0, 0, OPC_FARITH | OPC_REG, OPT_ST, OPT_ST0))
ALT(DEF_ASM_OP2(faddp, 0xdec0, 0, OPC_FARITH | OPC_REG, OPT_ST0, OPT_ST))
ALT(DEF_ASM_OP0L(faddp, 0xdec1, 0, OPC_FARITH))
ALT(DEF_ASM_OP1(fadds, 0xd8, 0, OPC_FARITH | OPC_MODRM, OPT_EA))
ALT(DEF_ASM_OP1(fiaddl, 0xda, 0, OPC_FARITH | OPC_MODRM, OPT_EA))
ALT(DEF_ASM_OP1(faddl, 0xdc, 0, OPC_FARITH | OPC_MODRM, OPT_EA))
ALT(DEF_ASM_OP1(fiadds, 0xde, 0, OPC_FARITH | OPC_MODRM, OPT_EA))

     DEF_ASM_OP0(fucompp, 0xdae9)
     DEF_ASM_OP0(ftst, 0xd9e4)
     DEF_ASM_OP0(fxam, 0xd9e5)
     DEF_ASM_OP0(fld1, 0xd9e8)
     DEF_ASM_OP0(fldl2t, 0xd9e9)
     DEF_ASM_OP0(fldl2e, 0xd9ea)
     DEF_ASM_OP0(fldpi, 0xd9eb)
     DEF_ASM_OP0(fldlg2, 0xd9ec)
     DEF_ASM_OP0(fldln2, 0xd9ed)
     DEF_ASM_OP0(fldz, 0xd9ee)

     DEF_ASM_OP0(f2xm1, 0xd9f0)
     DEF_ASM_OP0(fyl2x, 0xd9f1)
     DEF_ASM_OP0(fptan, 0xd9f2)
     DEF_ASM_OP0(fpatan, 0xd9f3)
     DEF_ASM_OP0(fxtract, 0xd9f4)
     DEF_ASM_OP0(fprem1, 0xd9f5)
     DEF_ASM_OP0(fdecstp, 0xd9f6)
     DEF_ASM_OP0(fincstp, 0xd9f7)
     DEF_ASM_OP0(fprem, 0xd9f8)
     DEF_ASM_OP0(fyl2xp1, 0xd9f9)
     DEF_ASM_OP0(fsqrt, 0xd9fa)
     DEF_ASM_OP0(fsincos, 0xd9fb)
     DEF_ASM_OP0(frndint, 0xd9fc)
     DEF_ASM_OP0(fscale, 0xd9fd)
     DEF_ASM_OP0(fsin, 0xd9fe)
     DEF_ASM_OP0(fcos, 0xd9ff)
     DEF_ASM_OP0(fchs, 0xd9e0)
     DEF_ASM_OP0(fabs, 0xd9e1)
     DEF_ASM_OP0(fninit, 0xdbe3)
     DEF_ASM_OP0(fnclex, 0xdbe2)
     DEF_ASM_OP0(fnop, 0xd9d0)
     DEF_ASM_OP0(fwait, 0x9b)

    /* fp load */
    DEF_ASM_OP1(fld, 0xd9c0, 0, OPC_REG, OPT_ST)
    DEF_ASM_OP1(fldl, 0xd9c0, 0, OPC_REG, OPT_ST)
    DEF_ASM_OP1(flds, 0xd9, 0, OPC_MODRM, OPT_EA)
ALT(DEF_ASM_OP1(fldl, 0xdd, 0, OPC_MODRM, OPT_EA))
    DEF_ASM_OP1(fildl, 0xdb, 0, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fildq, 0xdf, 5, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fildll, 0xdf, 5, OPC_MODRM,OPT_EA)
    DEF_ASM_OP1(fldt, 0xdb, 5, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fbld, 0xdf, 4, OPC_MODRM, OPT_EA)

    /* fp store */
    DEF_ASM_OP1(fst, 0xddd0, 0, OPC_REG, OPT_ST)
    DEF_ASM_OP1(fstl, 0xddd0, 0, OPC_REG, OPT_ST)
    DEF_ASM_OP1(fsts, 0xd9, 2, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fstps, 0xd9, 3, OPC_MODRM, OPT_EA)
ALT(DEF_ASM_OP1(fstl, 0xdd, 2, OPC_MODRM, OPT_EA))
    DEF_ASM_OP1(fstpl, 0xdd, 3, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fist, 0xdf, 2, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fistp, 0xdf, 3, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fistl, 0xdb, 2, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fistpl, 0xdb, 3, OPC_MODRM, OPT_EA)

    DEF_ASM_OP1(fstp, 0xddd8, 0, OPC_REG, OPT_ST)
    DEF_ASM_OP1(fistpq, 0xdf, 7, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fistpll, 0xdf, 7, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fstpt, 0xdb, 7, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(fbstp, 0xdf, 6, OPC_MODRM, OPT_EA)

    /* exchange */
    DEF_ASM_OP0(fxch, 0xd9c9)
ALT(DEF_ASM_OP1(fxch, 0xd9c8, 0, OPC_REG, OPT_ST))

    /* misc FPU */
    DEF_ASM_OP1(fucom, 0xdde0, 0, OPC_REG, OPT_ST )
    DEF_ASM_OP1(fucomp, 0xdde8, 0, OPC_REG, OPT_ST )

    DEF_ASM_OP0L(finit, 0xdbe3, 0, OPC_FWAIT)
    DEF_ASM_OP1(fldcw, 0xd9, 5, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fnstcw, 0xd9, 7, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fstcw, 0xd9, 7, OPC_MODRM | OPC_FWAIT, OPT_EA )
    DEF_ASM_OP0(fnstsw, 0xdfe0)
ALT(DEF_ASM_OP1(fnstsw, 0xdfe0, 0, 0, OPT_EAX ))
ALT(DEF_ASM_OP1(fnstsw, 0xdd, 7, OPC_MODRM, OPT_EA ))
    DEF_ASM_OP1(fstsw, 0xdfe0, 0, OPC_FWAIT, OPT_EAX )
ALT(DEF_ASM_OP0L(fstsw, 0xdfe0, 0, OPC_FWAIT))
ALT(DEF_ASM_OP1(fstsw, 0xdd, 7, OPC_MODRM | OPC_FWAIT, OPT_EA ))
    DEF_ASM_OP0L(fclex, 0xdbe2, 0, OPC_FWAIT)
    DEF_ASM_OP1(fnstenv, 0xd9, 6, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fstenv, 0xd9, 6, OPC_MODRM | OPC_FWAIT, OPT_EA )
    DEF_ASM_OP1(fldenv, 0xd9, 4, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fnsave, 0xdd, 6, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fsave, 0xdd, 6, OPC_MODRM | OPC_FWAIT, OPT_EA )
    DEF_ASM_OP1(frstor, 0xdd, 4, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(ffree, 0xddc0, 4, OPC_REG, OPT_ST )
    DEF_ASM_OP1(ffreep, 0xdfc0, 4, OPC_REG, OPT_ST )
    DEF_ASM_OP1(fxsave, 0x0fae, 0, OPC_MODRM, OPT_EA )
    DEF_ASM_OP1(fxrstor, 0x0fae, 1, OPC_MODRM, OPT_EA )
    /* The *q forms of fxrstor/fxsave use a REX prefix.
       If the operand would use extended registers we would have to modify
       it instead of generating a second one.  Currently that's no
       problem with TCC, we don't use extended registers.  */
    DEF_ASM_OP1(fxsaveq, 0x0fae, 0, OPC_MODRM | OPC_48, OPT_EA )
    DEF_ASM_OP1(fxrstorq, 0x0fae, 1, OPC_MODRM | OPC_48, OPT_EA )

    /* segments */
    DEF_ASM_OP2(arpl, 0x63, 0, OPC_MODRM, OPT_REG16, OPT_REG16 | OPT_EA)
ALT(DEF_ASM_OP2(larw, 0x0f02, 0, OPC_MODRM | OPC_WLX, OPT_REG | OPT_EA, OPT_REG))
    DEF_ASM_OP1(lgdt, 0x0f01, 2, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(lgdtq, 0x0f01, 2, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(lidt, 0x0f01, 3, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(lidtq, 0x0f01, 3, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(lldt, 0x0f00, 2, OPC_MODRM, OPT_EA | OPT_REG)
    DEF_ASM_OP1(lmsw, 0x0f01, 6, OPC_MODRM, OPT_EA | OPT_REG)
ALT(DEF_ASM_OP2(lslw, 0x0f03, 0, OPC_MODRM | OPC_WLX, OPT_EA | OPT_REG, OPT_REG))
    DEF_ASM_OP1(ltr, 0x0f00, 3, OPC_MODRM, OPT_EA | OPT_REG16)
    DEF_ASM_OP1(sgdt, 0x0f01, 0, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(sgdtq, 0x0f01, 0, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(sidt, 0x0f01, 1, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(sidtq, 0x0f01, 1, OPC_MODRM, OPT_EA)
    DEF_ASM_OP1(sldt, 0x0f00, 0, OPC_MODRM, OPT_REG | OPT_EA)
    DEF_ASM_OP1(smsw, 0x0f01, 4, OPC_MODRM, OPT_REG | OPT_EA)
    DEF_ASM_OP1(str, 0x0f00, 1, OPC_MODRM, OPT_REG32 | OPT_EA)
ALT(DEF_ASM_OP1(str, 0x660f00, 1, OPC_MODRM, OPT_REG16))
ALT(DEF_ASM_OP1(str, 0x0f00, 1, OPC_MODRM | OPC_48, OPT_REG64))
    DEF_ASM_OP1(verr, 0x0f00, 4, OPC_MODRM, OPT_REG | OPT_EA)
    DEF_ASM_OP1(verw, 0x0f00, 5, OPC_MODRM, OPT_REG | OPT_EA)
    DEF_ASM_OP0L(swapgs, 0x0f01, 7, OPC_MODRM)

    /* 486 */
    /* bswap can't be applied to 16bit regs */
    DEF_ASM_OP1(bswap, 0x0fc8, 0, OPC_REG, OPT_REG32 )
    DEF_ASM_OP1(bswapl, 0x0fc8, 0, OPC_REG, OPT_REG32 )
    DEF_ASM_OP1(bswapq, 0x0fc8, 0, OPC_REG | OPC_48, OPT_REG64 )

ALT(DEF_ASM_OP2(xaddb, 0x0fc0, 0, OPC_MODRM | OPC_BWLX, OPT_REG, OPT_REG | OPT_EA ))
ALT(DEF_ASM_OP2(cmpxchgb, 0x0fb0, 0, OPC_MODRM | OPC_BWLX, OPT_REG, OPT_REG | OPT_EA ))
    DEF_ASM_OP1(invlpg, 0x0f01, 7, OPC_MODRM, OPT_EA )

    /* pentium */
    DEF_ASM_OP1(cmpxchg8b, 0x0fc7, 1, OPC_MODRM, OPT_EA )

    /* AMD 64 */
    DEF_ASM_OP1(cmpxchg16b, 0x0fc7, 1, OPC_MODRM | OPC_48, OPT_EA )

    /* pentium pro */
ALT(DEF_ASM_OP2(cmovo, 0x0f40, 0, OPC_MODRM | OPC_TEST | OPC_WLX, OPT_REGW | OPT_EA, OPT_REGW))

    DEF_ASM_OP2(fcmovb, 0xdac0, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmove, 0xdac8, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovbe, 0xdad0, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovu, 0xdad8, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovnb, 0xdbc0, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovne, 0xdbc8, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovnbe, 0xdbd0, 0, OPC_REG, OPT_ST, OPT_ST0 )
    DEF_ASM_OP2(fcmovnu, 0xdbd8, 0, OPC_REG, OPT_ST, OPT_ST0 )

    DEF_ASM_OP2(fucomi, 0xdbe8, 0, OPC_REG, OPT_ST, OPT_ST