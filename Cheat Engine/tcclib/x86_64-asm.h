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

A