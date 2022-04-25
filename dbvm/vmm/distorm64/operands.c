
/*
operands.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "../config.h"

#include "operands.h"
#include "x86defs.h"

//#include <stdlib.h> /* For abs only. */

/*
 * SIB decoding is the most confusing part when decoding IA-32 instructions.
 * This explanation should clear up some stuff.
 *
 * ! When base == 5, use EBP as the base register !
 * if (rm == 4) {
 *	if mod == 01, decode SIB byte and ALSO read a 8 bits displacement.
 *	if mod == 10, decode SIB byte and ALSO read a 32 bits displacement.
 *	if mod == 11 <-- EXCEPTION, this is a general-purpose register and mustn't lead to SIB decoding!
 *	; So far so good, now the confusing part comes in with mod == 0 and base=5, but no worry.
 *	if (mod == 00) {
 *	 decode SIB byte WITHOUT any displacement.
 *	 EXCEPTION!!! when base == 5, read a 32 bits displacement, but this time DO NOT use (EBP) BASE at all!
 *	}
 *
 *	NOTE: base could be specify None (no base register) if base==5 and mod==5, but then you also need DISP32.
 * }
 */
static int extract_sib(const uint8_t** code, int* codeLen, _OffsetType* codeOffset,
                       _WString* instructionHex, _WString* operandText,
                       _PrefixState* ps,_DecodeType dt,
                       unsigned int mod, unsigned int sib, _OperandSizeType opSize)
{
	unsigned int scale = 0, index = 0, base = 0, rex = 0;

	const uint8_t* code0 = *code;
	_iflags totalPrefixes = ps->totalPrefixes;

	/*
	 * SIB bits:
	 * |7---6-5----3-2---0|
	 * |SCALE| INDEX| BASE|
	 * |------------------|
	 */

	scale = (sib >> 6) & 3;
	index = (sib >> 3) & 7;
	base = sib & 7;

	if (ps->isREXPrefixValid) rex = *ps->rexpos;

	/*
	 * format= <size ptr> seg: [base + index * scale + disp8/32]
	 * whereas base/index/scale/disp8/32 are ALL optional by specific rules!
	 */

	str_indirection_text(operandText, opSize);
	str_seg_text(operandText, ps, dt);
	chrcat_WS(operandText, OPEN_CHR);

	if (base != 5) {
		if (rex & PREFIX_REX_B) ps->usedPrefixes |= INST_PRE_REX;
		if (ADDR_SIZE_AFFECT(dt, totalPrefixes) == Decode64Bits) str_x86def(operandText, &_BASE64[base + ((rex & PREFIX_REX_B) == PREFIX_REX_B ? REX_GPR_BASE : 0)]);
		else str_x86def(operandText, &_BASE32[base + ((rex & PREFIX_REX_B) == PREFIX_REX_B ? REX_GPR_BASE : 0)]);
	} else if (mod != 0) {
		/*
		 * if base == 5 then you have to decode according to MOD.
		 * mod(00) - disp32.
		 * mod(01) - disp8 + rBP
		 * mod(10) - disp32 + rBP
		 * mod(11) - not possible, it's a general-purpose register.
		 */
		if (rex & PREFIX_REX_B) ps->usedPrefixes |= INST_PRE_REX;
		if (ADDR_SIZE_AFFECT(dt, totalPrefixes) == Decode64Bits) str_x86def(operandText, &_BASE64[5 + ((rex & PREFIX_REX_B) == PREFIX_REX_B ? REX_GPR_BASE : 0)]);
		else str_x86def(operandText, &_BASE32[5 + ((rex & PREFIX_REX_B) == PREFIX_REX_B ? REX_GPR_BASE : 0)]);
	}

	/* In 64 bits the REX prefix might affect the index of the SIB byte. */
	if (rex & PREFIX_REX_X) {
		ps->usedPrefixes |= INST_PRE_REX;
		index += REX_GPR_BASE;
	}
	if (index != 4) { /* In 64 bits decoding mode, if index == R12, it's valid! */
		/* Concat '+' only if we have a base or index. */
		if ((mod != 0) || (base != 5)) chrcat_WS(operandText, PLUS_DISP_CHR);
		if (ADDR_SIZE_AFFECT(dt, totalPrefixes) == Decode64Bits) str_x86def(operandText, &_INDEX64[index]);
		else str_x86def(operandText, &_INDEX32[index]);
		str_x86def(operandText, &_SCALE32[scale]);
	}

	/* Read the displacement if required, according to MOD. */
	switch (mod)
	{
		case 0:
			/*
			 * It might be there's only disp32 left in this case,
			 * so we have to check for index existance(!=4), because otherwise we would have a spare '+'.
			 */

			/* When mod=0 there's no force for disp32, unless base=5. */
			if (base == 5) {
				/* disp32 */
				*codeLen -= sizeof(int32_t);
				if (*codeLen < 0) return FALSE;
				str_hex_sp_dw(instructionHex, RULONG(code0));

				if (index != 4) {
					/* If an index was used make it signed relative address: */
					chrcat_WS(operandText, (RLONG(code0) >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
					str_code_hdw(operandText, abs(RLONG(code0)));
				} else { /* Otherwise it's a disp32 only, so make it absolute. */
					str_code_hdw(operandText, RULONG(code0));
				}

				*code += sizeof(int32_t);
				*codeOffset += sizeof(int32_t);
			}
		break;
		case 1:
			/* disp8 */
			*codeLen -= sizeof(int8_t);
			if (*codeLen < 0) return FALSE;
			str_hex_sp_b(instructionHex, *code0);

			chrcat_WS(operandText, (*(int8_t*)code0 >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
			str_code_hb(operandText, abs(*(int8_t*)code0));

			*code += sizeof(int8_t);
			*codeOffset += sizeof(int8_t);
		break;
		case 2:
			/* disp32 */
			*codeLen -= sizeof(int32_t);
			if (*codeLen < 0) return FALSE;
			str_hex_sp_dw(instructionHex, RULONG(code0));

			/* Signed relative address: */
			chrcat_WS(operandText, (RLONG(code0) >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
			str_code_hdw(operandText, abs(RLONG(code0)));

			*code += sizeof(int32_t);
			*codeOffset += sizeof(int32_t);
		break;
			/* case 3: break; --> It's a general-purpose register. 3rd time -ice cream?! */
	}

	chrcat_WS(operandText, CLOSE_CHR);
	return TRUE;
}

/*
 * This seems to be the hardest part in decoding the operands.
 * If you take a look carefully at Table 2-2. 32-Bit Addressing Forms with the ModR/M Byte,
 * you will understand it's easy to decode the operands.

 * First we check the DT, so we can decide according to which Table in the documentation we are supposed to decode.
 * Then we follow the specific table whether it's 16 bits or 32/64 bits.

 * Don't forget that Operand Size AND Address Size prefixes may change the decoding!

 * Some instructions force the use of RM16 or other specific types, so take it into account.
 */
static int extract_modrm(_CodeInfo* ci,
                         _WString* instructionHex, _WString* operandText, _OpType type,
                         _OperandNumberType opNum, _PrefixState* ps, _DecodeType dt,
                         int* lockableInstruction, unsigned int mod, unsigned int rm,
                         _iflags instFlags, _OperandSizeType opSize)
{
	unsigned int sib = 0, rex = 0;

	const uint8_t* code = ci->code;
	int codeLen = ci->codeLen;
	_OffsetType codeOffset = ci->codeOffset;

	_iflags totalPrefixes = ps->totalPrefixes;

	if (ps->isREXPrefixValid) rex = *ps->rexpos;

	if (mod == 3)	{ /* General-purpose register is handled the same way in 16/32/64 bits decoding modes. */
		switch(type)
		{
			case OT_RFULL_M16:
			case OT_RM_FULL:
				switch (OP_SIZE_AFFECT(dt, totalPrefixes, rex, instFlags))
				{
					case Decode16Bits:
						ps->usedPrefixes |= (totalPrefixes & INST_PRE_OP_SIZE);
						if (rex & PREFIX_REX_B) {
							ps->usedPrefixes |= INST_PRE_REX;
							rm += REX_GPR_BASE;
						}
						str_x86def(operandText, &_REGS16[rm]);
					break;
					case Decode32Bits:
						ps->usedPrefixes |= (totalPrefixes & INST_PRE_OP_SIZE);
						if (rex & PREFIX_REX_B) {
							ps->usedPrefixes |= INST_PRE_REX;
							rm += REX_GPR_BASE;
						}
						str_x86def(operandText, &_REGS32[rm]);
					break;
					case Decode64Bits:
						/* V 1.5.15 - A fix for SMSW RAX which use the REX prefix. */
						if (type == OT_RFULL_M16) ps->usedPrefixes |= (totalPrefixes & INST_PRE_REX);
						/* CALL NEAR/PUSH/POP defaults to 64 bits. --> INST_64BITS, REX isn't required, thus ignored anyways. */
						if (instFlags & INST_PRE_REX) ps->usedPrefixes |= INST_PRE_REX;
						/* V 1.5.14 - include REX is used for REX.B. */
						if (rex & PREFIX_REX_B) {
							ps->usedPrefixes |= INST_PRE_REX;
							rm += REX_GPR_BASE;
						}
						str_x86def(operandText, &_REGS64[rm]);
					break;
				}
			break;
			case OT_RM32:
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				str_x86def(operandText, &_REGS32[rm]);
			break;

			case OT_R32_64_M8:
			/* FALL THROUGH, decode 32 or 64 bits register. */
			case OT_R32_64_M16:
			/* FALL THROUGH, decode 32 or 64 bits register. */
			case OT_RM32_64: /* Take care specifically in MOVNTI/MOVD/CVT's instructions, making it _REG64 with REX or if they are promoted. */
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				/* Is it a promoted instruction? (only INST_64BITS is set and REX isn't required.) */
				if ((dt == Decode64Bits) && ((instFlags & (INST_64BITS | INST_PRE_REX)) == INST_64BITS)) {
					str_x86def(operandText, &_REGS64[rm]);
					break;
				}
				/* Give a chance to REX.W. Because if it was a promoted instruction we don't care about REX.W anyways. */
				if (rex & PREFIX_REX_W) {
					ps->usedPrefixes |= INST_PRE_REX;
					str_x86def(operandText, &_REGS64[rm]);
				} else str_x86def(operandText, &_REGS32[rm]);
			break;
			case OT_RM16_32: /* Ver 1.5.16 - Used only with MOVZXD instruction to support 16 bits operand. */
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				/* Is it 16 bits operand size? */
				if (ps->totalPrefixes & INST_PRE_OP_SIZE) {
					ps->usedPrefixes |= INST_PRE_OP_SIZE;
					str_x86def(operandText, &_REGS16[rm]);
				} else str_x86def(operandText, &_REGS32[rm]);
			break;
			case OT_RM16:
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				str_x86def(operandText, &_REGS16[rm]);
			break;
			case OT_RM8:
				if (ps->isREXPrefixValid) {
					ps->usedPrefixes |= INST_PRE_REX;
					str_x86def(operandText, &_REGS8_REX[rm + ((rex & PREFIX_REX_B) == PREFIX_REX_B ? REX_GPR_BASE : 0)]);
				} else str_x86def(operandText, &_REGS8[rm]);
			break;
			case OT_MM32:
			case OT_MM64:
				/* MMX doesn't support extended registers. */
				str_x86def(operandText, &_REGSMMX[rm]);
			break;
			case OT_XMM16:
			case OT_XMM32:
			case OT_XMM64:
			case OT_XMM128:
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				str_x86def(operandText, &_REGSSSE[rm]);
			break;
			case OT_R32_M8:
			/* FALL THROUGH, decode 32 bits register. */
			case OT_R32_M16:
				if (rex & PREFIX_REX_B) {
					ps->usedPrefixes |= INST_PRE_REX;
					rm += REX_GPR_BASE;
				}
				if (dt == Decode16Bits) str_x86def(operandText, &_REGS16[rm]);
				else str_x86def(operandText, &_REGS32[rm]);
			break;
			default: return FALSE;
		}
		/*
		 * It's ok if we don't update the pointers parameters when we will return FALSE, because
		 * they are to be ignored anyways.
		 */
		ci->code = code;
		ci->codeLen = codeLen;
		ci->codeOffset = codeOffset;

		return TRUE;
	}

	if (ADDR_SIZE_AFFECT(dt, totalPrefixes) == Decode16Bits) {
		/* Decoding according to Table 2-1. (16 bits) */
		switch (mod)
		{
			case 0x00: /* Indirection */
				ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
				if ((opNum == ONT_1) && (totalPrefixes & INST_PRE_LOCK)) *lockableInstruction = 1;

				if (rm == 6) {
					/* 6 is a special case - only 16 bits displacement. */
					codeLen -= sizeof(int16_t);
					if (codeLen < 0) return FALSE;
					str_hex_sp_w(instructionHex, RUSHORT(code));

					str_indirection_text(operandText, opSize);
					str_seg_text(operandText, ps, dt);
					chrcat_WS(operandText, OPEN_CHR);
					str_code_hw(operandText, RUSHORT(code));
					chrcat_WS(operandText, CLOSE_CHR);

					code += sizeof(int16_t);
					codeOffset += sizeof(int16_t);
				} else {
					str_indirection_text(operandText, opSize);
					str_seg_text(operandText, ps, dt);
					str_x86def(operandText, &_MODS16[rm]);
					chrcat_WS(operandText, CLOSE_CHR);
				}
			break;
			case 0x01: /* 8 bits displacement + indirection */
				ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
				if ((opNum == ONT_1) && (totalPrefixes & INST_PRE_LOCK)) *lockableInstruction = 1;

				codeLen -= sizeof(int8_t);
				if (codeLen < 0) return FALSE;
				str_hex_sp_b(instructionHex, *code);

				str_indirection_text(operandText, opSize);
				str_seg_text(operandText, ps, dt);
				str_x86def(operandText, &_MODS16[rm]);
				
				chrcat_WS(operandText, (*(int8_t*)code >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
				str_code_hb(operandText, abs(*(int8_t*)code));
				chrcat_WS(operandText, CLOSE_CHR);

				code += sizeof(int8_t);
				codeOffset += sizeof(int8_t);
			break;
			case 0x02: /* 16 bits displacement + indirection */
				ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
				if ((opNum == ONT_1) && (totalPrefixes & INST_PRE_LOCK)) *lockableInstruction = 1;

				codeLen -= sizeof(int16_t);
				if (codeLen < 0) return FALSE;
				str_hex_sp_w(instructionHex, RUSHORT(code));

				str_indirection_text(operandText, opSize);
				str_seg_text(operandText, ps, dt);
				str_x86def(operandText, &_MODS16[rm]);
				/* Signed relative address: */
				chrcat_WS(operandText, (RSHORT(code) >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
				str_code_hw(operandText, abs(RSHORT(code)));
				chrcat_WS(operandText, CLOSE_CHR);

				code += sizeof(int16_t);
				codeOffset += sizeof(int16_t);
			break;
		}
	} else { /* Decode32Bits or Decode64Bits! */
		/*
		* Remember that from 32/64 bits ModR/M byte a SIB byte could follow!
		* Decodes 64 bits now as well.
		*/
		switch (mod)
		{
			case 0x00: /* Indirection */
				ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
				if ((opNum == ONT_1) && (totalPrefixes & INST_PRE_LOCK)) *lockableInstruction = 1;

				if (rm == 4) {
					/* 4 is a special case - SIB byte follows! */

					/* Skip SIB byte. */
					if (--codeLen < 0) return FALSE;
					sib = *code;
					str_hex_b(instructionHex, sib);

					code += sizeof(int8_t);
					codeOffset += sizeof(int8_t);

					if (!extract_sib(&code, &codeLen, &codeOffset, instructionHex, operandText, ps, dt, mod, sib, opSize)) return FALSE;
				} else if (rm == 5) {
					ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
					/* 5 is a special case - only 32 bits displacement. */
					codeLen -= sizeof(int32_t);
					if (codeLen < 0) return FALSE;
					str_hex_sp_dw(instructionHex, RULONG(code));

					str_indirection_text(operandText, opSize);
					str_seg_text(operandText, ps, dt);
					chrcat_WS(operandText, OPEN_CHR);
					if (dt == Decode64Bits) {
						/* In 64 bits decoding mode depsite of the address size, a RIP-relative address it is. */
						strcat_WSN(operandText, REG_RIP_TEXT);
						/* Make it a signed relative address: */
						chrcat_WS(operandText, (RLONG(code) >= 0) ? PLUS_DISP_CHR : MINUS_DISP_CHR);
						str_code_hdw(operandText, abs(RLONG(code)));
					} else {
						/* Absolute address: */
						str_code_hdw(operandText, RULONG(code));
					}
					chrcat_WS(operandText, CLOSE_CHR);

					code += sizeof(int32_t);
					codeOffset += sizeof(int32_t);
				} else {
					str_indirection_text(operandText, opSize);
					str_seg_text(operandText, ps, dt);
					if (rex & PREFIX_REX_B) {
						ps->usedPrefixes |= INST_PRE_REX;
						rm += REX_GPR_BASE;
					}
					if (ADDR_SIZE_AFFECT(dt, totalPrefixes) == Decode64Bits) str_x86def(operandText, &_MODS64[rm]);
					else str_x86def(operandText, &_MODS32[rm]);
					chrcat_WS(operandText, CLOSE_CHR);
				}
			break;
			case 0x01: /* 8 bits displacement + indirection */
				ps->usedPrefixes |= (totalPrefixes & INST_PRE_ADDR_SIZE);
				if ((opNum == ONT_1) && (totalPrefixes & INST_PRE_LOCK)) *lockableInstruction = 1;

				if (rm == 4) {
					/* 4 is a special case - SIB byte + disp8 follows! */

					/* Skip SIB byte. */
					if (--codeLen < 0) return FALSE;
					sib = *code;
					str_hex_b(instructionHex, sib);

					code += sizeof(int8_t);
					codeOffset += sizeof(int8_t);
					if (!extract_sib(&code, &codeLen, &codeOffset, instructionHex, operandText, ps, dt, mod, sib, opSize)) return FALSE;
				} else {
					codeLen -= sizeof(int8_t);
					if (codeLen < 0) return FALSE;
					str_hex_sp_b(instructionHex, *code);

					str_indirection_text(operandText, opSize);