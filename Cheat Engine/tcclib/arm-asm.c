/*
 *  ARM specific functions for TCC assembler
 *
 *  Copyright (c) 2001, 2002 Fabrice Bellard
 *  Copyright (c) 2020 Danny Milosavljevic
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef TARGET_DEFS_ONLY

#define CONFIG_TCC_ASM
#define NB_ASM_REGS 16

ST_FUNC void g(int c);
ST_FUNC void gen_le16(int c);
ST_FUNC void gen_le32(int c);

/*************************************************************/
#else
/*************************************************************/

#define USING_GLOBALS
#include "tcc.h"

enum {
    OPT_REG32,
    OPT_REGSET32,
    OPT_IM8,
    OPT_IM8N,
    OPT_IM32,
};
#define OP_REG32  (1 << OPT_REG32)
#define OP_REG    (OP_REG32)
#define OP_IM32   (1 << OPT_IM32)
#define OP_IM8   (1 << OPT_IM8)
#define OP_IM8N   (1 << OPT_IM8N)
#define OP_REGSET32  (1 << OPT_REGSET32)

typedef struct Operand {
    uint32_t type;
    union {
        uint8_t reg;
        uint16_t regset;
        ExprValue e;
    };
} Operand;

/* Parse a text containing operand and store the result in OP */
static void parse_operand(TCCState *s1, Operand *op)
{
    ExprValue e;
    int8_t reg;
    uint16_t regset = 0;

    op->type = 0;

    if (tok == '{') { // regset literal
        next(); // skip '{'
        while (tok != '}' && tok != TOK_EOF) {
            reg = asm_parse_regvar(tok);
            if (reg == -1) {
                expect("register");
                return;
            } else
                next(); // skip register name

            if ((1 << reg) < regset)
                tcc_warning("registers will be processed in ascending order by hardware--but are not specified in ascending order here");
            regset |= 1 << reg;
            if (tok != ',')
                break;
            next(); // skip ','
        }
        if (tok != '}')
            expect("'}'");
        next(); // skip '}'
        if (regset == 0) {
            // ARM instructions don't support empty regset.
            tcc_error("empty register list is not supported");
        } else {
            op->type = OP_REGSET32;
            op->regset = regset;
        }
    } else if (tok == '#' || tok == '$') {
        /* constant value */
        next(); // skip '#' or '$'
        asm_expr(s1, &e);
        op->type = OP_IM32;
        op->e = e;
        if (!op->e.sym) {
            if ((int) op->e.v < 0 && (int) op->e.v >= -255)
                op->type = OP_IM8N;
            else if (op->e.v == (uint8_t)op->e.v)
                op->type = OP_IM8;
        } else
            expect("constant");
    } else if ((reg = asm_parse_regvar(tok)) != -1) {
        next(); // skip register name
        op->type = OP_REG32;
        op->reg = (uint8_t) reg;
    } else
        expect("operand");
}

/* XXX: make it faster ? */
ST_FUNC void g(int c)
{
    int ind1;
    if (nocode_wanted)
        return;
    ind1 = ind + 1;
    if (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind] = c;
    ind = ind1;
}

ST_FUNC void gen_le16 (int i)
{
    g(i);
    g(i>>8);
}

ST_FUNC void gen_le32 (int i)
{
    int ind1;
    if (nocode_wanted)
        return;
    ind1 = ind + 4;
    if (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind++] = i & 0xFF;
    cur_text_section->data[ind++] = (i >> 8) & 0xFF;
    cur_text_section->data[ind++] = (i >> 16) & 0xFF;
    cur_text_section->data[ind++] = (i >> 24) & 0xFF;
}

ST_FUNC void gen_expr32(ExprValue *pe)
{
    gen_le32(pe->v);
}

static uint32_t condition_code_of_token(int token) {
    if (token < TOK_ASM_nopeq) {
        expect("instruction");
        return 0;
    } else
        return (token - TOK_ASM_nopeq) & 15;
}

static void asm_emit_opcode(int token, uint32_t opcode) {
    gen_le32((condition_code_of_token(token) << 28) | opcode);
}

static void asm_nullary_opcode(int token)
{
    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_nopeq:
        asm_emit_opcode(token, 0xd << 21); // mov r0, r0
        break;
    case TOK_ASM_wfeeq:
        asm_emit_opcode(token, 0x320f002);
    case TOK_ASM_wfieq:
        asm_emit_opcode(token, 0x320f003);
        break;
    default:
        expect("nullary instruction");
    }
}

static void asm_unary_opcode(TCCState *s1, int token)
{
    Operand op;
    parse_operand(s1, &op);

    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_swieq:
        if (op.type != OP_IM8)
            expect("immediate 8-bit unsigned integer");
        else {
            /* Note: Dummy operand (ignored by processor): ARM ref documented 0...255, ARM instruction set documented 24 bit */
            asm_emit_opcode(token, (0xf << 24) | op.e.v);
        }
        break;
    default:
        expect("unary instruction");
    }
}

static void asm_binary_opcode(TCCState *s1, int token)
{
    Operand ops[2];
    Operand rotation;
    uint32_t encoded_rotation = 0;
    uint64_t amount;
    parse_operand(s1, &ops[0]);
    if (tok == ',')
        next();
    else
        expect("','");
    parse_operand(s1, &ops[1]);
    if (ops[0].type != OP_REG32) {
        expect("(destination operand) register");
        return;
    }

    if (ops[0].reg == 15) {
        tcc_error("'%s' does not support 'pc' as operand", get_tok_str(token, NULL));
        return;
    }

    if (ops[0].reg == 13)
        tcc_warning("Using 'sp' as operand with '%s' is deprecated by ARM", get_tok_str(token, NULL));

    if (ops[1].type != OP_REG32) {
        switch (ARM_INSTRUCTION_GROUP(token)) {
        case TOK_ASM_movteq:
        case TOK_ASM_movweq:
            if (ops[1].type == OP_IM8 || ops[1].type == OP_IM8N || ops[1].type == OP_IM32) {
                if (ops[1].e.v >= 0 && ops[1].e.v <= 0xFFFF) {
                    uint16_t immediate_value = ops[1].e.v;
                    switch (ARM_INSTRUCTION_GROUP(token)) {
                    case TOK_ASM_movteq:
                        asm_emit_opcode(token, 0x3400000 | (ops[0].reg << 12) | (immediate_value & 0xF000) << 4 | (immediate_value & 0xFFF));
                        break;
                    case TOK_ASM_movweq:
                        asm_emit_opcode(token, 0x3000000 | (ops[0].reg << 12) | (immediate_value & 0xF000) << 4 | (immediate_value & 0xFFF));
                        break;
                    }
                } else
                    expect("(source operand) immediate 16 bit value");
            } else
                expect("(source operand) immediate");
            break;
        default:
            expect("(source operand) register");
        }
        return;
    }

    if (ops[1].reg == 15) {
        tcc_error("'%s' does not support 'pc' as operand", get_tok_str(token, NULL));
        return;
    }

    if (ops[1].reg == 13)
        tcc_warning("Using 'sp' as operand with '%s' is deprecated by ARM", get_tok_str(token, NULL));

    if (tok == ',') {
        next(); // skip ','
        if (tok == TOK_ASM_ror) {
            next(); // skip 'ror'
            parse_operand(s1, &rotation);
            if (rotation.type != OP_IM8) {
                expect("immediate value for rotation");
                return;
            } else {
                amount = rotation.e.v;
                switch (amount) {
                case 8:
                    encoded_rotation = 1 << 10;
                    break;
                case 16:
                    encoded_rotation = 2 << 10;
                    break;
                case 24:
                    encoded_rotation = 3 << 10;
                    break;
                default:
                    expect("'8' or '16' or '24'");
                    return;
                }
            }
        }
    }
    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_clzeq:
        if (encoded_rotation)
            tcc_error("clz does not support rotation");
        asm_emit_opcode(token, 0x16f0f10 | (ops[0].reg << 12) | ops[1].reg);
        break;
    case TOK_ASM_sxtbeq:
        asm_emit_opcode(token, 0x6af0070 | (ops[0].reg << 12) | ops[1].reg | encoded_rotation);
        break;
    case TOK_ASM_sxtheq:
        asm_emit_opcode(token, 0x6bf0070 | (ops[0].reg << 12) | ops[1].reg | encoded_rotation);
        break;
    case TOK_ASM_uxtbeq:
        asm_emit_opcode(token, 0x6ef0070 | (ops[0].reg << 12) | ops[1].reg | encoded_rotation);
        break;
    case TOK_ASM_uxtheq:
        asm_emit_opcode(token, 0x6ff0070 | (ops[0].reg << 12) | ops[1].reg | encoded_rotation);
        break;
    default:
        expect("binary instruction");
    }
}

/* data processing and single data transfer instructions only */
#define ENCODE_RN(register_index) ((register_index) << 16)
#define ENCODE_RD(register_index) ((register_index) << 12)
#define ENCODE_SET_CONDITION_CODES (1 << 20)

/* Note: For data processing instructions, "1" means immediate.
   Note: For single data transfer instructions, "0" means immediate. */
#define ENCODE_IMMEDIATE_FLAG (1 << 25)

#define ENCODE_BARREL_SHIFTER_SHIFT_BY_REGISTER (1 << 4)
#define ENCODE_BARREL_SHIFTER_MODE_LSL (0 << 5)
#define ENCODE_BARREL_SHIFTER_MODE_LSR (1 << 5)
#define ENCODE_BARREL_SHIFTER_MODE_ASR (2 << 5)
#define ENCODE_BARREL_SHIFTER_MODE_ROR (3 << 5)
#define ENCODE_BARREL_SHIFTER_REGISTER(register_index) ((register_index) << 8)
#define ENCODE_BARREL_SHIFTER_IMMEDIATE(value) ((value) << 7)

static void asm_block_data_transfer_opcode(TCCState *s1, int token)
{
    uint32_t opcode;
    int op0_exclam = 0;
    Operand ops[2];
    int nb_ops = 1;
    parse_operand(s1, &ops[0]);
    if (tok == '!') {
        op0_exclam = 1;
        next(); // skip '!'
    }
    if (tok == ',') {
        next(); // skip comma
        parse_operand(s1, &ops[1]);
        ++nb_ops;
    }
    if (nb_ops < 1) {
        expect("at least one operand");
        return;
    } else if (ops[nb_ops - 1].type != OP_REGSET32) {
        expect("(last operand) register list");
        return;
    }

    // block data transfer: 1 0 0 P U S W L << 20 (general case):
    // operands:
    //   Rn: bits 19...16 base register
    //   Register List: bits 15...0

    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_pusheq: // TODO: Optimize 1-register case to: str ?, [sp, #-4]!
        // Instruction: 1 I=0 P=1 U=0 S=0 W=1 L=0 << 20, op 1101
        //   operands:
        //      Rn: base register
        //      Register List: bits 15...0
        if (nb_ops != 1)
            expect("exactly one operand");
        else
            asm_emit_opcode(token, (0x92d << 16) | ops[0].regset); // TODO: base register ?
        break;
    case TOK_ASM_popeq: // TODO: Optimize 1-register case to: ldr ?, [sp], #4
        // Instruction: 1 I=0 P=0 U=1 S=0 W=0 L=1 << 20, op 1101
        //   operands:
        //      Rn: base register
        //      Register List: bits 15...0
        if (nb_ops != 1)
            expect("exactly one operand");
        else
            asm_emit_opcode(token, (0x8bd << 16) | ops[0].regset); // TODO: base register ?
        break;
    case TOK_ASM_stmdaeq:
    case TOK_ASM_ldmdaeq:
    case TOK_ASM_stmeq:
    case TOK_ASM_ldmeq:
    case TOK_ASM_stmiaeq:
    case TOK_ASM_ldmiaeq:
    case TOK_ASM_stmdbeq:
    case TOK_ASM_ldmdbeq:
    case TOK_ASM_stmibeq:
    case TOK_ASM_ldmibeq:
        switch (ARM_INSTRUCTION_GROUP(token)) {
        case TOK_ASM_stmdaeq: // post-decrement store
            opcode = 0x80 << 20;
            break;
        case TOK_ASM_ldmdaeq: // post-decrement load
            opcode = 0x81 << 20;
            break;
        case TOK_ASM_stmeq: // post-increment store
        case TOK_ASM_stmiaeq: // post-increment store
            opcode = 0x88 << 20;
            break;
        case TOK_ASM_ldmeq: // post-increment load
        case TOK_ASM_ldmiaeq: // post-increment load
            opcode = 0x89 << 20;
            break;
        case TOK_ASM_stmdbeq: // pre-decrement store
            opcode = 0x90 << 20;
            break;
        case TOK_ASM_ldmdbeq: // pre-decrement load
            opcode = 0x91 << 20;
            break;
        case TOK_ASM_stmibeq: // pre-increment store
            opcode = 0x98 << 20;
            break;
        case TOK_ASM_ldmibeq: // pre-increment load
            opcode = 0x99 << 20;
            break;
        default:
            tcc_error("internal error: This place should not be reached (fallback in asm_block_data_transfer_opcode)");
        }
        // operands:
        //    Rn: first operand
        //    Register List: lower bits
        if (nb_ops != 2)
            expect("exactly two operands");
        else if (ops[0].type != OP_REG32)
            expect("(first operand) register");
        else {
            if (op0_exclam)
                opcode |= 1 << 21; // writeback
            asm_emit_opcode(token, opcode | ENCODE_RN(ops[0].reg) | ops[1].regset);
        }
        break;
    default:
        expect("block data transfer instruction");
    }
}

/* Parses shift directive and returns the parts that would have to be set in the opcode because of it.
   Does not encode the actual shift amount.
   It's not an error if there is no shift directive.

   NB_SHIFT: will be set to 1 iff SHIFT is filled.  Note that for rrx, there's no need to fill SHIFT.
   SHIFT: will be filled in with the shift operand to use, if any. */
static uint32_t asm_parse_optional_shift(TCCState* s1, int* nb_shift, Operand* shift)
{
    uint32_t opcode = 0;
    *nb_shift = 0;
    switch (tok) {
    case TOK_ASM_asl:
    case TOK_ASM_lsl:
    case TOK_ASM_asr:
    case TOK_ASM_lsr:
    case TOK_ASM_ror:
        switch (tok) {
        case TOK_ASM_asl:
            /* fallthrough */
        case TOK_ASM_lsl:
            opcode = ENCODE_BARREL_SHIFTER_MODE_LSL;
            break;
        case TOK_ASM_asr:
            opcode = ENCODE_BARREL_SHIFTER_MODE_ASR;
            break;
        case TOK_ASM_lsr:
            opcode = ENCODE_BARREL_SHIFTER_MODE_LSR;
            break;
        case TOK_ASM_ror:
            opcode = ENCODE_BARREL_SHIFTER_MODE_ROR;
            break;
        }
        next();
        parse_operand(s1, shift);
        *nb_shift = 1;
        break;
    case TOK_ASM_rrx:
        next();
        opcode = ENCODE_BARREL_SHIFTER_MODE_ROR;
        break;
    }
    return opcode;
}

static uint32_t asm_encode_shift(Operand* shift)
{
    uint64_t amount;
    uint32_t operands = 0;
    switch (shift->type) {
    case OP_REG32:
        if (shift->reg == 15)
            tcc_error("r15 cannot be used as a shift count");
        else {
            operands = ENCODE_BARREL_SHIFTER_SHIFT_BY_REGISTER;
            operands |= ENCODE_BARREL_SHIFTER_REGISTER(shift->reg);
        }
        break;
    case OP_IM8:
        amount = shift->e.v;
        if (amount > 0 && amount < 32)
            operands = ENCODE_BARREL_SHIFTER_IMMEDIATE(amount);
        else
            tcc_error("shift count out of range");
        break;
    default:
        tcc_error("unknown shift amount");
    }
    return operands;
}

static void asm_data_processing_opcode(TCCState *s1, int token)
{
    Operand ops[3];
    int nb_ops;
	Operand shift;// = {}; //cheat engine modification (won't compile in vs2017)
    int nb_shift = 0;
    uint32_t operands = 0;

	shift.type = 0; //cheat engine modification (in vs2017: set to 0)
	

    /* modulo 16 entries per instruction for the different condition codes */
    uint32_t opcode_idx = (ARM_INSTRUCTION_GROUP(token) - TOK_ASM_andeq) >> 4;
    uint32_t opcode_nos = opcode_idx >> 1; // without "s"; "OpCode" in ARM docs

    for (nb_ops = 0; nb_ops < sizeof(ops)/sizeof(ops[0]); ) {
        if (tok == TOK_ASM_asl || tok == TOK_ASM_lsl || tok == TOK_ASM_lsr || tok == TOK_ASM_asr || tok == TOK_ASM_ror || tok == TOK_ASM_rrx)
            break;
        parse_operand(s1, &ops[nb_ops]);
        ++nb_ops;
        if (tok != ',')
            break;
        next(); // skip ','
    }
    if (tok == ',')
        next();
    operands |= asm_parse_optional_shift(s1, &nb_shift, &shift);
    if (nb_ops < 2)
        expect("at least two operands");
    else if (nb_ops == 2) {
        memcpy(&ops[2], &ops[1], sizeof(ops[1])); // move ops[2]
        memcpy(&ops[1], &ops[0], sizeof(ops[0])); // ops[1] was implicit
        nb_ops = 3;
    } else if (nb_ops == 3) {
        if (opcode_nos == 0xd || opcode_nos == 0xf || opcode_nos == 0xa || opcode_nos == 0xb || opcode_nos == 0x8 || opcode_nos == 0x9) { // mov, mvn, cmp, cmn, tst, teq
            tcc_error("'%s' cannot be used with three operands", get_tok_str(token, NULL));
            return;
        }
    }
    if (nb_ops != 3) {
        expect("two or three operands");
        return;
    } else {
        uint32_t opcode = 0;
        uint32_t immediate_value;
        uint8_t half_immediate_rotation;
        if (nb_shift && shift.type == OP_REG32) {
            if ((ops[0].type == OP_REG32 && ops[0].reg == 15) ||
                (ops[1].type == OP_REG32 && ops[1].reg == 15)) {
                tcc_error("Using the 'pc' register in data processing instructions that have a register-controlled shift is not implemented by ARM");
                return;
            }
        }

        // data processing (general case):
        // operands:
        //   Rn: bits 19...16 (first operand)
        //   Rd: bits 15...12 (destination)
        //   Operand2: bits 11...0 (second operand);  depending on I that's either a register or an immediate
        // operator:
        //   bits 24...21: "OpCode"--see below

        /* operations in the token list are ordered by opcode */
        opcode = opcode_nos << 21; // drop "s"
        if (ops[0].type != OP_REG32)
            expect("(destination operand) register");
        else if (opcode_nos == 0xa || opcode_nos == 0xb || opcode_nos == 0x8 || opcode_nos == 0x9) // cmp, cmn, tst, teq
            operands |= ENCODE_SET_CONDITION_CODES; // force S set, otherwise it's a completely different instruction.
        else
            operands |= ENCODE_RD(ops[0].reg);
        if (ops[1].type != OP_REG32)
            expect("(first source operand) register");
        else if (!(opcode_nos == 0xd || opcode_nos == 0xf)) // not: mov, mvn (those have only one source operand)
            operands |= ENCODE_RN(ops[1].reg);
        switch (ops[2].type) {
        case OP_REG32:
            operands |= ops[2].reg;
            break;
        case OP_IM8:
        case OP_IM32:
            operands |= ENCODE_IMMEDIATE_FLAG;
            immediate_value = ops[2].e.v;
            for (half_immediate_rotation = 0; half_immediate_rotation < 16; ++half_immediate_rotation) {
                if (immediate_value >= 0x00 && immediate_value < 0x100)
                    break;
                // rotate left by two
                immediate_value = ((immediate_value & 0x3FFFFFFF) << 2) | ((immediate_value & 0xC0000000) >> 30);
            }
            if (half_immediate_rotation >= 16) {
                /* fallthrough */
            } else {
                operands |= immediate_value;
                operands |= half_immediate_rotation << 8;
                break;
            }
        case OP_IM8N: // immediate negative value
            operands |= ENCODE_IMMEDIATE_FLAG;
            immediate_value = ops[2].e.v;
            /* Instruction swapping:
               0001 = EOR - Rd:= Op1 EOR Op2     -> difficult
               0011 = RSB - Rd:= Op2 - Op1       -> difficult
               0111 = RSC - Rd:= Op2 - Op1 + C   -> difficult
               1000 = TST - CC on: Op1 AND Op2   -> difficult
               1001 = TEQ - CC on: Op1 EOR Op2   -> difficult
               1100 = ORR - Rd:= Op1 OR Op2      -> difficult
            */
            switch (opcode_nos) {
            case 0x0: // AND - Rd:= Op1 AND Op2
                opcode = 0xe << 21; // BIC
                immediate_value = ~immediate_value;
                break;
            case 0x2: // SUB - Rd:= Op1 - Op2
                opcode = 0x4 << 21; // ADD
                immediate_value = -immediate_value;
                break;
            case 0x4: // ADD - Rd:= Op1 + Op2
                opcode = 0x2 << 21; // SUB
                immediate_value = -immediate_value;
                break;
            case 0x5: // ADC - Rd:= Op1 + Op2 + C
                opcode = 0x6 << 21; // SBC
                immediate_value = ~immediate_value;
                break;
            case 0x6: // SBC - Rd:= Op1 - Op2 + C
                opcode = 0x5 << 21; // ADC
                immediate_value = ~immediate_value;
                break;
            case 0xa: // CMP - CC on: Op1 - Op2
                opcode = 0xb << 21; // CMN
                immediate_value = -immediate_value;
                break;
            case 0xb: // CMN - CC on: Op1 + Op2
                opcode = 0xa << 21; // CMP
                immediate_value = -immediate_value;
                break;
            case 0xd: // MOV - Rd:= Op2
                opcode = 0xf << 21; // MVN
                immediate_value = ~immediate_value;
                break;
            case 0xe: // BIC - Rd:= Op1 AND NOT Op2
                opcode = 0x0 << 21; // AND
                immediate_value = ~immediate_value;
                break;
            case 0xf: // MVN - Rd:= NOT Op2
                opcode = 0xd << 21; // MOV
                immediate_value = ~immediate_value;
                break;
            default:
                tcc_error("cannot use '%s' with a negative immediate value", get_tok_str(token, NULL));
            }
            for (half_immediate_rotation = 0; half_immediate_rotation < 16; ++half_immediate_rotation) {
                if (immediate_value >= 0x00 && immediate_value < 0x100)
                    break;
                // rotate left by two
                immediate_value = ((immediate_value & 0x3FFFFFFF) << 2) | ((immediate_value & 0xC0000000) >> 30);
            }
            if (half_immediate_rotation >= 16) {
                immediate_value = ops[2].e.v;
                tcc_error("immediate value 0x%X cannot be encoded into ARM immediate", (unsigned) immediate_value);
                return;
            }
            operands |= immediate_value;
            operands |= half_immediate_rotation << 8;
            break;
        default:
            expect("(second source operand) register or immediate value");
        }

        if (nb_shift) {
            if (operands & ENCODE_IMMEDIATE_FLAG)
                tcc_error("immediate rotation not implemented");
            else
                operands |= asm_encode_shift(&shift);
        }

        /* S=0 and S=1 entries alternate one after another, in that order */
        opcode |= (opcode_idx & 1) ? ENCODE_SET_CONDITION_CODES : 0;
        asm_emit_opcode(token, opcode | operands);
    }
}

static void asm_shift_opcode(TCCState *s1, int token)
{
    Operand ops[3];
    int nb_ops;
    int definitely_neutral = 0;
    uint32_t opcode = 0xd << 21; // MOV
    uint32_t operands = 0;

    for (nb_ops = 0; nb_ops < sizeof(ops)/sizeof(ops[0]); ++nb_ops) {
        parse_operand(s1, &ops[nb_ops]);
        if (tok != ',') {
            ++nb_ops;
            break;
        }
        next(); // skip ','
    }
    if (nb_ops < 2) {
        expect("at least two operands");
        return;
    }

    if (ops[0].type != OP_REG32) {
        expect("(destination operand) register");
        return;
    } else
        operands |= ENCODE_RD(ops[0].reg);

    if (nb_ops == 2) {
        switch (ARM_INSTRUCTION_GROUP(token)) {
        case TOK_ASM_rrxseq:
            opcode |= ENCODE_SET_CONDITION_CODES;
            /* fallthrough */
        case TOK_ASM_rrxeq:
            if (ops[1].type == OP_REG32) {
                operands |= ops[1].reg;
                operands |= ENCODE_BARREL_SHIFTER_MODE_ROR;
                asm_emit_opcode(token, opcode | operands);
            } else
                tcc_error("(first source operand) register");
            return;
        default:
            memcpy(&ops[2], &ops[1], sizeof(ops[1])); // move ops[2]
            memcpy(&ops[1], &ops[0], sizeof(ops[0])); // ops[1] was implicit
            nb_ops = 3;
        }
    }
    if (nb_ops != 3) {
        expect("two or three operands");
        return;
    }

    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_lslseq:
    case TOK_ASM_lsrseq:
    case TOK_ASM_asrseq:
    case TOK_ASM_rorseq:
        opcode |= ENCODE_SET_CONDITION_CODES;
        break;
    }

    switch (ops[1].type) {
    case OP_REG32:
        operands |= ops[1].reg;
        break;
    case OP_IM8:
        operands |= ENCODE_IMMEDIATE_FLAG;
        operands |= ops[1].e.v;
        break;
    }

    switch (ops[2].type) {
    case OP_REG32:
        if ((ops[0].type == OP_REG32 && ops[0].reg == 15) ||
            (ops[1].type == OP_REG32 && ops[1].reg == 15)) {
            tcc_error("Using the 'pc' register in data processing instructions that have a register-controlled shift is not implemented by ARM");
        }
        operands |= asm_encode_shift(&ops[2]);
        break;
    case OP_IM8:
        if (ops[2].e.v)
            operands |= asm_encode_shift(&ops[2]);
        else
            definitely_neutral = 1;
        break;
    }

    if (!definitely_neutral) switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_lslseq:
    case TOK_ASM_lsleq:
        operands |= ENCODE_BARREL_SHIFTER_MODE_LSL;
        break;
    case TOK_ASM_lsrseq:
    case TOK_ASM_lsreq:
        operands |= ENCODE_BARREL_SHIFTER_MODE_LSR;
        break;
    case TOK_ASM_asrseq:
    case TOK_ASM_asreq:
        operands |= ENCODE_BARREL_SHIFTER_MODE_ASR;
        break;
    case TOK_ASM_rorseq:
    case TOK_ASM_roreq:
        operands |= ENCODE_BARREL_SHIFTER_MODE_ROR;
        break;
    default:
        expect("shift instruction");
        return;
    }
    asm_emit_opcode(token, opcode | operands);
}

static void asm_multiplication_opcode(TCCState *s1, int token)
{
    Operand ops[4];
    int nb_ops = 0;
    uint32_t opcode = 0x90;

    for (nb_ops = 0; nb_ops < sizeof(ops)/sizeof(ops[0]); ++nb_ops) {
        parse_operand(s1, &ops[nb_ops]);
        if (tok != ',') {
            ++nb_ops;
            break;
        }
        next(); // skip ','
    }
    if (nb_ops < 2)
        expect("at least two operands");
    else if (nb_ops == 2) {
        switch (ARM_INSTRUCTION_GROUP(token)) {
        case TOK_ASM_mulseq:
        case TOK_ASM_muleq:
            memcpy(&ops[2], &ops[0], sizeof(ops[1])); // ARM is actually like this!
            break;
        default:
            expect("at least three operands");
            return;
        }
        nb_ops = 3;
    }

    // multiply (special case):
    // operands:
    //   Rd: bits 19...16
    //   Rm: bits 3...0
    //   Rs: bits 11...8
    //   Rn: bits 15...12

    if (ops[0].type == OP_REG32)
        opcode |= ops[0].reg << 16;
    else
        expect("(destination operand) register");
    if (ops[1].type == OP_REG32)
        opcode |= ops[1].reg;
    else
        expect("(first source operand) register");
    if (ops[2].type == OP_REG32)
        opcode |= ops[2].reg << 8;
    else
        expect("(second source operand) register");
    if (nb_ops > 3) {
        if (ops[3].type == OP_REG32)
            opcode |= ops[3].reg << 12;
        else
            expect("(third source operand) register");
    }

    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_mulseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_muleq:
        if (nb_ops != 3)
            expect("three operands");
        else {
            asm_emit_opcode(token, opcode);
        }
        break;
    case TOK_ASM_mlaseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_mlaeq:
        if (nb_ops != 4)
            expect("four operands");
        else {
            opcode |= 1 << 21; // Accumulate
            asm_emit_opcode(token, opcode);
        }
        break;
    default:
        expect("known multiplication instruction");
    }
}

static void asm_long_multiplication_opcode(TCCState *s1, int token)
{
    Operand ops[4];
    int nb_ops = 0;
    uint32_t opcode = 0x90 | (1 << 23);

    for (nb_ops = 0; nb_ops < sizeof(ops)/sizeof(ops[0]); ++nb_ops) {
        parse_operand(s1, &ops[nb_ops]);
        if (tok != ',') {
            ++nb_ops;
            break;
        }
        next(); // skip ','
    }
    if (nb_ops != 4) {
        expect("four operands");
        return;
    }

    // long multiply (special case):
    // operands:
    //   RdLo: bits 15...12
    //   RdHi: bits 19...16
    //   Rs: bits 11...8
    //   Rm: bits 3...0

    if (ops[0].type == OP_REG32)
        opcode |= ops[0].reg << 12;
    else
        expect("(destination lo accumulator) register");
    if (ops[1].type == OP_REG32)
        opcode |= ops[1].reg << 16;
    else
        expect("(destination hi accumulator) register");
    if (ops[2].type == OP_REG32)
        opcode |= ops[2].reg;
    else
        expect("(first source operand) register");
    if (ops[3].type == OP_REG32)
        opcode |= ops[3].reg << 8;
    else
        expect("(second source operand) register");

    switch (ARM_INSTRUCTION_GROUP(token)) {
    case TOK_ASM_smullseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_smulleq:
        opcode |= 1 << 22; // signed
        asm_emit_opcode(token, opcode);
        break;
    case TOK_ASM_umullseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_umulleq:
        asm_emit_opcode(token, opcode);
        break;
    case TOK_ASM_smlalseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_smlaleq:
        opcode |= 1 << 22; // signed
        opcode |= 1 << 21; // Accumulate
        asm_emit_opcode(token, opcode);
        break;
    case TOK_ASM_umlalseq:
        opcode |= 1 << 20; // Status
        /* fallthrough */
    case TOK_ASM_umlaleq:
        opcode |= 1 << 21; // Accumulate
        asm_emit_opcode(token, opcode);
        break;
    default:
        expect("known long multiplication instruction");
    }
}

static void asm_single_data_transfer_opcode(TCCState *s1, int token)
{
    