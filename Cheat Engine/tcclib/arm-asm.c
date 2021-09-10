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
        tcc_warning("Using 'sp' as operand with '%s' is deprecated by ARM", get_tok_str(tok