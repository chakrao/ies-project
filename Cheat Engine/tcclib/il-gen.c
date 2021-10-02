/*
 *  CIL code generator for TCC
 * 
 *  Copyright (c) 2002 Fabrice Bellard
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

#error this code has bit-rotted since 2003

/* number of available registers */
#define NB_REGS             3

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
#define RC_ST      0x0001  /* any stack entry */
#define RC_ST0     0x0002  /* top of stack */
#define RC_ST1     0x0004  /* top - 1 */

#define RC_INT     RC_ST
#define RC_FLOAT   RC_ST
#define RC_IRET    RC_ST0 /* function return: integer register */
#define RC_LRET    RC_ST0 /* function return: second integer register */
#define RC_FRET    RC_ST0 /* function return: float register */

/* pretty names for the registers */
enum {
    REG_ST0 = 0,
    REG_ST1,
    REG_ST2,
};

const int reg_classes[NB_REGS] = {
    /* ST0 */ RC_ST | RC_ST0,
    /* ST1 */ RC_ST | RC_ST1,
    /* ST2 */ RC_ST,
};

/* return registers for function */
#define REG_IRET REG_ST0 /* single word int return register */
#define REG_LRET REG_ST0 /* second word return register (for long long) */
#define REG_FRET REG_ST0 /* float return register */

/* defined if function parameters must be evaluated in reverse order */
/* #define INVERT_FUNC_PARAMS */

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #define FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#define PTR_SIZE 4

/* long double size and alignment, in bytes */
#define LDOUBLE_SIZE  8
#define LDOUBLE_ALIGN 8

/* function call context */
typedef struct GFuncContext {
    int func_call; /* func call type (FUNC_STDCALL or FUNC_CDECL) */
} GFuncContext;

/******************************************************/
/* opcode definitions */

#define IL_OP_PREFIX 0xFE

enum ILOPCodes {
#define OP(name, str, n) IL_OP_ ## name = n,
#include "il-opcodes.h"
#undef OP
};

char *il_opcodes_str[] = {
#define OP(name, str, n) [n] = str,
#include "il-opcodes.h"
#undef OP
};

/******************************************************/

/* arguments variable numbers start from there */
#define ARG_BASE 0x70000000

static FILE *il_outfile;

static void out_byte(int c)
{
    *(char *)ind++ = c;
}

static void out_le32(int c)
{
    out_byte(c);
    out_byte(c >> 8);
    out_byte(c >> 16);
    out_byte(c >> 24);
}

static void init_outfile(void)
{
    if (!il_outfile) {
        il_outfile = stdout;
        fprintf(il_outfile, 
                ".assembly extern mscorlib\n"
                "{\n"
                ".ver 1:0:2411:0\n"
                "}\n\n");
    }
}

static void out_op1(int op)
{
    if (op & 0x100)
        out_byte(IL_OP_PREFIX);
    out_byte(op & 0xff);
}

/* output an opcode with prefix */
static void out_op(int op)
{
    out_op1(op);
    fprintf(il_outfile, " %s\n", il_opcodes_str[op]);
}

static void out_opb(int op, int c)
{
    out_op1(op);
    out_byte(c);
    fprintf(il_outfile, " %s %d\n", il_opcodes_str[op], c);
}

static void out_opi(int op, int c)
{
    out_op1(op);
    out_le32(c);
    fprintf(il_outfile, " %s 0x%x\n", il_opcodes_str[op], c);
}

/* XXX: not complete */
static void il_type_to_str(char *buf, int buf_size, 
                           int t, const char *varstr)
{
    int bt;
    Sym *s, *sa;
    char buf1[256];
    const char *tstr;

    t = t & VT_TYPE;
    bt = t & VT_BTYPE;
    buf[0] = '\0';
    if (t & VT_UNSIGNED)
        pstrcat(buf, buf_size, "unsigned ");
    switch(bt) {
    case VT_VOID:
        tstr = "void";
        goto add_tstr;
    case VT_BOOL:
        tstr = "bool";
        goto add_tstr;
    case VT_BYTE:
        tstr = "int8";
        goto add_tstr;
    case VT_SHORT:
        tstr = "int16";
        goto add_tstr;
    case VT_ENUM:
    case VT_INT:
    case VT_LONG:
        tstr = "int32";
        goto add_tstr;
    case VT_LLONG:
        tstr = "int64";
        goto add_tstr;
    case VT_FLOAT:
        tstr = "float32";
        goto add_tstr;
    case VT_DOUBLE:
    case VT_LDOUBLE:
        tstr = "float64";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        break;
    case VT_STRUCT:
        tcc_error("structures not handled yet");
        break;
    case VT_FUNC:
        s = sym_find((unsigned)t >> VT_STRUCT_SHIFT);
        il_type_to_str(buf, buf_size, s->t, varstr);
        pstrcat(buf, buf_size, "(");
        sa = s->next;
        while (sa != NULL) {
            il_type_to_str(buf1, sizeof(buf1), sa->t, NULL);
            pstrcat(buf, buf_size, buf1);
            sa = sa->next;
            if (sa)
                pstrcat(buf, buf_size, ", ");
        }
        pstrcat(buf, buf_size, ")");
        goto no_var;
    case VT_PTR:
        s = sym_find((unsigned)t >> VT_STRUCT_SHIFT);
        pstrcpy(buf1, sizeof(buf1), "*");
        if (varstr)
            pstrcat(buf1, sizeof(buf1), varstr);
        il_type_to_str(buf, buf_size, s->t, buf1);
        goto no_var;
    }
    if (varstr) {
        pstrcat(buf, buf_size, " ");
  