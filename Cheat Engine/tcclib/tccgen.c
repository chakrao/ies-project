
/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
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

#define USING_GLOBALS
#include "tcc.h"

/********************************************************/
/* global variables */

/* loc : local variable index
   ind : output code index
   rsym: return symbol
   anon_sym: anonymous symbol index
*/
ST_DATA int rsym, anon_sym, ind, loc;

ST_DATA Sym *global_stack;
ST_DATA Sym *local_stack;
ST_DATA Sym *define_stack;
ST_DATA Sym *global_label_stack;
ST_DATA Sym *local_label_stack;

static Sym *sym_free_first;
static void **sym_pools;
static int nb_sym_pools;

static Sym *all_cleanups, *pending_gotos;
static int local_scope;
static int in_sizeof;
static int in_generic;
static int section_sym;

ST_DATA SValue *vtop;
static SValue _vstack[1 + VSTACK_SIZE];
#define vstack (_vstack + 1)

ST_DATA int const_wanted; /* true if constant wanted */
ST_DATA int nocode_wanted; /* no code generation wanted */
#define unevalmask 0xffff /* unevaluated subexpression */
#define NODATA_WANTED (nocode_wanted > 0) /* no static data output wanted either */
#define STATIC_DATA_WANTED (nocode_wanted & 0xC0000000) /* only static data output */

/* Automagical code suppression ----> */
#define CODE_OFF() (nocode_wanted |= 0x20000000)
#define CODE_ON() (nocode_wanted &= ~0x20000000)

/* Clear 'nocode_wanted' at label if it was used */
ST_FUNC void gsym(int t) { if (t) { gsym_addr(t, ind); CODE_ON(); }}
static int gind(void) { CODE_ON(); return ind; }

/* Set 'nocode_wanted' after unconditional jumps */
static void gjmp_addr_acs(int t) { gjmp_addr(t); CODE_OFF(); }
static int gjmp_acs(int t) { t = gjmp(t); CODE_OFF(); return t; }

/* These are #undef'd at the end of this file */
#define gjmp_addr gjmp_addr_acs
#define gjmp gjmp_acs
/* <---- */

ST_DATA int global_expr;  /* true if compound literals must be allocated globally (used during initializers parsing */
ST_DATA CType func_vt; /* current function return type (used by return instruction) */
ST_DATA int func_var; /* true if current function is variadic (used by return instruction) */
ST_DATA int func_vc;
static int last_line_num, new_file, func_ind; /* debug info control */
ST_DATA const char *funcname;
ST_DATA CType int_type, func_old_type, char_type, char_pointer_type;
static CString initstr;

#if PTR_SIZE == 4
#define VT_SIZE_T (VT_INT | VT_UNSIGNED)
#define VT_PTRDIFF_T VT_INT
#elif LONG_SIZE == 4
#define VT_SIZE_T (VT_LLONG | VT_UNSIGNED)
#define VT_PTRDIFF_T VT_LLONG
#else
#define VT_SIZE_T (VT_LONG | VT_LLONG | VT_UNSIGNED)
#define VT_PTRDIFF_T (VT_LONG | VT_LLONG)
#endif

ST_DATA struct switch_t {
    struct case_t {
        int64_t v1, v2;
	int sym;
    } **p; int n; /* list of case ranges */
    int def_sym; /* default symbol */
    int *bsym;
    struct scope *scope;
    struct switch_t *prev;
    SValue sv;
} *cur_switch; /* current switch */

#define MAX_TEMP_LOCAL_VARIABLE_NUMBER 8
/*list of temporary local variables on the stack in current function. */
ST_DATA struct temp_local_variable {
	int location; //offset on stack. Svalue.c.i
	short size;
	short align;
} arr_temp_local_vars[MAX_TEMP_LOCAL_VARIABLE_NUMBER];
short nb_temp_local_vars;

static struct scope {
    struct scope *prev;
    struct { int loc, num; } vla;
    struct { Sym *s; int n; } cl;
    int *bsym, *csym;
    Sym *lstk, *llstk;
} *cur_scope, *loop_scope, *root_scope;

typedef struct {
    Section *sec;
    int local_offset;
    Sym *flex_array_ref;
} init_params;

/********************************************************/
/* stab debug support */

static const struct {
  int type;
  const char *name;
} default_debug[] = {
    {   VT_INT, "int:t1=r1;-2147483648;2147483647;" },
    {   VT_BYTE, "char:t2=r2;0;127;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT, "long int:t3=r3;-2147483648;2147483647;" },
#else
    {   VT_LLONG | VT_LONG, "long int:t3=r3;-9223372036854775808;9223372036854775807;" },
#endif
    {   VT_INT | VT_UNSIGNED, "unsigned int:t4=r4;0;037777777777;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT | VT_UNSIGNED, "long unsigned int:t5=r5;0;037777777777;" },
#else
    /* use octal instead of -1 so size_t works (-gstabs+ in gcc) */
    {   VT_LLONG | VT_LONG | VT_UNSIGNED, "long unsigned int:t5=r5;0;01777777777777777777777;" },
#endif
    {   VT_QLONG, "__int128:t6=r6;0;-1;" },
    {   VT_QLONG | VT_UNSIGNED, "__int128 unsigned:t7=r7;0;-1;" },
    {   VT_LLONG, "long long int:t8=r8;-9223372036854775808;9223372036854775807;" },
    {   VT_LLONG | VT_UNSIGNED, "long long unsigned int:t9=r9;0;01777777777777777777777;" },
    {   VT_SHORT, "short int:t10=r10;-32768;32767;" },
    {   VT_SHORT | VT_UNSIGNED, "short unsigned int:t11=r11;0;65535;" },
    {   VT_BYTE | VT_DEFSIGN, "signed char:t12=r12;-128;127;" },
    {   VT_BYTE | VT_DEFSIGN | VT_UNSIGNED, "unsigned char:t13=r13;0;255;" },
    {   VT_FLOAT, "float:t14=r1;4;0;" },
    {   VT_DOUBLE, "double:t15=r1;8;0;" },
#ifdef TCC_USING_DOUBLE_FOR_LDOUBLE
    {   VT_DOUBLE | VT_LONG, "long double:t16=r1;8;0;" },
#else
    {   VT_LDOUBLE, "long double:t16=r1;16;0;" },
#endif
    {   -1, "_Float32:t17=r1;4;0;" },
    {   -1, "_Float64:t18=r1;8;0;" },
    {   -1, "_Float128:t19=r1;16;0;" },
    {   -1, "_Float32x:t20=r1;8;0;" },
    {   -1, "_Float64x:t21=r1;16;0;" },
    {   -1, "_Decimal32:t22=r1;4;0;" },
    {   -1, "_Decimal64:t23=r1;8;0;" },
    {   -1, "_Decimal128:t24=r1;16;0;" },
    /* if default char is unsigned */
    {   VT_BYTE | VT_UNSIGNED, "unsigned char:t25=r25;0;255;" },
    /* boolean type */
    {   VT_BOOL, "bool:t26=r26;0;255;" },
    {   VT_VOID, "void:t27=27" },
};

static int debug_next_type;

static struct debug_hash {
    int debug_type;
    Sym *type;
} *debug_hash;

static int n_debug_hash;

static struct debug_info {
    int start;
    int end;
    int n_sym;
    struct debug_sym {
        int type;
        unsigned long value;
        char *str;
        Section *sec;
        int sym_index;
    } *sym;
    struct debug_info *child, *next, *last, *parent;
} *debug_info, *debug_info_root;

/********************************************************/
#if 1
#define precedence_parser
static void init_prec(void);
#endif
/********************************************************/
#ifndef CONFIG_TCC_ASM
ST_FUNC void asm_instr(void)
{
    tcc_error("inline asm() not supported");
}
ST_FUNC void asm_global_instr(void)
{
    tcc_error("inline asm() not supported");
}
#endif

/* ------------------------------------------------------------------------- */
static void gen_cast(CType *type);
static void gen_cast_s(int t);
static inline CType *pointed_type(CType *type);
static int is_compatible_types(CType *type1, CType *type2);
static int parse_btype(CType *type, AttributeDef *ad);
static CType *type_decl(CType *type, AttributeDef *ad, int *v, int td);
static void parse_expr_type(CType *type);
static void init_putv(init_params *p, CType *type, unsigned long c);
static void decl_initializer(init_params *p, CType *type, unsigned long c, int flags);
static void block(int is_expr);
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r, int has_init, int v, int scope);
static void decl(int l);
static int decl0(int l, int is_for_loop_init, Sym *);
static void expr_eq(void);
static void vla_runtime_type_size(CType *type, int *a);
static int is_compatible_unqualified_types(CType *type1, CType *type2);
static inline int64_t expr_const64(void);
static void vpush64(int ty, unsigned long long v);
static void vpush(CType *type);
static int gvtst(int inv, int t);
static void gen_inline_functions(TCCState *s);
static void free_inline_functions(TCCState *s);
static void skip_or_save_block(TokenString **str);
static void gv_dup(void);
static int get_temp_local_var(int size,int align);
static void clear_temp_local_var_list();
static void cast_error(CType *st, CType *dt);

ST_INLN int is_float(int t)
{
    int bt = t & VT_BTYPE;
    return bt == VT_LDOUBLE
        || bt == VT_DOUBLE
        || bt == VT_FLOAT
        || bt == VT_QFLOAT;
}

static inline int is_integer_btype(int bt)
{
    return bt == VT_BYTE
        || bt == VT_BOOL
        || bt == VT_SHORT
        || bt == VT_INT
        || bt == VT_LLONG;
}

static int btype_size(int bt)
{
    return bt == VT_BYTE || bt == VT_BOOL ? 1 :
        bt == VT_SHORT ? 2 :
        bt == VT_INT ? 4 :
        bt == VT_LLONG ? 8 :
        bt == VT_PTR ? PTR_SIZE : 0;
}

/* returns function return register from type */
static int R_RET(int t)
{
    if (!is_float(t))
        return REG_IRET;
#ifdef TCC_TARGET_X86_64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return TREG_ST0;
#elif defined TCC_TARGET_RISCV64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return REG_IRET;
#endif
    return REG_FRET;
}

/* returns 2nd function return register, if any */
static int R2_RET(int t)
{
    t &= VT_BTYPE;
#if PTR_SIZE == 4
    if (t == VT_LLONG)
        return REG_IRE2;
#elif defined TCC_TARGET_X86_64
    if (t == VT_QLONG)
        return REG_IRE2;
    if (t == VT_QFLOAT)
        return REG_FRE2;
#elif defined TCC_TARGET_RISCV64
    if (t == VT_LDOUBLE)
        return REG_IRE2;
#endif
    return VT_CONST;
}

/* returns true for two-word types */
#define USING_TWO_WORDS(t) (R2_RET(t) != VT_CONST)

/* put function return registers to stack value */
static void PUT_R_RET(SValue *sv, int t)
{
    sv->r = R_RET(t), sv->r2 = R2_RET(t);
}

/* returns function return register class for type t */
static int RC_RET(int t)
{
    return reg_classes[R_RET(t)] & ~(RC_FLOAT | RC_INT);
}

/* returns generic register class for type t */
static int RC_TYPE(int t)
{
    if (!is_float(t))
        return RC_INT;
#ifdef TCC_TARGET_X86_64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return RC_ST0;
    if ((t & VT_BTYPE) == VT_QFLOAT)
        return RC_FRET;
#elif defined TCC_TARGET_RISCV64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return RC_INT;
#endif
    return RC_FLOAT;
}

/* returns 2nd register class corresponding to t and rc */
static int RC2_TYPE(int t, int rc)
{
    if (!USING_TWO_WORDS(t))
        return 0;
#ifdef RC_IRE2
    if (rc == RC_IRET)
        return RC_IRE2;
#endif
#ifdef RC_FRE2
    if (rc == RC_FRET)
        return RC_FRE2;
#endif
    if (rc & RC_FLOAT)
        return RC_FLOAT;
    return RC_INT;
}

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
ST_FUNC int ieee_finite(double d)
{
    int p[4];
    memcpy(p, &d, sizeof(double));
    return ((unsigned)((p[1] | 0x800fffff) + 1)) >> 31;
}

/* compiling intel long double natively */
#if (defined __i386__ || defined __x86_64__) \
    && (defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64)
# define TCC_IS_NATIVE_387
#endif

ST_FUNC void test_lvalue(void)
{
    if (!(vtop->r & VT_LVAL))
        expect("lvalue");
}

ST_FUNC void check_vstack(void)
{
    if (vtop != vstack - 1)
        tcc_error("internal compiler error: vstack leak (%d)",
                  (int)(vtop - vstack + 1));
}

/* ------------------------------------------------------------------------- */
/* vstack debugging aid */

#if 0
void pv (const char *lbl, int a, int b)
{
    int i;
    for (i = a; i < a + b; ++i) {
        SValue *p = &vtop[-i];
        printf("%s vtop[-%d] : type.t:%04x  r:%04x  r2:%04x  c.i:%d\n",
            lbl, i, p->type.t, p->r, p->r2, (int)p->c.i);
    }
}
#endif

/* ------------------------------------------------------------------------- */
/* start of translation unit info */
ST_FUNC void tcc_debug_start(TCCState *s1)
{
    if (s1->do_debug) {
        int i;
        char buf[512];

        /* file info: full path + filename */
        section_sym = put_elf_sym(symtab_section, 0, 0,
                                  ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                                  text_section->sh_num, NULL);
        getcwd(buf, sizeof(buf));
#ifdef _WIN32
        normalize_slashes(buf);
#endif
        pstrcat(buf, sizeof(buf), "/");
        put_stabs_r(s1, buf, N_SO, 0, 0,
                    text_section->data_offset, text_section, section_sym);
        put_stabs_r(s1, file->prev ? file->prev->filename : file->filename,
                    N_SO, 0, 0,
                    text_section->data_offset, text_section, section_sym);
        for (i = 0; i < sizeof (default_debug) / sizeof (default_debug[0]); i++)
            put_stabs(s1, default_debug[i].name, N_LSYM, 0, 0, 0);

        new_file = last_line_num = 0;
        func_ind = -1;
        debug_next_type = sizeof(default_debug) / sizeof(default_debug[0]);
        debug_hash = NULL;
        n_debug_hash = 0;

        /* we're currently 'including' the <command line> */
        tcc_debug_bincl(s1);
    }

    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0,
                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0,
                SHN_ABS, file->filename);
}

static void tcc_debug_stabs (TCCState *s1, const char *str, int type, unsigned long value,
                             Section *sec, int sym_index)
{
    struct debug_sym *s;

    if (debug_info) {
        debug_info->sym =
            (struct debug_sym *)tcc_realloc (debug_info->sym,
                                             sizeof(struct debug_sym) *
                                             (debug_info->n_sym + 1));
        s = debug_info->sym + debug_info->n_sym++;
        s->type = type;
        s->value = value;
        s->str = tcc_strdup(str);
        s->sec = sec;
        s->sym_index = sym_index;
    }
    else if (sec)
        put_stabs_r (s1, str, type, 0, 0, value, sec, sym_index);
    else
        put_stabs (s1, str, type, 0, 0, value);
}

static void tcc_debug_stabn(int type, int value)
{
    if (type == N_LBRAC) {
        struct debug_info *info =
            (struct debug_info *) tcc_mallocz(sizeof (*info));

        info->start = value;
        info->parent = debug_info;
        if (debug_info) {
            if (debug_info->child) {
                if (debug_info->child->last)
                    debug_info->child->last->next = info;
                else
                    debug_info->child->next = info;
                debug_info->child->last = info;
            }
            else
                debug_info->child = info;
        }
        else
            debug_info_root = info;
        debug_info = info;
    }
    else {
        debug_info->end = value;
        debug_info = debug_info->parent;
    }
}

static void tcc_get_debug_info(TCCState *s1, Sym *s, CString *result)
{
    int type;
    int n = 0;
    int debug_type = -1;
    Sym *t = s;
    CString str;

    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR || type == (VT_PTR | VT_ARRAY))
            n++, t = t->type.ref;
        else
            break;
    }
    if ((type & VT_BTYPE) == VT_STRUCT) {
        int i;

        t = t->type.ref;
        for (i = 0; i < n_debug_hash; i++) {
            if (t == debug_hash[i].type) {
                debug_type = debug_hash[i].debug_type;
                break;
            }
        }
        if (debug_type == -1) {
            debug_type = ++debug_next_type;
            debug_hash = (struct debug_hash *)
                tcc_realloc (debug_hash,
                             (n_debug_hash + 1) * sizeof(*debug_hash));
            debug_hash[n_debug_hash].debug_type = debug_type;
            debug_hash[n_debug_hash++].type = t;
            cstr_new (&str);
            cstr_printf (&str, "%s:T%d=%c%d",
                         (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(t->v & ~SYM_STRUCT, NULL),
                         debug_type,
                         IS_UNION (t->type.t) ? 'u' : 's',
                         t->c);
            while (t->next) {
                int pos, size, align;

                t = t->next;
                cstr_printf (&str, "%s:",
                             (t->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                             ? "" : get_tok_str(t->v & ~SYM_FIELD, NULL));
                tcc_get_debug_info (s1, t, &str);
                if (t->type.t & VT_BITFIELD) {
                    pos = t->c * 8 + BIT_POS(t->type.t);
                    size = BIT_SIZE(t->type.t);
                }
                else {
                    pos = t->c * 8;
                    size = type_size(&t->type, &align) * 8;
                }
                cstr_printf (&str, ",%d,%d;", pos, size);
            }
            cstr_printf (&str, ";");
            tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
            cstr_free (&str);
        }
    }
    else if (IS_ENUM(type)) {
        Sym *e = t = t->type.ref;

        debug_type = ++debug_next_type;
        cstr_new (&str);
        cstr_printf (&str, "%s:T%d=e",
                     (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                     ? "" : get_tok_str(t->v & ~SYM_STRUCT, NULL),
                     debug_type);
        while (t->next) {
            t = t->next;
            cstr_printf (&str, "%s:",
                         (t->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(t->v & ~SYM_FIELD, NULL));
            cstr_printf (&str, e->type.t & VT_UNSIGNED ? "%u," : "%d,",
                         (int)t->enum_val);
        }
        cstr_printf (&str, ";");
        tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
        cstr_free (&str);
    }
    else if ((type & VT_BTYPE) != VT_FUNC) {
        type &= ~VT_STRUCT_MASK;
        for (debug_type = 1;
             debug_type <= sizeof(default_debug) / sizeof(default_debug[0]);
             debug_type++)
            if (default_debug[debug_type - 1].type == type)
                break;
        if (debug_type > sizeof(default_debug) / sizeof(default_debug[0]))
            return;
    }
    if (n > 0)
        cstr_printf (result, "%d=", ++debug_next_type);
    t = s;
    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR)
            cstr_printf (result, "%d=*", ++debug_next_type);
        else if (type == (VT_PTR | VT_ARRAY))
            cstr_printf (result, "%d=ar1;0;%d;",
                         ++debug_next_type, t->type.ref->c - 1);
        else if (type == VT_FUNC) {
            cstr_printf (result, "%d=f", ++debug_next_type);
            tcc_get_debug_info (s1, t->type.ref, result);
            return;
        }
        else
            break;
        t = t->type.ref;
    }
    cstr_printf (result, "%d", debug_type);
}

static void tcc_debug_finish (TCCState *s1, struct debug_info *cur)
{
    while (cur) {
        int i;
        struct debug_info *next = cur->next;

        for (i = 0; i < cur->n_sym; i++) {
            struct debug_sym *s = &cur->sym[i];

            if (s->sec)
                put_stabs_r(s1, s->str, s->type, 0, 0, s->value,
                            s->sec, s->sym_index);
            else
                put_stabs(s1, s->str, s->type, 0, 0, s->value);
            tcc_free (s->str);
        }
        tcc_free (cur->sym);
        put_stabn(s1, N_LBRAC, 0, 0, cur->start);
        tcc_debug_finish (s1, cur->child);
        put_stabn(s1, N_RBRAC, 0, 0, cur->end);
        tcc_free (cur);
        cur = next;
    }
}

static void tcc_add_debug_info(TCCState *s1, int param, Sym *s, Sym *e)
{
    CString debug_str;
    cstr_new (&debug_str);
    for (; s != e; s = s->prev) {
        if (!s->v || (s->r & VT_VALMASK) != VT_LOCAL)
            continue;
        cstr_reset (&debug_str);
        cstr_printf (&debug_str, "%s:%s", get_tok_str(s->v, NULL), param ? "p" : "");
        tcc_get_debug_info(s1, s, &debug_str);
        tcc_debug_stabs(s1, debug_str.data, param ? N_PSYM : N_LSYM, s->c, NULL, 0);
    }
    cstr_free (&debug_str);
}

static void tcc_debug_extern_sym(TCCState *s1, Sym *sym, int sh_num, int sym_bind)
{
    Section *s = s1->sections[sh_num];
    CString str;

    cstr_new (&str);
    cstr_printf (&str, "%s:%c",
        get_tok_str(sym->v, NULL),
        sym_bind == STB_GLOBAL ? 'G' : local_scope ? 'V' : 'S'
        );
    tcc_get_debug_info(s1, sym, &str);
    if (sym_bind == STB_GLOBAL)
        tcc_debug_stabs(s1, str.data, N_GSYM, 0, NULL, 0);
    else
        tcc_debug_stabs(s1, str.data,
            (sym->type.t & VT_STATIC) && data_section == s
            ? N_STSYM : N_LCSYM, 0, s, sym->c);
    cstr_free (&str);
}

static void tcc_debug_typedef(TCCState *s1, Sym *sym)
{
    CString str;

    cstr_new (&str);
    cstr_printf (&str, "%s:t",
                 (sym->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                 ? "" : get_tok_str(sym->v & ~SYM_FIELD, NULL));
    tcc_get_debug_info(s1, sym, &str);
    tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
    cstr_free (&str);
}

/* put end of translation unit info */
ST_FUNC void tcc_debug_end(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabs_r(s1, NULL, N_SO, 0, 0,
        text_section->data_offset, text_section, section_sym);
    tcc_free(debug_hash);
}

static BufferedFile* put_new_file(TCCState *s1)
{
    BufferedFile *f = file;
    /* use upper file if from inline ":asm:" */
    if (f->filename[0] == ':')
        f = f->prev;
    if (f && new_file) {
        put_stabs_r(s1, f->filename, N_SOL, 0, 0, ind, text_section, section_sym);
        new_file = last_line_num = 0;
    }
    return f;
}

/* generate line number info */
ST_FUNC void tcc_debug_line(TCCState *s1)
{
    BufferedFile *f;
    if (!s1->do_debug
        || cur_text_section != text_section
        || !(f = put_new_file(s1))
        || last_line_num == f->line_num)
        return;
    if (func_ind != -1) {
        put_stabn(s1, N_SLINE, 0, f->line_num, ind - func_ind);
    } else {
        /* from tcc_assemble */
        put_stabs_r(s1, NULL, N_SLINE, 0, f->line_num, ind, text_section, section_sym);
    }
    last_line_num = f->line_num;
}

/* put function symbol */
ST_FUNC void tcc_debug_funcstart(TCCState *s1, Sym *sym)
{
    CString debug_str;
    BufferedFile *f;
    if (!s1->do_debug)
        return;
    debug_info_root = NULL;
    debug_info = NULL;
    tcc_debug_stabn(N_LBRAC, ind - func_ind);
    if (!(f = put_new_file(s1)))
        return;
    cstr_new (&debug_str);
    cstr_printf(&debug_str, "%s:%c", funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
    tcc_get_debug_info(s1, sym->type.ref, &debug_str);
    put_stabs_r(s1, debug_str.data, N_FUN, 0, f->line_num, 0, cur_text_section, sym->c);
    cstr_free (&debug_str);

    tcc_debug_line(s1);
}

/* put function size */
ST_FUNC void tcc_debug_funcend(TCCState *s1, int size)
{
    if (!s1->do_debug)
        return;
    tcc_debug_stabn(N_RBRAC, size);
    tcc_debug_finish (s1, debug_info_root);
}

/* put alternative filename */
ST_FUNC void tcc_debug_putfile(TCCState *s1, const char *filename)
{
    if (0 == strcmp(file->filename, filename))
        return;
    pstrcpy(file->filename, sizeof(file->filename), filename);
    new_file = 1;
}

/* begin of #include */
ST_FUNC void tcc_debug_bincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabs(s1, file->filename, N_BINCL, 0, 0, 0);
    new_file = 1;
}

/* end of #include */
ST_FUNC void tcc_debug_eincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabn(s1, N_EINCL, 0, 0, 0);
    new_file = 1;
}

/* ------------------------------------------------------------------------- */
/* initialize vstack and types.  This must be done also for tcc -E */
ST_FUNC void tccgen_init(TCCState *s1)
{
    vtop = vstack - 1;
    memset(vtop, 0, sizeof *vtop);

    /* define some often used types */
    int_type.t = VT_INT;

    char_type.t = VT_BYTE;
    if (s1->char_is_unsigned)
        char_type.t |= VT_UNSIGNED;
    char_pointer_type = char_type;
    mk_pointer(&char_pointer_type);

    func_old_type.t = VT_FUNC;
    func_old_type.ref = sym_push(SYM_FIELD, &int_type, 0, 0);
    func_old_type.ref->f.func_call = FUNC_CDECL;
    func_old_type.ref->f.func_type = FUNC_OLD;
#ifdef precedence_parser
    init_prec();
#endif
    cstr_new(&initstr);
}

ST_FUNC int tccgen_compile(TCCState *s1)
{
    cur_text_section = NULL;
    funcname = "";
    anon_sym = SYM_FIRST_ANOM;
    section_sym = 0;
    const_wanted = 0;
    nocode_wanted = 0x80000000;
    local_scope = 0;

    tcc_debug_start(s1);
#ifdef TCC_TARGET_ARM
    arm_init(s1);
#endif
#ifdef INC_DEBUG
    printf("%s: **** new file\n", file->filename);
#endif
    parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | PARSE_FLAG_TOK_STR;
    next();
    decl(VT_CONST);
    gen_inline_functions(s1);
    check_vstack();
    /* end of translation unit info */
    tcc_debug_end(s1);
    return 0;
}

ST_FUNC void tccgen_finish(TCCState *s1)
{
    cstr_free(&initstr);
    free_inline_functions(s1);
    sym_pop(&global_stack, NULL, 0);
    sym_pop(&local_stack, NULL, 0);
    /* free preprocessor macros */
    free_defines(NULL);
    /* free sym_pools */
    dynarray_reset(&sym_pools, &nb_sym_pools);
    sym_free_first = NULL;
}

/* ------------------------------------------------------------------------- */
ST_FUNC ElfSym *elfsym(Sym *s)
{
  if (!s || !s->c)
    return NULL;
  return &((ElfSym *)symtab_section->data)[s->c];
}

/* apply storage attributes to Elf symbol */
ST_FUNC void update_storage(Sym *sym)
{
    ElfSym *esym;
    int sym_bind, old_sym_bind;

    esym = elfsym(sym);
    if (!esym)
        return;

    if (sym->a.visibility)
        esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
            | sym->a.visibility;

    if (sym->type.t & (VT_STATIC | VT_INLINE))
        sym_bind = STB_LOCAL;
    else if (sym->a.weak)
        sym_bind = STB_WEAK;
    else
        sym_bind = STB_GLOBAL;
    old_sym_bind = ELFW(ST_BIND)(esym->st_info);
    if (sym_bind != old_sym_bind) {
        esym->st_info = ELFW(ST_INFO)(sym_bind, ELFW(ST_TYPE)(esym->st_info));
    }

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport)
        esym->st_other |= ST_PE_IMPORT;
    if (sym->a.dllexport)
        esym->st_other |= ST_PE_EXPORT;
#endif

#if 0
    printf("storage %s: bind=%c vis=%d exp=%d imp=%d\n",
        get_tok_str(sym->v, NULL),
        sym_bind == STB_WEAK ? 'w' : sym_bind == STB_LOCAL ? 'l' : 'g',
        sym->a.visibility,
        sym->a.dllexport,
        sym->a.dllimport
        );
#endif
}

/* ------------------------------------------------------------------------- */
/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */

ST_FUNC void put_extern_sym2(Sym *sym, int sh_num,
                            addr_t value, unsigned long size,
                            int can_add_underscore)
{
    int sym_type, sym_bind, info, other, t;
    ElfSym *esym;
    const char *name;
    char buf1[256];

    if (!sym->c) {
        name = get_tok_str(sym->v, NULL);
        t = sym->type.t;
        if ((t & VT_BTYPE) == VT_FUNC) {
            sym_type = STT_FUNC;
        } else if ((t & VT_BTYPE) == VT_VOID) {
            sym_type = STT_NOTYPE;
            if ((t & (VT_BTYPE|VT_ASM_FUNC)) == VT_ASM_FUNC)
                sym_type = STT_FUNC;
        } else {
            sym_type = STT_OBJECT;
        }
        if (t & (VT_STATIC | VT_INLINE))
            sym_bind = STB_LOCAL;
        else
            sym_bind = STB_GLOBAL;
        other = 0;

#ifdef TCC_TARGET_PE
        if (sym_type == STT_FUNC && sym->type.ref) {
            Sym *ref = sym->type.ref;
            if (ref->a.nodecorate) {
                can_add_underscore = 0;
            }
            if (ref->f.func_call == FUNC_STDCALL && can_add_underscore) {
                sprintf(buf1, "_%s@%d", name, ref->f.func_args * PTR_SIZE);
                name = buf1;
                other |= ST_PE_STDCALL;
                can_add_underscore = 0;
            }
        }
#endif

        if (sym->asm_label) {
            name = get_tok_str(sym->asm_label, NULL);
            can_add_underscore = 0;
        }

        if (tcc_state->leading_underscore && can_add_underscore) {
            buf1[0] = '_';
            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
            name = buf1;
        }

        info = ELFW(ST_INFO)(sym_bind, sym_type);
        sym->c = put_elf_sym(symtab_section, value, size, info, other, sh_num, name);

        if (tcc_state->do_debug
            && sym_type != STT_FUNC
            && sym->v < SYM_FIRST_ANOM)
            tcc_debug_extern_sym(tcc_state, sym, sh_num, sym_bind);

    } else {
        esym = elfsym(sym);
        esym->st_value = value;
        esym->st_size = size;
        esym->st_shndx = sh_num;
    }
    update_storage(sym);
}

ST_FUNC void put_extern_sym(Sym *sym, Section *section,
                           addr_t value, unsigned long size)
{
    int sh_num = section ? section->sh_num : SHN_UNDEF;
    put_extern_sym2(sym, sh_num, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
ST_FUNC void greloca(Section *s, Sym *sym, unsigned long offset, int type,
                     addr_t addend)
{
    int c = 0;

    if (nocode_wanted && s == cur_text_section)
        return;

    if (sym) {
        if (0 == sym->c)
            put_extern_sym(sym, NULL, 0, 0);
        c = sym->c;
    }

    /* now we can add ELF relocation info */
    put_elf_reloca(symtab_section, s, offset, type, c, addend);
}

#if PTR_SIZE == 4
ST_FUNC void greloc(Section *s, Sym *sym, unsigned long offset, int type)
{
    greloca(s, sym, offset, type, 0);
}
#endif

/* ------------------------------------------------------------------------- */
/* symbol allocator */
static Sym *__sym_malloc(void)
{
    Sym *sym_pool, *sym, *last_sym;
    int i;

    sym_pool = tcc_malloc(SYM_POOL_NB * sizeof(Sym));
    dynarray_add(&sym_pools, &nb_sym_pools, sym_pool);

    last_sym = sym_free_first;
    sym = sym_pool;
    for(i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(void)
{
    Sym *sym;
#ifndef SYM_DEBUG
    sym = sym_free_first;
    if (!sym)
        sym = __sym_malloc();
    sym_free_first = sym->next;
    return sym;
#else
    sym = tcc_malloc(sizeof(Sym));
    return sym;
#endif
}

ST_INLN void sym_free(Sym *sym)
{
#ifndef SYM_DEBUG
    sym->next = sym_free_first;
    sym_free_first = sym;
#else
    tcc_free(sym);
#endif
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(Sym **ps, int v, int t, int c)
{
    Sym *s;

    s = sym_malloc();
    memset(s, 0, sizeof *s);
    s->v = v;
    s->type.t = t;
    s->c = c;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
ST_FUNC Sym *sym_find2(Sym *s, int v)
{
    while (s) {
        if (s->v == v)
            return s;
        else if (s->v == -1)
            return NULL;
        s = s->prev;
    }
    return NULL;
}

/* structure lookup */
ST_INLN Sym *struct_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_struct;
}

/* find an identifier */
ST_INLN Sym *sym_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_identifier;
}

static int sym_scope(Sym *s)
{
  if (IS_ENUM_VAL (s->type.t))
    return s->type.ref->sym_scope;
  else
    return s->sym_scope;
}

/* push a given symbol on the symbol stack */
ST_FUNC Sym *sym_push(int v, CType *type, int r, int c)
{
    Sym *s, **ps;
    TokenSym *ts;

    if (local_stack)
        ps = &local_stack;
    else
        ps = &global_stack;
    s = sym_push2(ps, v, type->t, c);
    s->type.ref = type->ref;
    s->r = r;
    /* don't record fields or anonymous symbols */
    /* XXX: simplify */
    if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
        /* record symbol in token array */
        ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
        if (v & SYM_STRUCT)
            ps = &ts->sym_struct;
        else
            ps = &ts->sym_identifier;
        s->prev_tok = *ps;
        *ps = s;
        s->sym_scope = local_scope;
        if (s->prev_tok && sym_scope(s->prev_tok) == s->sym_scope)
            tcc_error("redeclaration of '%s'",
                get_tok_str(v & ~SYM_STRUCT, NULL));
    }
    return s;
}

/* push a global identifier */
ST_FUNC Sym *global_identifier_push(int v, int t, int c)
{
    Sym *s, **ps;
    s = sym_push2(&global_stack, v, t, c);
    s->r = VT_CONST | VT_SYM;
    /* don't record anonymous symbol */
    if (v < SYM_FIRST_ANOM) {
        ps = &table_ident[v - TOK_IDENT]->sym_identifier;
        /* modify the top most local identifier, so that sym_identifier will
           point to 's' when popped; happens when called from inline asm */
        while (*ps != NULL && (*ps)->sym_scope)
            ps = &(*ps)->prev_tok;
        s->prev_tok = *ps;
        *ps = s;
    }
    return s;
}

/* pop symbols until top reaches 'b'.  If KEEP is non-zero don't really
   pop them yet from the list, but do remove them from the token array.  */
ST_FUNC void sym_pop(Sym **ptop, Sym *b, int keep)
{
    Sym *s, *ss, **ps;
    TokenSym *ts;
    int v;

    s = *ptop;
    while(s != b) {
        ss = s->prev;
        v = s->v;
        /* remove symbol in token array */
        /* XXX: simplify */
        if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
            ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
            if (v & SYM_STRUCT)
                ps = &ts->sym_struct;
            else
                ps = &ts->sym_identifier;
            *ps = s->prev_tok;
        }
	if (!keep)
	    sym_free(s);
        s = ss;
    }
    if (!keep)
	*ptop = b;
}

/* ------------------------------------------------------------------------- */
static void vcheck_cmp(void)
{
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator.

       Don't do this when nocode_wanted.  vtop might come from
       !nocode_wanted regions (see 88_codeopt.c) and transforming
       it to a register without actually generating code is wrong
       as their value might still be used for real.  All values
       we push under nocode_wanted will eventually be popped
       again, so that the VT_CMP/VT_JMP value will be in vtop
       when code is unsuppressed again. */

    if (vtop->r == VT_CMP && !nocode_wanted)
        gv(RC_INT);
}

static void vsetc(CType *type, int r, CValue *vc)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full (vstack)");
    vcheck_cmp();
    vtop++;
    vtop->type = *type;
    vtop->r = r;
    vtop->r2 = VT_CONST;
    vtop->c = *vc;
    vtop->sym = NULL;
}

ST_FUNC void vswap(void)
{
    SValue tmp;

    vcheck_cmp();
    tmp = vtop[0];
    vtop[0] = vtop[-1];
    vtop[-1] = tmp;
}

/* pop stack value */
ST_FUNC void vpop(void)
{
    int v;
    v = vtop->r & VT_VALMASK;
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
    /* for x86, we need to pop the FP stack */
    if (v == TREG_ST0) {
        o(0xd8dd); /* fstp %st(0) */
    } else
#endif
    if (v == VT_CMP) {
        /* need to put correct jump if && or || without test */
        gsym(vtop->jtrue);
        gsym(vtop->jfalse);
    }
    vtop--;
}

/* push constant of type "type" with useless value */
static void vpush(CType *type)
{
    vset(type, VT_CONST, 0);
}

/* push arbitrary 64bit constant */
static void vpush64(int ty, unsigned long long v)
{
    CValue cval;
    CType ctype;
    ctype.t = ty;
    ctype.ref = NULL;
    cval.i = v;
    vsetc(&ctype, VT_CONST, &cval);
}

/* push integer constant */
ST_FUNC void vpushi(int v)
{
    vpush64(VT_INT, v);
}

/* push a pointer sized constant */
static void vpushs(addr_t v)
{
    vpush64(VT_SIZE_T, v);
}

/* push long long constant */
static inline void vpushll(long long v)
{
    vpush64(VT_LLONG, v);
}

ST_FUNC void vset(CType *type, int r, int v)
{
    CValue cval;
    cval.i = v;
    vsetc(type, r, &cval);
}

static void vseti(int r, int v)
{
    CType type;
    type.t = VT_INT;
    type.ref = NULL;
    vset(&type, r, v);
}

ST_FUNC void vpushv(SValue *v)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        tcc_error("memory full (vstack)");
    vtop++;
    *vtop = *v;
}

static void vdup(void)
{
    vpushv(vtop);
}

/* rotate n first stack elements to the bottom
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC void vrotb(int n)
{
    int i;
    SValue tmp;

    vcheck_cmp();
    tmp = vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        vtop[i] = vtop[i+1];
    vtop[0] = tmp;
}

/* rotate the n elements before entry e towards the top
   I1 ... In ... -> In I1 ... I(n-1) ... [top is right]
 */
ST_FUNC void vrote(SValue *e, int n)
{
    int i;
    SValue tmp;

    vcheck_cmp();
    tmp = *e;
    for(i = 0;i < n - 1; i++)
        e[-i] = e[-i - 1];
    e[-n + 1] = tmp;
}

/* rotate n first stack elements to the top
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
ST_FUNC void vrott(int n)
{
    vrote(vtop, n);
}

/* ------------------------------------------------------------------------- */
/* vtop->r = VT_CMP means CPU-flags have been set from comparison or test. */

/* called from generators to set the result from relational ops  */
ST_FUNC void vset_VT_CMP(int op)
{
    vtop->r = VT_CMP;
    vtop->cmp_op = op;
    vtop->jfalse = 0;
    vtop->jtrue = 0;
}

/* called once before asking generators to load VT_CMP to a register */
static void vset_VT_JMP(void)
{
    int op = vtop->cmp_op;

    if (vtop->jtrue || vtop->jfalse) {
        /* we need to jump to 'mov $0,%R' or 'mov $1,%R' */
        int inv = op & (op < 2); /* small optimization */
        vseti(VT_JMP+inv, gvtst(inv, 0));
    } else {
        /* otherwise convert flags (rsp. 0/1) to register */
        vtop->c.i = op;
        if (op < 2) /* doesn't seem to happen */
            vtop->r = VT_CONST;
    }
}

/* Set CPU Flags, doesn't yet jump */
static void gvtst_set(int inv, int t)
{
    int *p;

    if (vtop->r != VT_CMP) {
        vpushi(0);
        gen_op(TOK_NE);
        if (vtop->r != VT_CMP) /* must be VT_CONST then */
            vset_VT_CMP(vtop->c.i != 0);
    }

    p = inv ? &vtop->jfalse : &vtop->jtrue;
    *p = gjmp_append(*p, t);
}

/* Generate value test
 *
 * Generate a test for any value (jump, comparison and integers) */
static int gvtst(int inv, int t)
{
    int op, x, u;

    gvtst_set(inv, t);
    t = vtop->jtrue, u = vtop->jfalse;
    if (inv)
        x = u, u = t, t = x;
    op = vtop->cmp_op;

    /* jump to the wanted target */
    if (op > 1)
        t = gjmp_cond(op ^ inv, t);
    else if (op != inv)
        t = gjmp(t);
    /* resolve complementary jumps to here */
    gsym(u);

    vtop--;
    return t;
}

/* generate a zero or nozero test */
static void gen_test_zero(int op)
{
    if (vtop->r == VT_CMP) {
        int j;
        if (op == TOK_EQ) {
            j = vtop->jfalse;
            vtop->jfalse = vtop->jtrue;
            vtop->jtrue = j;
            vtop->cmp_op ^= 1;
        }
    } else {
        vpushi(0);
        gen_op(op);
    }
}

/* ------------------------------------------------------------------------- */
/* push a symbol value of TYPE */
ST_FUNC void vpushsym(CType *type, Sym *sym)
{
    CValue cval;
    cval.i = 0;
    vsetc(type, VT_CONST | VT_SYM, &cval);
    vtop->sym = sym;
}

/* Return a static symbol pointing to a section */
ST_FUNC Sym *get_sym_ref(CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    int v;
    Sym *sym;

    v = anon_sym++;
    sym = sym_push(v, type, VT_CONST | VT_SYM, 0);
    sym->type.t |= VT_STATIC;
    put_extern_sym(sym, sec, offset, size);
    return sym;
}

/* push a reference to a section offset by adding a dummy symbol */
static void vpush_ref(CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    vpushsym(type, get_sym_ref(type, sec, offset, size));  
}

/* define a new external reference to a symbol 'v' of type 'u' */
ST_FUNC Sym *external_global_sym(int v, CType *type)
{
    Sym *s;

    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = global_identifier_push(v, type->t | VT_EXTERN, 0);
        s->type.ref = type->ref;
    } else if (IS_ASM_SYM(s)) {
        s->type.t = type->t | (s->type.t & VT_EXTERN);
        s->type.ref = type->ref;
        update_storage(s);
    }
    return s;
}

/* create an external reference with no specific type similar to asm labels.
   This avoids type conflicts if the symbol is used from C too */
ST_FUNC Sym *external_helper_sym(int v)
{
    CType ct = { VT_ASM_FUNC, NULL };
    return external_global_sym(v, &ct);
}

/* push a reference to an helper function (such as memmove) */
ST_FUNC void vpush_helper_func(int v)
{
    vpushsym(&func_old_type, external_helper_sym(v));
}

/* Merge symbol attributes.  */
static void merge_symattr(struct SymAttr *sa, struct SymAttr *sa1)
{
    if (sa1->aligned && !sa->aligned)
      sa->aligned = sa1->aligned;
    sa->packed |= sa1->packed;
    sa->weak |= sa1->weak;
    if (sa1->visibility != STV_DEFAULT) {
	int vis = sa->visibility;
	if (vis == STV_DEFAULT
	    || vis > sa1->visibility)
	  vis = sa1->visibility;
	sa->visibility = vis;
    }
    sa->dllexport |= sa1->dllexport;
    sa->nodecorate |= sa1->nodecorate;
    sa->dllimport |= sa1->dllimport;
}

/* Merge function attributes.  */
static void merge_funcattr(struct FuncAttr *fa, struct FuncAttr *fa1)
{
    if (fa1->func_call && !fa->func_call)
      fa->func_call = fa1->func_call;
    if (fa1->func_type && !fa->func_type)
      fa->func_type = fa1->func_type;
    if (fa1->func_args && !fa->func_args)
      fa->func_args = fa1->func_args;
    if (fa1->func_noreturn)
      fa->func_noreturn = 1;
    if (fa1->func_ctor)
      fa->func_ctor = 1;
    if (fa1->func_dtor)
      fa->func_dtor = 1;
}

/* Merge attributes.  */
static void merge_attr(AttributeDef *ad, AttributeDef *ad1)
{
    merge_symattr(&ad->a, &ad1->a);
    merge_funcattr(&ad->f, &ad1->f);

    if (ad1->section)
      ad->section = ad1->section;
    if (ad1->alias_target)
      ad->alias_target = ad1->alias_target;
    if (ad1->asm_label)
      ad->asm_label = ad1->asm_label;
    if (ad1->attr_mode)
      ad->attr_mode = ad1->attr_mode;
}

/* Merge some type attributes.  */
static void patch_type(Sym *sym, CType *type)
{
    if (!(type->t & VT_EXTERN) || IS_ENUM_VAL(sym->type.t)) {
        if (!(sym->type.t & VT_EXTERN))
            tcc_error("redefinition of '%s'", get_tok_str(sym->v, NULL));
        sym->type.t &= ~VT_EXTERN;
    }

    if (IS_ASM_SYM(sym)) {
        /* stay static if both are static */
        sym->type.t = type->t & (sym->type.t | ~VT_STATIC);
        sym->type.ref = type->ref;
    }

    if (!is_compatible_types(&sym->type, type)) {
        tcc_error("incompatible types for redefinition of '%s'",
                  get_tok_str(sym->v, NULL));

    } else if ((sym->type.t & VT_BTYPE) == VT_FUNC) {
        int static_proto = sym->type.t & VT_STATIC;
        /* warn if static follows non-static function declaration */
        if ((type->t & VT_STATIC) && !static_proto
            /* XXX this test for inline shouldn't be here.  Until we
               implement gnu-inline mode again it silences a warning for
               mingw caused by our workarounds.  */
            && !((type->t | sym->type.t) & VT_INLINE))
            tcc_warning("static storage ignored for redefinition of '%s'",
                get_tok_str(sym->v, NULL));

        /* set 'inline' if both agree or if one has static */
        if ((type->t | sym->type.t) & VT_INLINE) {
            if (!((type->t ^ sym->type.t) & VT_INLINE)
             || ((type->t | sym->type.t) & VT_STATIC))
                static_proto |= VT_INLINE;
        }

        if (0 == (type->t & VT_EXTERN)) {
            struct FuncAttr f = sym->type.ref->f;
            /* put complete type, use static from prototype */
            sym->type.t = (type->t & ~(VT_STATIC|VT_INLINE)) | static_proto;
            sym->type.ref = type->ref;
            merge_funcattr(&sym->type.ref->f, &f);
        } else {
            sym->type.t &= ~VT_INLINE | static_proto;
        }

        if (sym->type.ref->f.func_type == FUNC_OLD
             && type->ref->f.func_type != FUNC_OLD) {
            sym->type.ref = type->ref;
        }

    } else {
        if ((sym->type.t & VT_ARRAY) && type->ref->c >= 0) {
            /* set array size if it was omitted in extern declaration */
            sym->type.ref->c = type->ref->c;
        }
        if ((type->t ^ sym->type.t) & VT_STATIC)
            tcc_warning("storage mismatch for redefinition of '%s'",
                get_tok_str(sym->v, NULL));
    }
}

/* Merge some storage attributes.  */
static void patch_storage(Sym *sym, AttributeDef *ad, CType *type)
{
    if (type)
        patch_type(sym, type);

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport != ad->a.dllimport)
        tcc_error("incompatible dll linkage for redefinition of '%s'",
            get_tok_str(sym->v, NULL));
#endif
    merge_symattr(&sym->a, &ad->a);
    if (ad->asm_label)
        sym->asm_label = ad->asm_label;
    update_storage(sym);
}

/* copy sym to other stack */
static Sym *sym_copy(Sym *s0, Sym **ps)
{
    Sym *s;
    s = sym_malloc(), *s = *s0;
    s->prev = *ps, *ps = s;
    if (s->v < SYM_FIRST_ANOM) {
        ps = &table_ident[s->v - TOK_IDENT]->sym_identifier;
        s->prev_tok = *ps, *ps = s;
    }
    return s;
}

/* copy s->type.ref to stack 'ps' for VT_FUNC and VT_PTR */
static void sym_copy_ref(Sym *s, Sym **ps)
{
    int bt = s->type.t & VT_BTYPE;
    if (bt == VT_FUNC || bt == VT_PTR) {
        Sym **sp = &s->type.ref;
        for (s = *sp, *sp = NULL; s; s = s->next) {
            Sym *s2 = sym_copy(s, ps);
            sp = &(*sp = s2)->next;
            sym_copy_ref(s2, ps);
        }
    }
}

/* define a new external reference to a symbol 'v' */
static Sym *external_sym(int v, CType *type, int r, AttributeDef *ad)
{
    Sym *s;

    /* look for global symbol */
    s = sym_find(v);