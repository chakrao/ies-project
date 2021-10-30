/*
 *  COFF file handling for TCC
 * 
 *  Copyright (c) 2003, 2004 TK
 *  Copyright (c) 2004 Fabrice Bellard
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

#include "tcc.h"

#define MAXNSCNS 255		/* MAXIMUM NUMBER OF SECTIONS         */
#define MAX_STR_TABLE 1000000
AOUTHDR o_filehdr;		/* OPTIONAL (A.OUT) FILE HEADER       */

SCNHDR section_header[MAXNSCNS];

#define MAX_FUNCS 1000
#define MAX_FUNC_NAME_LENGTH 128

int nFuncs;
char Func[MAX_FUNCS][MAX_FUNC_NAME_LENGTH];
char AssociatedFile[MAX_FUNCS][MAX_FUNC_NAME_LENGTH];
int LineNoFilePtr[MAX_FUNCS];
int EndAddress[MAX_FUNCS];
int LastLineNo[MAX_FUNCS];
int FuncEntries[MAX_FUNCS];

int OutputTheSection(Section * sect);
short int GetCoffFlags(const char *s);
void SortSymbolTable(TCCState *s1);
Section *FindSection(TCCState * s1, const char *sname);

int C67_main_entry_point;

int FindCoffSymbolIndex(TCCState * s1, const char *func_name);
int nb_syms;

typedef struct {
    long tag;
    long size;
    long fileptr;
    long nextsym;
    short int dummy;
} AUXFUNC;

typedef struct {
    long regmask;
    unsigned short lineno;
    unsigned short nentries;
    int localframe;
    int nextentry;
    short int dummy;
} AUXBF;

typedef struct {
    long dummy;
    unsigned short lineno;
    unsigned short dummy1;
    int dummy2;
    int dummy3;
    unsigned short dummy4;
} AUXEF;

ST_FUNC int tcc_output_coff(TCCState *s1, FILE *f)
{
    Section *tcc_sect;
    SCNHDR *coff_sec;
    int file_pointer;
    char *Coff_str_table, *pCoff_str_table;
    int CoffTextSectionNo, coff_nb_syms;
    FILHDR file_hdr;		/* FILE HEADER STRUCTURE              */
    Section *stext, *sdata, *sbss;
    int i, NSectionsToOutput = 0;

    Coff_str_table = pCoff_str_table = NULL;

    stext = FindSection(s1, ".text");
    sdata = FindSection(s1, ".data");
    sbss = FindSection(s1, ".bss");

    nb_syms = symtab_section->data_offset / sizeof(Elf32_Sym);
    coff_nb_syms = FindCoffSymbolIndex(s1, "XXXXXXXXXX1");

    file_hdr.f_magic = COFF_C67_MAGIC;	/* magic number */
    file_hdr.f_timdat = 0;	/* time & date stamp */
    file_hdr.f_opthdr = sizeof(AOUTHDR);	/* sizeof(optional hdr) */
    file_hdr.f_flags = 0x1143;	/* flags (copied from what code composer does) */
    file_hdr.f_TargetID = 0x99;	/* for C6x = 0x0099 */

    o_filehdr.magic = 0x0108;	/* see magic.h                          */
    o_filehdr.vstamp = 0x0190;	/* version stamp                        */
    o_filehdr.tsize = stext->data_offset;	/* text size in bytes, padded to FW bdry */
    o_filehdr.dsize = sdata->data_offset;	/* initialized data "  "                */
    o_filehdr.bsize = sbss->data_offset;	/* uninitialized data "   "             */
    o_filehdr.entrypt = C67_main_entry_point;	/* entry pt.                          */
    o_filehdr.text_start = stext->sh_addr;	/* base of text used for this file      */
    o_filehdr.data_start = sdata->sh_addr;	/* base of data used for this file      */


    // create all the section headers

    file_pointer = FILHSZ + sizeof(AOUTHDR);

    CoffTextSectionNo = -1;

    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    NSectionsToOutput++;

	    if (CoffTextSectionNo == -1 && tcc_sect == stext)
		CoffTextSectionNo = NSectionsToOutput;	// rem which coff sect number the .text sect is

	    strcpy(coff_sec->s_name, tcc_sect->name);	/* section name */

	    coff_sec->s_paddr = tcc_sect->sh_addr;	/* physical address */
	    coff_sec->s_vaddr = tcc_sect->sh_addr;	/* virtual address */
	    coff_sec->s_size = tcc_sect->data_offset;	/* section size */
	    coff_sec->s_scnptr = 0;	/* file ptr to raw data for section */
	    coff_sec->s_relptr = 0;	/* file ptr to relocation */
	    coff_sec->s_lnnoptr = 0;	/* file ptr to line numbers */
	    coff_sec->s_nreloc = 0;	/* number of relocation entries */
	    coff_sec->s_flags = GetCoffFlags(coff_sec->s_name);	/* flags */
	    coff_sec->s_reserved = 0;	/* reserved byte */
	    coff_sec->s_page = 0;	/* memory page id */

	    file_pointer += sizeof(SCNHDR);
	}
    }

    file_hdr.f_nscns = NSectionsToOutput;	/* number of sections */

    // now loop through and determine file pointer locations
    // for the raw data


    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    // put raw data
	    coff_sec->s_scnptr = file_pointer;	/* file ptr to raw data for section */
	    file_pointer += coff_sec->s_size;
	}
    }

    // now loop through and determine file pointer locations
    // for the relocation data

    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    // put relocations data
	    if (coff_sec->s_nreloc > 0) {
		coff_sec->s_relptr = file_pointer;	/* file ptr to relocation */
		file_pointer += coff_sec->s_nreloc * sizeof(struct reloc);
	    }
	}
    }

    // now loop through and determine file pointer locations
    // for the line number data

    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	coff_sec->s_nlnno = 0;
	coff_sec->s_lnnoptr = 0;

	if (s1->do_debug && tcc_sect == stext) {
	    // count how many line nos data

	    // also find association between source file name and function
	    // so we can sort the symbol table


	    Stab_Sym *sym, *sym_end;
	    char func_name[MAX_FUNC_NAME_LENGTH],
		last_func_name[MAX_FUNC_NAME_LENGTH];
	    unsigned long func_addr, last_pc, pc;
	    const char *incl_files[INCLUDE_STACK_SIZE];
	    int incl_index, len, last_line_num;
	    const char *str, *p;

	    coff_sec->s_lnnoptr = file_pointer;	/* file ptr to linno */


	    func_name[0] = '\0';
	    func_addr = 0;
	    incl_index = 0;
	    last_func_name[0] = '\0';
	    last_pc = 0xffffffff;
	    last_line_num = 1;
	    sym = (Stab_Sym *) stab_section->data + 1;
	    sym_end =
		(Stab_Sym *) (stab_section->data +
			      stab_section->data_offset);

	    nFuncs = 0;
	    while (sym < sym_end) {
		switch (sym->n_type) {
		    /* function start or end */
		case N_FUN:
		    if (sym->n_strx == 0) {
			// end of function

			coff_sec->s_nlnno++;
			file_pointer += LINESZ;

			pc = sym->n_value + func_addr;
			func_name[0] = '\0';
			func_addr = 0;
			EndAddress[nFuncs] = pc;
			FuncEntries[nFuncs] =
			    (file_pointer -
			     LineNoFilePtr[nFuncs]) / LINESZ - 1;
			LastLineNo[nFuncs++] = last_line_num + 1;
		    } else {
			// beginning of function

			LineNoFilePtr[nFuncs] = file_pointer;
			coff_sec->s_nlnno++;
			file_pointer += LINESZ;

			str =
			    (const char *) stabstr_section->data +
			    sym->n_strx;

			p = strchr(str, ':');
			if (!p) {
			    pstrcpy(func_name, sizeof(func_name), str);
			    pstrcpy(Func[nFuncs], sizeof(func_name), str);
			} else {
			    len = p - str;
			    if (len > sizeof(func_name) - 1)
				len = sizeof(func_name) - 1;
			    memcpy(func_name, str, len);
			    memcpy(Func[nFuncs], str, len);
			    func_name[len] = '\0';
			}

			// save the file that it came in so we can sort later
			pstrcpy(AssociatedFile[nFuncs], sizeof(func_name),
				incl_files[incl_index - 1]);

			func_addr = sym->n_value;
		    }
		    break;

		    /* line number info */
		case N_SLINE:
		    pc = sym->n_value + func_addr;

		    last_pc = pc;
		    last_line_num = sym->n_desc;

		    /* XXX: slow! */
		    strcpy(last_func_name, func_name);

		    coff_sec->s_nlnno++;
		    file_pointer += LINESZ;
		    break;
		    /* include files */
		case N_BINCL:
		    str =
			(const char *) stabstr_section->data + sym->n_strx;
		  add_incl:
		    if (incl_index < INCLUDE_STACK_SIZE) {
			incl_files[incl_index++] = str;
		    }
		    break;
		case N_EINCL:
		    if (incl_index > 1)
			incl_index--;
		    break;
		case N_SO:
		    if (sym->n_strx == 0) {
			incl_index = 0;	/* end of translation unit */
		    } else {
			str =
			    (const char *) stabstr_section->data +
			    sym->n_strx;
			/* do not add path */
			len = strlen(str);
			if (len > 0 && str[len - 1] != '/')
			    goto add_incl;
		    }
		    break;
		}
		sym++;
	    }
	}

    }

    file_hdr.f_symptr = file_pointer;	/* file pointer to symtab */

    if (s1->do_debug)
	file_hdr.f_nsyms = coff_nb_syms;	/* number of symtab entries */
    else
	file_hdr.f_nsyms = 0;

    file_pointer += file_hdr.f_nsyms * SYMNMLEN;

    // OK now we are all set to write the file


    fwrite(&file_hdr, FILHSZ, 1, f);
    fwrite(&o_filehdr, sizeof(o_filehdr), 1, f);

    // write section headers
    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    fwrite(coff_sec, sizeof(SCNHDR), 1, f);
	}
    }

    // write raw data
    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    fwrite(tcc_sect->data, tcc_sect->data_offset, 1, f);
	}
    }

    // write relocation data
    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (OutputTheSection(tcc_sect)) {
	    // put relocations data
	    if (coff_sec->s_nreloc > 0) {
		fwrite(tcc_sect->reloc,
		       coff_sec->s_nreloc * sizeof(struct reloc), 1, f);
	    }
	}
    }


    // group the symbols in order of filename, func1, func2, etc
    // finally global symbols

    if (s1->do_debug)
	SortSymbolTable(s1);

    // write line no data

    for (i = 1; i < s1->nb_sections; i++) {
	coff_sec = &section_header[i];
	tcc_sect = s1->sections[i];

	if (s1->do_debug && tcc_sect == stext) {
	    // count how many line nos data


	    Stab_Sym *sym, *sym_end;
	    char func_name[128], last_func_name[128];
	    unsigned long func_addr, last_pc, pc;
	    const char *incl_files[INCLUDE_STACK_SIZE];
	    int incl_index, len, last_line_num;
	    const char *str, *p;

	    LINENO CoffLineNo;

	    func_name[0] = '\0';
	    func_addr = 0;
	    incl_index = 0;
	    last_func_name[0] = '\0';
	    last_pc = 0;
	    last_line_num = 1;
	    sym = (Stab_Sym *) stab_section->data + 1;
	    sym_end =
		(Stab_Sym *) (stab_section->data +
			      stab_section->data_offset);

	    while (sym < sym_end) {
		switch (sym->n_type) {
		    /* function start or end */
		case N_FUN:
		    if (sym->n_strx == 0) {
			// end of function

			CoffLineNo.l_addr.l_paddr = last_pc;
			CoffLineNo.l_lnno = last_line_num + 1;
			fwrite(&CoffLineNo, 6, 1, f);

			pc = sym->n_value + func_addr;
			func_name[0] = '\0';
			func_addr = 0;
		    } else {
			// beginning of function

			str =
			    (const char *) stabstr_section->data +
			    sym->n_strx;


			p = strchr(str, ':');
			if (!p) {
			    pstrcpy(func_name, sizeof(func_name), str);
			} else {
			    len = p - str;
			    if (len > sizeof(func_name) - 1)
				len = sizeof(func_name) - 1;
			    memcpy(func_name, str, len);
			    func_name[len] = '\0';
			}
			func_addr = sym->n_value;
			last_pc = func_addr;
			last_line_num = -1;

			// output a function begin

			CoffLineNo.l_addr.l_symndx =
			    FindCoffSymbolIndex(s1, func_name);
			CoffLineNo.l_lnno = 0;

			fwrite(&CoffLineNo, 6, 1, f);
		    }
		    break;

		    /* line number info */
		case N_SLINE:
		    pc = sym->n_value + func_addr;


		    /* XXX: slow! */
		    strcpy(last_func_name, func_name);

		    // output a line reference

		    CoffLineNo.l_addr.l_paddr = last_pc;

		    if (last_line_num == -1) {
			CoffLineNo.l_lnno = sym->n_desc;
		    } else {
			CoffLineNo.l_lnno = last_line_num + 1;
		    }

		    fwrite(&CoffLineNo, 6, 1, f);

		    last_pc = pc;
		    last_line_num = sym->n_desc;

		    break;

		    /* include files */
		case N_BINCL:
		    str =
			(const char *) stabstr_section->data + sym->n_strx;
		  add_incl2:
		    if (incl_index < INCLUDE_STACK_SIZE) {
			incl_files[incl_index++] = str;
		    }
		    break;
		case N_EINCL:
		    if (incl_index > 1)
			incl_index--;
		    break;
		case N_SO:
		    if (sym->n_strx == 0) {
			incl_index = 0;	/* end of translation unit */
		    } else {
			str =
			    (const char *) stabstr_section->data +
			    sym->n_strx;
			/* do not add path */
			len = strlen(str);
			if (len > 0 && str[len - 1] != '/')
			    goto add_incl2;
		    }
		    break;
		}
		sym++;
	    }
	}
    }

    // write symbol table
    if (s1->do_debug) {
	int k;
	struct syment csym;
	AUXFUNC auxfunc;
	AUXBF auxbf;
	AUXEF auxef;
	int i;
	Elf32_Sym *p;
	const char *name;
	int nstr;
	int n = 0;

	Coff_str_table = (char *) tcc_malloc(MAX_STR_TABLE);
	pCoff_str_table = Coff_str_table;
	nstr = 0;

	p = (Elf32_Sym *) symtab_section->data;


	for (i = 0; i < nb_syms; i++) {

	    name = symtab_section->link->data + p->st_name;

	    for (k = 0; k < 8; k++)
		csym._n._n_name[k] = 0;

	    if (strlen(name) <= 8) {
		strcpy(csym._n._n_name, name);
	    } else {
		if (pCoff_str_table - Coff_str_table + strlen(name) >
		    MAX_STR_TABLE - 1)
		    tcc_error("String table too large");

		csym._n._n_n._n_zeroes = 0;
		csym._n._n_n._n_offset =
		    pCoff_str_table - Coff_str_table + 4;

		strcpy(pCoff_str_table, name);
		pCoff_str_table += strlen(name) + 1;	// skip over null
		nstr++;
	    }

	    if (p->st_info == 4) {
		// put a filename symbol
		csym.n_value = 33;	// ?????
		c