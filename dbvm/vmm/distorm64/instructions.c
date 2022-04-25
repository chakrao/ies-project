
/*
instructions.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/

#include "instructions.h"

#include "insts.h"
#include "prefix.h"
#include "textdefs.h"
#include "x86defs.h"
#include "wstring.h"

/*
I use the trie data structure as I found it most fitting to a disassembler mechanism.
When you read a byte and have to decide if it's enough or you should read more bytes, 'till you get to the instruction information.
It's really fast because you POP the instruction info in top 3 iterates on the DB, because an instruction can be formed from two bytes + 3 bits reg from the ModR/M byte.
For a simple explanation, check this out:
http://www.csse.monash.edu.au/~lloyd/tildeAlgDS/Tree/Trie/
Futher reading: http://en.wikipedia.org/wiki/Trie

The first GATE (array you read off a trie data structure), as I call them, is statically allocated by the compiler.
The second and third gates if used are being allocated dynamically by the instructions-insertion functionality.

How would such a thing look in memory, say we support 4 instructions with 3 bytes top (means 2 dynamically allocated gates).

->
|-------|                                0,
|0|     -------------------------------> |-------|
|1|RET  |      1,                        |0|AND  |
|2|     -----> |-------|                 |1|XOR  |
|3|INT3 |      |0|PUSH |                 |2|OR   |         0,3,
|-------|      |1|POP  |                 |3|     --------->|-------|
               |2|PUSHF|                 |-------|         |0|ROR  |
               |3|POPF |                                   |1|ROL  |
               |-------|                                   |2|SHR  |
                                                           |3|SHL  |
                                                           |-------|

Of course, this is NOT how Intel instructions set looks!!!
but I just wanted to give a small demonstration.
Now the instructions you get from such a trie DB goes like this:

0, 0 - AND
0, 1 - XOR
0, 2 - OR
0, 3, 0, ROR
0, 3, 1, ROL
0, 3, 2, SHR
0, 3, 3, SHL
1 - RET
2, 0 - PUSH
2, 1 - POP
2, 2 - PUSHF
2, 3 - POPF
3 - INT3

I guess it's clear by now.
So now, if you read 0, you know that you have to enter the second gate(list) with the second byte specifying the index.
But if you read 1, you know that you go to an instruction (in this case, a RET).
That's why there's an Instruction-Node structure, it tells you whether you got to an instruction or another list
so you should keep on reading byte).

In Intel, you could go through 4 gates at top, because there're instructions which are built from 2 bytes and another smaller list
for the REG part, or newest SSE4 instructions which use 4 bytes for opcode.
Therefore, Intel's first gate is 256 long, and other gates are 256 (/72) or 8 long, yes, it costs pretty much alot of memory
for non-used defined instructions, but I think that it still rocks.
*/

/*
 * This function is reponsible to return the instruction information of the first found in code.
 * It returns the _InstInfo of the found instruction, otherwise NULL.
 * code should point to the ModR/M byte upon exit (if used), or after the instruction binary code itself.
 * This function is NOT decoding-type dependant, it is up to the caller to see whether the instruction is valid.
 * Get the instruction info, using a Trie data structure.
 * I call it "raw", because it simply locates an instruction, it doesn't care what bytes it's using, such as prefixes.
 */
static _InstInfo* locate_raw_inst(const uint8_t** code0, int* codeLen0, _OffsetType* codeOffset0, _WString* instructionHex, int isREXPrefixValid, _DecodeType dt)
{
	const uint8_t* code = *code0;
	int codeLen = *codeLen0;
	_OffsetType codeOffset = *codeOffset0;

	unsigned int tmpIndex0 = 0, tmpIndex1 = 0, tmpIndex2 = 0, tmpIndex3 = 0;
	_InstNode* in = NULL;
	_InstInfo* ii = NULL;

	/* Precaution. */
	if (codeLen <= 0) return NULL;

	tmpIndex0 = *code;

	/* Check for NULL node for index 0. */
	in = (_InstNode*)Instructions.list[Instructions.ids[tmpIndex0]];
	if (in == NULL) return NULL;

	/* Single byte instruction (OCST_1BYTE). */
	if (in->type == INT_INFO) {
		str_hex_b(instructionHex, tmpIndex0);

		codeLen -= 1;
		if (codeLen < 0) return NULL;
		code += 1;
		codeOffset += 1;
		*code0 = code;
		*codeLen0 = codeLen;
		*codeOffset0 = codeOffset;

		/*
		 * ARPL/MOVSXD share the same instruction number, and both have different operands and mnemonics, of course.
		 * Practically, I couldn't come up with a comfortable way to merge the operands' types of ARPL/MOVSXD.
		 * And since the DB can't be patched dynamically, because the DB has to be multi-threaded compliant,
		 * I have no choice but to check for ARPL/MOVSXD right here - "right about now, the funk soul brother, check it out now, the funk soul brother...", fatboy slim
		 */
		if (tmpIndex0 == INST_ARPL_INDEX) return ((dt == Decode64Bits) ? (_InstInfo*)&II_movsxd : &II_arpl);

		return (_InstInfo*)in;