/*
textdefs.c

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "textdefs.h"

int8_t SEP_STR[] = ", ";

/*
 * REMARK: The reason I use static table is to avoid dynamic allocation
 * and finally the manual call to "free all memory" which is irritating.

 * 8bits, unsigned text representation prebuilt made for optimizations.
 * The range it covers varies is from 0 to 256.
 */

int8_t TextBTable[256][4] = {
/*
* !!NOTE!! str_hex_sp_b is based on this elements' length.
* def prebuilt():
* 	s = ""
* 	for i in xrange(256):
* 		if ((i % 0x10) == 0):
* 			s += "\r\n"
* 		s += "\" %02x\", " % (i)
* 	return s
*/
" 00", " 01", " 02", " 03", " 04", " 05", " 06", " 07", " 08", " 09", " 0a", " 0b", " 0c", " 0d", " 0e", " 0f",
" 10", " 11", " 12", " 13", " 14", " 15", " 16", " 17", " 18", " 19", " 1a", " 1b", " 1c", " 1d", " 1e", " 1f",
" 20", " 21", " 22", " 23", " 24", " 25", " 26", " 27", " 28", " 29", " 2a", " 2b", " 2c", " 2d", " 2e", " 2f",
" 30", " 31", " 32", " 33", " 34", " 35", " 36", " 37", " 38", " 39", " 3a", " 3b", " 3c", " 3d", " 3e", " 3f",
" 40", " 41", " 42", " 43", " 44", " 45", " 46", " 47", " 48", " 49", " 4a", " 4b", " 4c", " 4d", " 4e", " 4f",
" 50", " 51", " 52", " 53", " 54", " 55", " 56", " 57", " 58", " 59", " 5a", " 5b", " 5c", " 5d", " 5e", " 5f",
" 60", " 61", " 62", " 63", " 64", " 65", " 66", " 67", " 68", " 69", " 6a", " 6b", " 6c", " 6d", " 6e", " 6f",
" 70", " 71", " 72", " 73", " 74", " 75", " 76", " 77", " 78", " 79", " 7a", " 7b", " 7c", " 7d", " 7e", " 7f",