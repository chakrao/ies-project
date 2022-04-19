/*
 * apic.c
 *
 *  Created on: Jun 21, 2009
 *      Author: erich
 */


#include "main.h"
#include "common.h"
#include "apic.h"
#include "msrnames.h"
#include "mm.h"

int getHighestPendingInter