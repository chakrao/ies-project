/*
mm.c:
This will manage the memory allocs and free's
Just used for basic initialization allocation, frees shouldn't happen too often

*/

#include "mm.h"
#include "main.h"
#include "multicore.h"
#include "common.h"
#include "vmmhelper.h"
#include "displaydebug.h"

//#define sendstringf(s,x...)
//#define sendstring(s)


/*
 * new mm
 *
 */

#define BASE_VIRTUAL_ADDRESS 0x1000000000ULL

//MAPPEDMEMORY is the range of virtual memory allocated for individual CPU threads mapping
#define MAPPEDMEMORY 0x08000000000ULL

//GLOBALMAPPEDMEMORY is the virtual memory allocated for the whole system for mapping
#define GLOBALMAPPEDMEMORY 0x07000000000ULL

//for virtual memory allocs
criticalSection AllocCS={.name="AllocCS", .debuglevel=2};
criticalSection GlobalMapCS={.name="GlobalMapCS", .debuglevel=2}