/*
 * dbvmoffload.h
 *
 *  Created on: Nov 21, 2017
 *      Author: eric
 */

#ifndef DBVMOFFLOAD_H_
#define DBVMOFFLOAD_H_

#include <efi.h>
#include <efilib.h>


#pragma pack (1)
typedef struct _PTE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PAT       :  1; // PAT
        unsigned G         :  1; // global page
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        //unsigned PFN       : 20; // page-frame number
} PTE, *PPTE;

typedef struct _PDE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PS        :  1; // reserved (0)
        unsigned G         :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        //unsigned PFN       : 20; // page-frame number
} PDE, *PPDE;

typedef struct _PDE2MB
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned reserved1 :  1; // reserved (0)
        unsigned PS        :  1; // reserved (0)
        unsigned reserved3 :  1; // reserved (0)
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
        //unsigned PFN       : 20; // page-frame number (>> 13 instead of >>12);
} PDE2MB, *PPDE2MB;



typedef struct _PTE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // page-level write-through
        unsigned PCD       :  1; // page-level cache disabled
        unsigned A         :  1; // accessed
        unsigned D         :  1; // dirty
        unsigned PAT       :  1; //
        unsigned G         :  1; // global page
        unsigned A1        :  1; // available 1 aka copy-on-write
        unsigned A2        :  1; // available 2/ is 1 when paged to disk
        unsigned A3        :  1; // available 3
    //the following 2 items cause a problem in ms's compiler
        unsigned PFN       : 24; // page-frame number
        unsigned reserved  : 28;
} PTE_PAE, *PPTE_PAE;

typedef struct _PDE_PAE
{
        unsigned P         :  1; // present (1 = present)
        unsigned RW        :  1; // read/write
        unsigned US        :  1; // user/supervisor
        unsigned PWT       :  1; // pa