/*
 * helpers.h
 *
 *  Created on: Nov 11, 2017
 *      Author: eric
 */

#ifndef HELPERS_H_
#define HELPERS_H_

extern EFI_SYSTEM_TABLE *st;

#pragma pack(2) //alignment of 2 bytes
typedef struct tagGDT
{
  UINT16 wLimit;
  UINT64 vector;
} GDT, *PGDT;
#pragma pack()

#pragma pack(1) //alignment of 1 byte
typedef struct tagINT_VECTOR
{
  UINT16  wLowOffset;
  UINT16  wSelector;
  UINT8  bUnused;
  UINT8    bAccessFlags;

  /*
  unsigned gatetype  : 3; //101=Task, 110=interrupt, 111=trap
  unsigned gatesize  : 1; //1=32bit, 0=16bit
  unsigned zero      : 1;
  unsigned DPL       : 2;
  unsigned P         : 1;
  */
  UINT16  wHighOffset;
  UINT32 TopOffset;
  UINT32 Reserved;

} INT_VECTOR, *PINT_VECTOR;
#pragma pack()

#pragma pack(2) //allignemnt of 2 byte
typedef struct tagIDT
{
  UINT16 wLimit;
  PINT_VECTOR vector;
} IDT, *PIDT