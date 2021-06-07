//copyright Cheat Engine 2022. All rights reserved
unit disassemblerArm32Thumb;

{$mode objfpc}{$H+}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
interface

uses
  Classes, SysUtils, LastDisassembleData;



type
  TInstructionGroupPointerType=(igpGroup, igpInstructions);
  TInstructionGroup=record
    mask: DWORD;
    value: DWORD;
    list: pointer;
    listType: TInstructionGroupPointerType;
    name: string;
  end;
  TInstructionGroupArray=array of TInstructionGroup;
  PInstructionGroupArray=^TInstructionGroupArray;


  TIndexedParameter=(ind_no, ind_index, ind_stop, ind_stopexp, ind_single, ind_singleexp);
  TThumbParameterType=(pt_rreg3, //3 bit to define a reg in r0 to r7
                       pt_rreg3_same, //no-encoding, but must make sure that the given register is the same as already encoded at the given offset
            