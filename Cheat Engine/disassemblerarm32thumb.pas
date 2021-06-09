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
                       pt_rreg3_exp, //same as rreg3 but with a !
                       pt_rreg3_1, //4 bit arm reg encoded as 3 bits and the last bit is in the extra field
                       pt_rreg4, //4 bit arm reg

                       pt_spreg, //just the sp reg, has no encoding bits
                       pt_imm_val0, //just #0
                       pt_imm,
                       pt_imm5_1_shl1_label,
                       pt_imm_shl2_poslabel, //positive label
                       pt_imm_shl2,
                       pt_immx, //offset if an aob of offsets, and maxval is an aob of bitlength (so max 4 encodings)
                       pt_simm_shl1_label,
                       pt_reglist13,
                       pt_reglist8,
                       pt_reglist8_exclude_rreg3, //reglist, but if extra offset in extra contains an rreg specified in this entry, it's invalid
                       pt_reglist8_withExtra, //the pbyte(@extra)[0]=extra registernr, pbyte(@extra)[1]=bitposition for 1/0
                       pt_cond,

                       //32 bit
                       pt_const_thumb, //A6.3.2
                       pt_const_thumb_noenc,
                       pt_const_thumb_noenc16, //same as pt_const_thumb_noenc but adds appends imm4 to tyhe value at offset 16
                       pt_const_thumb_poslabel,//pc relative
                       pt_const_thumb_neglabel,

                       pt_shift5_thumb
                       );



  TAParameters=record
    ptype: TThumbParameterType;
    offset: dword; //binary position  (in case of imm2/pt_reglist_*: offset is a 32-bit bitmask and assumed concatenated from left to right)
    maxval: dword;
    extra:  qword; //extra data for paramtypes
    optional: boolean;
    defvalue: integer; //in case of optional
    index: TIndexedParameter;
  end;

  TAParametersList=array of TAParameters;

  TInstructionUse=(iuBoth=0, iuAssembler=1, iuDisassembler=2);

  TOpcodeAdditions=(
                    opa_s20,
                    opa_ITCond_S, //if/then block condition, else S
                    opa_tcond8 //opcode followed by conditional (EQ, NZ, etc...) bit 8 contains the condition

                    );

  POpcodeArray=^topcodearray;
  TOpcode=record
    mnemonic: string;
    additions: set of TOpcodeAdditions;
    params: TAParametersList;
    mask: DWORD;
    value:DWORD;
    use: TInstructionUse;
    alt: popcodearray;
  end;
  POpcode=^TOpcode;
  TOpcodeArray=array of TOpcode;

  EInvalidInstruction=class(Exception);

  TThumbParameterTypes=set of TThumbParameterType;

  TThumbInstructionset=object
  private
    address: dword;
    opcode: uint32;
    size: integer;

    procedure InitThumbSupport;

    function ParseParametersForDisassembler(plist: TAParametersList): boolean;
    function ScanOpcodeList(const list: topcodearray): boolean;
    function ScanGroupList(const list: TInstructionGroupArray): boolean;
    //assembler
    function ParseParameterForAssembler(param:TAParameters; paramstr: string): boolean;
    function GuessTypes(param: string): TThumbParameterTypes;
  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var DisassembleAddress: ptruint{$ifdef armdev}; _opcode: dword{$endif}): string;
    function assemble(_address: ptruint; instruction: string): DWORD;
  end;

  {$ifdef armdev}
  procedure GetThumbInstructionsAssemblerListDebug(r: tstrings);
  {$endif}




implementation

{$ifndef armdev}
uses math, NewKernelHandler,ProcessHandlerUnit,StringHashList;
{$else}
uses StringHashList, math, windows, Rtti, RttiUtils, TypInfo;
{$endif}

const
  ArmRegisters : array [0..15] of string=('R0','R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','FP','IP','SP','LR','PC');
  ArmConditions: array [0..15] of string=('EQ','NE','CS', 'CC', 'MI', 'PL', 'VS', 'VC', 'HI', 'LS', 'GE', 'LT', 'GT', 'LE', '','NV');


  ThumbInstructionsBase16: array of TOpcode=(
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_imm_shl2_poslabel; offset:0; maxval:255));          mask: %1111100000000000; value: %0100100000000000),
    (mnemonic:'ADR';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_imm_shl2_poslabel; offset:0; maxval:255));          mask: %1111100000000000; value: %1010000000000000),
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:255)); mask: %1111100000000000; value: %1010100000000000),
    (mnemonic:'STM';  additions:[];  params:((ptype:pt_rreg3_exp; offset:8; maxval:7), (ptype:pt_reglist13; offset:0));                          mask: %1111100000000000; value: %1100000000000000),
    (mnemonic:'LDM';  additions:[];  params:((ptype:pt_rreg3_exp; offset:8; maxval:7), (ptype:pt_reglist8_exclude_rreg3; offset:0;maxval:0; extra:8));   mask: %1111100000000000; value: %1100100000000000),
    (mnemonic:'B';    additions:[];  params:((ptype:pt_simm_shl1_label; offset:0; maxval:$7ff));                                                 mask: %1111100000000000; value: %1110000000000000)
  );

  ThumbInstructionsMiscellaneous16BitInstructions: array of TOpcode=(
    //1011************
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_spreg), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:127));  mask: %1111111110000000; value: %1011000000000000),
    (mnemonic:'SUB';  additions:[];  params:((ptype:pt_spreg), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:127));  mask: %1111111110000000; value: %1011000010000000),

    (mnemonic:'CBZ'; additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_imm5_1_shl1_label;offset:3;maxval:$1f;extra:9));mask: %1111110100000000; value: %1011000100000000),
    (mnemonic:'CBNZ';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_imm5_1_shl1_label;offset:3;maxval:$1f;extra:9));mask: %1111110100000000; value: %1011100100000000),

    (mnemonic:'PUSH'; additions:[];  params:((ptype:pt_reglist8_withExtra; offset:0; maxval:0; extra:$080e));                 mask: %1111111000000000; value: %1011010000000000),
    (mnemonic:'SETEND'; additions:[];  params:((ptype:pt_imm; offset:3; maxval:1));                                           mask: %1111111111110111; value: %1011011001010000),

    (mnemonic:'CPS'; additions:[];  params:();                                                                                mask: %1111111111101000; value: %1011011001100000),//todo: fill in

    (mnemonic:'REV';  additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101000000000),
    (mnemonic:'REV16';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101001000000),
    (mnemonic:'REVSH';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101011000000),

    (mnemonic:'POP'; additions:[];  params:((ptype:pt_reglist8_withExtra; offset:0; maxval:0; extra:$080f));                  mask: %1111111000000000; value: %1011110000000000),

    (mnemonic:'BKPT'; additions:[]; params:((ptype:pt_imm; offset:0; maxval:255));                                            mask: %1111111100000000; value: %1011111000000000),


    //if then and hints

    (mnemonic:'NOP'; additions:[]; params:();                                                                                 mask: %1111111111111111; value: %1011111100000000),
    (mnemonic:'YIELD'; additions:[]; params:();                                                                               mask: %1111111111111111; value: %1011111100010000),
    (mnemonic:'WFE'; additions:[]; params:();                                                                                 mask: %1111111111111111; value: %1011111100100000),
    (mnemonic:'WFI'; additions:[]; params:();                                                                                 mask: %1111111111111111; value: %1011111100110000),
    (mnemonic:'SEV'; additions:[]; params:();                                                                                 mask: %1111111111111111; value: %1011111101000000),

    (mnemonic:'IT'; additions:[];  params:((ptype:pt_cond; offset:4; maxval:15), (ptype:pt_imm; offset:0; maxval:15));        mask: %1111111100000000; value: %1011111100000000)

  );

  ThumbInstructionsShiftAddSubtractMoveAndCompare16:array of TOpcode=(
    //00**************
    (mnemonic:'MOVS'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                                    mask: %1111111111000000; value: %0000000000000000),
    (mnemonic:'LSL';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0000000000000000),
    (mnemonic:'LSR';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0000100000000000),
    (mnemonic:'ASR';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0001000000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset