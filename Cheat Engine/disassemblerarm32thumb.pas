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
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3; offset:6));            mask: %1111111000000000; value: %0001100000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3; offset:6));            mask: %1111111000000000; value: %0001111000000000),
    (mnemonic:'MOV';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:0; maxval: 255));                             mask: %1111100000000000; value: %0010000000000000),
    (mnemonic:'CMP';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:0; maxval: 255));                             mask: %1111100000000000; value: %0010100000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:6; maxval: 7));   mask: %1111111000000000; value: %0001110000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:6; maxval: 255));                             mask: %1111100000000000; value: %0011000000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:6; maxval: 7));   mask: %1111111000000000; value: %0001111000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:6; maxval: 255));                             mask: %1111100000000000; value: %0011100000000000)
  );

  ThumbInstructionsDataProcessing16:array of TOpcode=(
    //010000**********
    (mnemonic:'AND'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000000000000),
    (mnemonic:'EOR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000001000000),
    (mnemonic:'LSL'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000010000000),
    (mnemonic:'LSR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000011000000),
    (mnemonic:'ASR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000100000000),
    (mnemonic:'ADC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000101000000),
    (mnemonic:'SBC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000110000000),
    (mnemonic:'ROR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000111000000),
    (mnemonic:'TST'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001000000000),
    (mnemonic:'RSB'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm_val0));            mask: %1111111111000000; value: %0100001001000000),
    (mnemonic:'CMP'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001010000000),
    (mnemonic:'CMN'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001011000000),
    (mnemonic:'ORR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001100000000),
    (mnemonic:'MUL'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001101000000),
    (mnemonic:'BIC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3_same; offset:0));mask: %1111111111000000; value: %0100001110000000),
    (mnemonic:'MVN'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001111000000)
  );

  ThumbInstructionsSpecialDataInstructionsAndBranchAndExchange16:array of TOpcode=(
    //010001**********;
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100010000000000),
    (mnemonic:'CMP';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100010100000000),
    (mnemonic:'MOV';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100011000000000),
    (mnemonic:'BX';   additions:[];  params:((ptype:pt_rreg4; offset:3));                                                       mask: %1111111110000111; value: %0100011100000000),
    (mnemonic:'BLX';  additions:[];  params:((ptype:pt_rreg4; offset:3));                                                       mask: %1111111110000111; value: %0100011110000000)
  );

  ThumbInstructionsLoadStoreSingleDataItem:array of TOpcode=(
    //0101************
    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101000000000000),
    (mnemonic:'STRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101001000000000),
    (mnemonic:'STRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101010000000000),
    (mnemonic:'LDRSB';additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101011000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101100000000000),
    (mnemonic:'LDRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101101000000000),
    (mnemonic:'LDRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101110000000000),
    (mnemonic:'LDRSH';additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101111000000000),

    //011*************
    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0110000000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0110100000000000),

    (mnemonic:'STRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0111000000000000),
    (mnemonic:'LDRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0111100000000000),

    //100*************
    (mnemonic:'STRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1000000000000000),
    (mnemonic:'LDRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1000100000000000),

    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1001000000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1001100000000000)


  );

  ThumbInstructionsConditionalBranchAndSupervisorCall16: array of TOpcode=(
  //1101********
    (mnemonic:'UDF';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:255));                      mask: %1111111100000000; value: %1101111000000000),
    (mnemonic:'SVC';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:255));                      mask: %1111111100000000; value: %1101111100000000),
    (mnemonic:'B';    additions:[opa_tcond8]; params:((ptype:pt_simm_shl1_label; offset:0; maxval:255)); mask: %1111000000000000; value: %1101000000000000)
  );

  //32 bit

  ThumbInstructionsUndefined: array of TOpcode=(
    (mnemonic:'UNDEFINED';  additions:[];  params:();  mask: %00000000000000000000000000000000; value: %00000000000000000000000000000000)
  );


  ThumbInstructionsDataProcessingModifiedImmediate: array of TOpcode=(
    //11110*0*********0***************
    (mnemonic:'TST';  additions:[]; params:((ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb));                                    mask: %11111011111100001000111100000000; value: %11110000000100000000111100000000),  //16
    (mnemonic:'TEQ';  additions:[]; params:((ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb));                                    mask: %11111011111100001000111100000000; value: %11110000100100000000111100000000),  //16
    (mnemonic:'CMN';  additions:[]; params:((ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb));                                    mask: %11111011111100001000111100000000; value: %11110001000100000000111100000000),  //16
    (mnemonic:'CMP';  additions:[]; params:((ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb));                                    mask: %11111011111100001000111100000000; value: %11110001101100000000111100000000),  //16
    (mnemonic:'MOV';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8),(ptype:pt_const_thumb));                              mask: %11111011111011111000000000000000; value: %11110000010011110000000000000000),  //15
    (mnemonic:'MVN';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8),(ptype:pt_const_thumb));                              mask: %11111011111011111000000000000000; value: %11110000011011110000000000000000),  //15
    (mnemonic:'AND';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110000000000000000000000000000),  //11
    (mnemonic:'BIC';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110000001000000000000000000000),  //11
    (mnemonic:'ORR';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110000010000000000000000000000),  //11
    (mnemonic:'ORN';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110000011000000000000000000000),  //11
    (mnemonic:'EOR';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110000100000000000000000000000),  //11
    (mnemonic:'ADD';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110001000000000000000000000000),  //11
    (mnemonic:'ADC';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110001010000000000000000000000),  //11
    (mnemonic:'SBC';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110001011000000000000000000000),  //11
    (mnemonic:'SUB';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110001101000000000000000000000),  //11
    (mnemonic:'RSB';  additions:[opa_s20]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb)); mask: %11111011111000001000000000000000; value: %11110001110000000000000000000000)   //11
  );

  ThumbInstructionsDataProcessingPlainBinaryImmediate: array of TOpcode=(
    //11110*1*********0***************
    (mnemonic:'ADR.W';additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_const_thumb_poslabel));                          mask: %11111011111111111000000000000000; value: %11110010000011110000000000000000),  //16
    (mnemonic:'ADR.W';additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_const_thumb_neglabel));                          mask: %11111011111111111000000000000000; value: %11110010101011110000000000000000),  //16
    (mnemonic:'ADDW'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb_noenc)); mask: %11111011111100001000000000000000; value: %11110010000000000000000000000000),  //12
    (mnemonic:'MOVW'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_const_thumb_noenc16));                           mask: %11111011111100001000000000000000; value: %11110010010000000000000000000000),  //12
    (mnemonic:'SUBW'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_const_thumb_noenc)); mask: %11111011111100001000000000000000; value: %11110010101000000000000000000000),  //12
    (mnemonic:'MOVT'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_const_thumb_noenc16));                           mask: %11111011111100001000000000000000; value: %11110010110000000000000000000000),  //12


    (mnemonic:'SBFX'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_immx; offset:$00000c06; maxval:$00000302),  (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111100001000000000100000; value: %11110011010000000000000000000000), //14
    (mnemonic:'SSAT16';additions:[];params:((ptype:pt_rreg4; offset:8), (ptype:pt_imm; offset:0; maxval:$f), (ptype:pt_rreg4; offset:16));                                                       mask: %11111111111100001111000011110000; value: %11110011001000000000000000000000), //20
    (mnemonic:'SSAT'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_imm; offset:0; maxval:$1f), (ptype:pt_rreg4; offset:16), (ptype:pt_shift5_thumb; offset:4));                   mask: %11111111110100001000000000100000; value: %11110011000000000000000000000000),  //13

    (mnemonic:'BFC'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_immx; offset:$00000c06; maxval:$00000302),  (ptype:pt_imm; offset:0; maxval:$1f));                              mask: %11111111111111111000000000100000; value: %11110011011011110000000000000000), //18
    (mnemonic:'BFI'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_immx; offset:$00000c06; maxval:$00000302),  (ptype:pt_imm; offset:0; maxval:$1f));  mask: %11111111111100001000000000100000; value: %11110011011000000000000000000000), //14

    (mnemonic:'USAT16';additions:[];params:((ptype:pt_rreg4; offset:8), (ptype:pt_imm; offset:0; maxval:$f), (ptype:pt_rreg4; offset:16));                                                       mask: %11111111111100001111000011110000; value: %11110011101000000000000000000000),
    (mnemonic:'USAT'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_imm; offset:0; maxval:$1f), (ptype:pt_rreg4; offset:16), (ptype:pt_shift5_thumb; offset:4));                   mask: %11111111110100001000000000100000; value: %11110011100000000000000000000000),

    (mnemonic:'UBFX'; additions:[]; params:((ptype:pt_rreg4; offset:8), (ptype:pt_rreg4; offset:16),(ptype:pt_immx; offset:$00000c06; maxval:$00000302),  (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111100001000000000100000; value: %11110011110000000000000000000000) //14


  );

  ThumbInstructionsBranchesAndMiscellaneousControl:array of TOpcode=(    );



  ThumbInstructionsLoadStoreMultiple32: array of TOpcode=(    );
  ThumbInstructionsLoadStoreDualLoadStoreExclusiveTableBranch32: array of TOpcode=(    );
  ThumbInstructionsDataProcessingShiftedRegister: array of TOpcode=(    );


  ThumbInstructionsStoreSingleDataItem: array of TOpcode=(    );
  ThumbInstructionsLoadByteMemoryHints: array of TOpcode=(    );
  ThumbInstructionsLoadHalfWordMemoryHints: array of TOpcode=(    );
  ThumbInstructionsLoadWord:   array of TOpcode=(    );
  ThumbInstructionsMultiplyMultiplyAccumulateAndAbsoleDifference:array of TOpcode=(    );
  ThumbInstructionsLongMultiplyLongMultiplyAccumulateAndDivide:array of TOpcode=(    );

  ThumbInstructionsChangeProcessorState:array of TOpcode=(    );
  ThumbInstructionsMiscellaneousControlInstructions:array of TOpcode=(    );



//--------------------------- instruction groups----------------------------//


 //


  ThumbGroupBase16: array of TInstructionGroup=(
    (mask:%1111110000000000; value: %0100000000000000; list: @ThumbInstructionsDataProcessing16; listType: igpInstructions; name: 'ThumbInstructionsDataProcessing'),
    (mask:%1111110000000000; value: %0100010000000000; list: @ThumbInstructionsSpecialDataInstructionsAndBranchAndExchange16; listType: igpInstructions; name: 'ThumbGroupSpecialDataInstructionsAndBranchAndExchange'),
    (mask:%1111100000000000; value: %0100100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //LDR
    (mask:%1111100000000000; value: %1010000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //ADR
    (mask:%1111100000000000; value: %1010100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //ADD
    (mask:%1111100000000000; value: %1100000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //STM
    (mask:%1111100000000000; value: %1100100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //LDM
    (mask:%1111100000000000; value: %1110000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //B


    (mask:%1111000000000000; value: %1011000000000000; list: @ThumbInstructionsMiscellaneous16BitInstructions; listType: igpInstructions; name: 'ThumbInstructionsMiscellaneous16BitInstructions'),


    (mask:%1111000000000000; value: %1101000000000000; list: @ThumbInstructionsConditionalBranchAndSupervisorCall16; listType: igpInstructions; name: 'ThumbGroupConditionalBranchAndSupervisorCall'),


    (mask:%1111000000000000; value: %0101000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),
    (mask:%1110000000000000; value: %0110000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),
    (mask:%1110000000000000; value: %1000000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),


    (mask:%1100000000000000; value: %0000000000000000; list: @ThumbInstructionsShiftAddSubtractMoveAndCompare16; listType: igpInstructions; name: 'ThumbInstructionsShiftAddSubtractMoveAndCompare')
  );



  //32
  ThumbGroupCoprocessorAdvancedSIMDAndFloatingPointInstructions: array of TInstructionGroup=();

  ThumbGroupBranchesAndMiscellaneousControl: array of TInstructionGroup=(
         //11110***********1***************          11110***********1***************
    (mask:%11111111111100001101000000000000; value: %11110011101000001000000000000000; list: @ThumbInstructionsChangeProcessorState; listType: igpInstructions; name: 'ThumbInstructionsChangeProcessorState'),
    (mask:%11111111111100001101000000000000; value: %11110011101100001000000000000000; list: @ThumbInstructionsMiscellaneousControlInstructions; listType: igpInstructions; name: 'ThumbInstructionsMiscellaneousControlInstructions'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ThumbInstructionsBranchesAndMiscellaneousControl; listType: igpInstructions; name: 'ThumbInstructionsBranchesAndMiscellaneousControl')
  );

  ThumbGroupAdvancedSIMDElementOrStructureLoadStoreInstructions: array of TInstructionGroup=();
  ThumbGroupDataProcessingRegister: array of TInstructionGroup=();


  ThumbGroupBase32: array of TInstructionGroup=(
    //note: the words are swapped
    //111*****************************

    (mask:%11111110010000000000000000000000; value: %11101000000000000000000000000000; list: @ThumbInstructionsLoadStoreMultiple32; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreMultiple32'),
    (mask:%11111110010000000000000000000000; value: %11101000010000000000000000000000; list: @ThumbInstructionsLoadStoreDualLoadStoreExclusiveTableBranch32; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreDualLoadStoreExclusiveTableBranch32'),
    (mask:%11111110000000000000000000000000; value: %11101010000000000000000000000000; list: @ThumbInstructionsDataProcessingShiftedRegister; listType: igpInstructions; name: 'ThumbInstructionsDataProcessingShiftedRegister'),
    (mask:%11111100000000000000000000000000; value: %11101100000000000000000000000000; list: @ThumbGroupCoprocessorAdvancedSIMDAndFloatingPointInstructions; listType: igpGroup; name: 'ThumbGroupCoprocessorAdvancedSUMDAndFloatingPointInstructions'),

    (mask:%11111010000000001000000000000000; value: %11110000000000000000000000000000; list: @ThumbInstructionsDataProcessingModifiedImmediate; listType: igpInstructions; name: 'ThumbInstructionsDataProcessingModifiedImmediate'),
    (mask:%11111010000000001000000000000000; value: %11110010000000000000000000000000; list: @ThumbInstructionsDataProcessingPlainBinaryImmediate; listType: igpInstructions; name: 'ThumbInstructionsDataProcessingPlainBinaryImmediate'),
    (mask:%11111000000000001000000000000000; value: %11110000000000001000000000000000; list: @ThumbGroupBranchesAndMiscellaneousControl; listType: igpGroup; name: 'ThumbGroupBranchesAndMiscellaneousControl'),

    (mask:%11111111000100000000000000000000; value: %11111000000000000000000000000000; list: @ThumbInstructionsStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsStoreSingleDataItem'),
    (mask:%11111110011100000000000000000000; value: %11111000000100000000000000000000; list: @ThumbInstructionsLoadByteMemoryHints; listType: igpInstructions; name: 'ThumbInstructionsLoadByteMemoryHints'),
    (mask:%11111110011100000000000000000000; value: %11111000001100000000000000000000; list: @ThumbInstructionsLoadHalfWordMemoryHints; listType: igpInstructions; name: 'ThumbInstructionsLoadHalfWordMemoryHints'),
    (mask:%11111110011100000000000000000000; value: %11111000010100000000000000000000; list: @ThumbInstructionsLoadWord; listType: igpInstructions; name: 'ThumbInstructionsLoadWord'),

    (mask:%11111110011100000000000000000000; value: %11111000011100000000000000000000; list: @ThumbInstructionsUndefined; listType: igpInstructions; name: 'ThumbInstructionsUndefined'),

    (mask:%11111111000100000000000000000000; value: %11111001000000000000000000000000; list: @ThumbGroupAdvancedSIMDElementOrStructureLoadStoreInstructions; listType: igpGroup; name: 'ThumbGroupAdvancedSIMDElementOrStructureLoadStoreInstructions'),
    (mask:%11111111000000000000000000000000; value: %11111010000000000000000000000000; list: @ThumbGroupDataProcessingRegister; listType: igpGroup; name: 'ThumbGroupDataProcessingRegister'),

    (mask:%11111111100000000000000000000000; value: %11111011000000000000000000000000; list: @ThumbInstructionsMultiplyMultiplyAccumulateAndAbsoleDifference; listType: igpInstructions; name: 'ThumbInstructionsMultiplyMultiplyAccumulateAndAbsoleDifference'),
    (mask:%11111111100000000000000000000000; value: %11111011100000000000000000000000; list: @ThumbInstructionsLongMultiplyLongMultiplyAccumulateAndDivide; listType: igpInstructions; name: 'ThumbInstructionsLongMultiplyLongMultiplyAccumulateAndDivide'),

    (mask:%11111100000000000000000000000000; value: %11111100000000000000000000000000; list: @ThumbGroupCoprocessorAdvancedSIMDAndFloatingPointInstructions;  listType: igpGroup; name: 'ThumbGroupCoprocessorAdvancedSIMDAndFloatingPointInstructions')

  );


var
  ThumbInstructionsAssemblerList: TStringHashList;

  tlbilist: TStringHashList;


{$ifdef armdev}
procedure DebugOutputOpcode(opcode: POpcode);
var
  s: string;
  i: integer;
  ti: PTypeInfo;
  tn: string;

  insideIndex: boolean;
begin
  s:='';
  insideIndex:=false;
  for i:=0 to length(opcode^.params)-1 do
  begin
    tn:='';
    if (insideIndex=false) and (opcode^.params[i].index<>ind_no) then
    begin
      insideindex:=true;
      tn:=tn+'[';
    end;

    ti:=TypeInfo(TThumbParameterType);
    tn:=tn+GetEnumName(ti, integer(opcode^.params[i].ptype));
    {
    if (insideIndex) and (opcode^.params[i].index in [ind_no, ind_stop, ind_stopexp, ind_single, ind_singleexp]) then
    begin
      insideindex:=false;
      tn:=tn+']';

      if opcode^.params[i].index in [ind_stopexp, ind_singleexp] then
        tn:=tn+'!';
    end; }

    if i>0 then s:=s+', ';
    s:=s+tn;
  end;


  if insideindex then
    s:=s+']';

  outputdebugstring(pchar(opcode^.mnemonic+'('+s+')'));
end;
{$endif}


function SignExtend(value: qword; mostSignificantBit: integer): qword; inline;
{
Signextends a given offset. mostSignificant bit defines what bit determines if it should be sign extended or not
}
begin
  if (value and (1 shl mostSignificantBit))<>0 then //needs to be sign extended
      value:=value or ((qword($ffffffffffffffff) shl mostSignificantBit));

  result:=value;
end;

function highestbit(v: dword): integer;
var i: integer;
begin
  case v of
        $ff: exit(8);
       $1ff: exit(9);
      $3fff: exit(14);
      $7fff: exit(15);
      $ffff: exit(16);
     $1ffff: exit(17);
     $3ffff: exit(18);
     $7ffff: exit(19);
    $ffffff: exit(23);

    $7ffffff: exit(26);
    else
    begin
      result:=0;
      for i:=31 downto 0 do
        if (v and (1 shl i))<>0 then exit(i);
    end;
  end;
end;

function ones(len: integer): qword;
begin
  result:=qword(qword(1) shl len)-1;
end;

function zeroextend(v: qword; bitlen: integer): qword;
begin
  result:=v and ones(bitlen);
end;

function ror(v: qword; esize: integer; r: integer): qword;
var a,b: qword;
begin
  a:=v shl (esize-r) and ones(esize);
  b:=v shr r and ones(esize);

  result:=(a or b) and ones(esize);

end;

function replicate(v: qword; sourceSize: integer; destinationSize: integer): qword;
var
  repval: qword;
  times: integer;
  i: integer;
  r: qword;
begin
  if sourcesize=0 then exit;

  repval:=v and zeroextend(v, sourcesize);

  times:=destinationsize div sourcesize;
  result:=0;
  for i:=1 to times do
    result:=(result shl sourcesize) or repval;

  result:=result or repval;
end;

function armbitmask(v: qword; datasize: integer): qword;
var
  d: bitpacked record
    imms: 0..$3f;
    immr: 0..$3f;
    immN: 0..1;
  end absolute v;

  len,len2: integer;
  levels,level2: integer;

  s: integer;
  r: integer;
  diff: integer;
  esize: integer;
  welem: qword;
  telem: qword;

  _d: integer;

  scratch: bitpacked record
    v: 0..$3f;
    tmask_and, wmask_and: 0..$3f;
    tmask_or, wmask_or: 0..$3f;
    levels: 0..$3f;
  end;

  tmask: qword;
  wmask: qword;
begin
  result:=0;
  scratch.v:=d.imms;
  scratch.v:=not scratch.v;


  len:=highestbit((d.immn shl 6) or (scratch.v));
  if len<1 then exit(0);

  scratch.v:=ones(len);
  levels:=scratch.v;

  s:=d.imms and levels;
  r:=d.immr and levels;
  diff:=s-r;

  esize:=1 shl len;

  _d:=diff and (ones(len-1));

  welem:=zeroextend(ones(s+1), esize);
  telem:=zeroextend(ones(_d+1), esize);

  wmask:=replicate(ror(welem, esize, r), esize, datasize);
  tmask:=replicate(telem, esize, datasize);

  exit(wmask);
end;

function bhsd(encoding2: integer): string;
begin
  case encoding2 and 3 of
    %00: exit('B');
    %01: exit('H');
    %10: exit('S');
    %11: exit('D');
  end;
end;

function bhsd2(encoding2: integer): string;
begin
  case encoding2 and 3 of
    %01: exit('B');
    %10: exit('H');
    %11: exit('S');
  end;
end;


function floatToFP8(f: single): byte;
var
  fi: single;
  fib: bitpacked record
    frac: 0..$3FFFFF;
    exp: 0..255;
    sign: 0..1;
  end absolute fi;

  rb: byte;
  r: bitpacked record
    frac: 0..$f;
    exp: 0..7;
    sign: 0..1;
  end absolute rb;

begin
  fi:=f;

  rb:=0;
  r.sign:=fib.sign;
  r.exp:=fib.Exp;
  r.frac:=fib.frac shr 18;

  result:=rb;

end;

function fp8tofloat(v: byte): single;
var
  n: integer;
  e: integer;
  f: integer;
  sign: integer;
  exp: integer;
  frac: integer;

  r: single;
  fr: bitpacked record
    sign: 1..1;
    exp: 0..255;
    frac: 0..$3FFFFF;
  end absolute r;

begin
  r:=0;
  n:=32;
  e:=8;
  f:=n-e-1;

  fr.sign:=(v shr 7) and 1;
  fr.exp:=(not((v shr 6) and 1) shr 7) or (replicate((v shr 6) and 1, 1,e-3) shl 2) or ((v shr 4) and 3);
  fr.frac:=(v and $f) shl 18;

  result:=r;

end;

function shiftTypeToInteger(s: string): integer;
begin
  if s='LSL' then exit(0);
  if s='LSR' then exit(1);
  if s='ASR' then exit(2);
  if s='ROR' then exit(3);
  result:=-1;
end;

function getShiftTypeString(t: integer): string;
begin
  case t and $3 of
    0: exit('LSL');
    1: exit('LSR');
    2: exit('ASR');
    3: exit('ROR');
  end;
end;

function ARMRegisterStringToInteger(s: string):integer;
begin
  result:=-1;
  if length(s)<2 then exit;
  s:=uppercase(s);

  try
    if s[1]='R' then exit(strtoint