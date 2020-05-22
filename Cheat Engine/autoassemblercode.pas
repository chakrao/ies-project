// Copyright Cheat Engine. All Rights Reserved.

(*
replaces {$luacode}/{$ccode} with a call to a safecall routine.
If lua:
This routine then calls a lua function in CE using the ceserver pipe with the saved state.
the lua function wraps the userprovided function with code that sets up the parameters to what the user wishes :"myvarname=RAX somethingelse=RDI"
on return of that lua function the given parameters get written back to the original state, which gets restored on function exit (This is slightly different from {$ccode} which allows specifying reference of val )

If c:
This routine then calls a c-compiled function.  One problem is that the address needs to be known before compilation, so has to be done on the 2nd pass again

replaces {$c} with nothing, but adds it to the total c-code (handy for headers, helper functions, libraries, etc...)
*)

unit autoassemblercode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolListHandler,tcclib;

type
  TAutoAssemblerCodePass2Data=record
    //luadata not needed


    cdata:record
      cscript: TStringlist;
      address: ptruint;
      bytesize: integer;
      usesxmm: boolean;
      references: array of record
        name: string;
        address: ptruint;
      end;

      symbols: array of record
        name: string;
        address: ptruint;
      end;

      linklist: array of record //list of symbolnames that need to be filled in after compilation.  Name can be found in referenced, fromname in the symbollist of the compiled program
        name: string;
        fromname: string;
      end;

      targetself: boolean;

      symbolPrefix: string;
      nodebug: boolean;
      sourceCodeInfo: TSourceCodeInfo;
{$ifdef windows}
      kernelAlloc: boolean;
{$endif}
    end;
  end;




procedure AutoAssemblerCodePass1(script: TStrings; out dataForPass2: TAutoAssemblerCodePass2Data; syntaxcheckonly: boolean; targetself: boolean);
procedure AutoAssemblerCodePass2(var dataForPass2: TAutoAssemblerCodePass2Data; symbollist: TSymbolListHandler);


implementation

uses {$ifdef windows}windows,{$endif}{$ifdef darwin}macport,macportdefines,math,{$endif}
  ProcessHandlerUnit, symbolhandler, luahandler, lua, lauxlib, lualib, StrUtils,
  Clipbrd, dialogs, lua_server, Assemblerunit, NewKernelHandler, DBK32functions,
  StringHashList, globals, networkInterfaceApi;


type
  TAACodeType=(ctLua, ctC);
  TLuaCodeParameter=record   //also CCode parameters
    varname: string;
    contextitem: integer;

    {
    ContextItem:
    0: RAX  / EAX
    1: RBX  / EBX
    2: RCX
    3: RDX
    4: RSI
    5: RDI
    6: RSP
    7: RBP
    8: R8
    9: R9
    10: R10
    11: R11
    12: R12
    13: R13
    14: R14
    15: R15
    16..31: ^ as float  (bit 4 is 1)  noted as RAXF, RBXF, RCXF, etc...
    32..47: XMM0..XMM15 (bytetables/bytetablestruct)
    48: XMM0.0 or XMM0.0F (float)
    49: XMM0.1
    50: XMM0.2
    51: XMM0.3
    52..55: XMM1.*
    56..59: XMM2.*
    60..53: XMM3.*
    64..67: XMM4.*
    68..71: XMM5.*
    72..75: XMM6.*
    76..79: XMM7.*
    80..83: XMM8.*
    84..87: XMM9.*
    88..91: XMM10.*
    92..95: XMM11.*
    96..99: XMM12.*
   100..103: XMM13.*
   104..107: XMM14.*
   108..111: XMM15.*
   112: XMM0.0D (double)
   113: XMM0.1D (double)
   114..115: XMM1.*D
   116..117: XMM2.*D
   118..119: XMM3.*D
   120..121: XMM4.*D
   122..123: XMM5.*D
   124..125: XMM6.*D
   126..127: XMM7.*D
   128..129: XMM8.*D
   130..131: XMM9.*D
   132..133: XMM10.*D
   134..135: XMM11.*D
   136..137: XMM12.*D
   138..139: XMM13.*D
   140..141: XMM14.*D
   142..143: XMM15.*D



    }
  end;

  TLuaCodeParams=array of TLuaCodeParameter;


var
  //list containing functions tcclib1.c defines.
  //If any of these functions are us