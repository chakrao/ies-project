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
  //If any of these functions are used, compile tcclib1-ce.c into the target process.
  //for __m* make
  tcclibimportlist: TStringHashList;

  {
  32-bit only:
  __divdi3
  __moddi3
  __udivdi3
  __umoddi3
  __ashrdi3
  __lshrdi3
  __ashldi3
  __floatundisf

  //32 and 64-bit:
  __floatundidf
  __floatundixf
  __fixunssfdi
  __fixsfdi
  __fixunsdfdi
  __fixdfdi
  __fixunsxfdi
  __fixxfdi
  }



procedure parseLuaCodeParameters(s: string; var output: TLuaCodeParams);
var
  i: integer;
  r,r2,r3: TStringArray;


  varname, regname: string;
  so: TStringSplitOptions;
  o: TLuaCodeParameter;
  xmmnr: integer;
  subnr: integer;
  st: string;
begin
  setlength(output,0);

  r:=s.Split(' ');
  for i:=0 to length(r)-1 do
  begin
    FillByte(o, sizeof(o),0);

    r2:=r[i].Split('=');

    if length(r2)<>2 then
      continue;

    varname:=r2[0];
    regname:=uppercase(r2[1]);


    o.varname:=varname;


    case regname of
      'EAX','RAX': o.contextitem:=0;
      'EBX','RBX': o.contextitem:=1;
      'ECX','RCX': o.contextitem:=2;
      'EDX','RDX': o.contextitem:=3;
      'ESI','RSI': o.contextitem:=4;
      'EDI','RDI': o.contextitem:=5;
      'ESP','RSP': o.contextitem:=6;
      'EBP','RBP': o.contextitem:=7;
      'R8': o.contextItem:=8;
      'R9': o.contextItem:=9;
      'R10': o.contextItem:=10;
      'R11': o.contextItem:=11;
      'R12': o.contextItem:=12;
      'R13': o.contextItem:=13;
      'R14': o.contextItem:=14;
      'R15': o.contextItem:=15;

      'EAXF','RAXF': o.contextitem:=16;
      'EBXF','RBXF': o.contextitem:=17;
      'ECXF','RCXF': o.contextitem:=18;
      'EDXF','RDXF': o.contextitem:=19;
      'ESIF','RSIF': o.contextitem:=20;
      'EDIF','RDIF': o.contextitem:=21;
      'EBPF','RBPF': o.contextitem:=22;
      'ESPF','RSPF': o.contextitem:=23;
      'R8F': o.contextItem:=24;
      'R9F': o.contextItem:=25;
      'R10F': o.contextItem:=26;
      'R11F': o.contextItem:=27;
      'R12F': o.contextItem:=28;
      'R13F': o.contextItem:=29;
      'R14F': o.contextItem:=30;
      'R15F': o.contextItem:=31;
      else
      begin

        if (length(regname)>=4) and regname.StartsWith('XMM') then
        begin
          if regname.Contains('.')=false then
          begin
            //xmm bytetable
            if regname[4]='-' then
              xmmnr:=strtoint(regname.Substring(4)) //XMM-x
            else
              xmmnr:=strtoint(regname.Substring(3)); //except on invalid data. That's ok
            o.contextItem:=32+xmmnr;
          end
          else
          begin
            r3:=regname.Split('.');
            if length(r3)<>2 then raise exception.create('Invalid xmm register format (Invalid dot usage)');

            if regname[4]='-' then
              xmmnr:=strtoint(r3[0].Substring(4)) //XMM-x.yyyyyyz
            else
              xmmnr:=strtoint(r3[0].Substring(3));

            if (length(r3[1])>2) or (length(r3[1])=0) then raise exception.create('Invalid xmm register format');

            if (length(r3[1])=2) and (not (uppercase(r3[1])[2] in ['D','F'])) then exception.create('Invalid xmm register format (Not F or D)');

            subnr:=strtoint(r3[1][1]);

            if (length(r3[1])=2) and (uppercase(r3[1][2])='D') then
              o.contextItem:=112+xmmnr*2+subnr //XMM*.*D (double)
            else
              o.contextItem:=48+xmmnr*4+subnr; //XMM*.*F or //XMM*.*
          end;
        end;
      end;

    end;

    setlength(output,length(output)+1);
    output[length(output)-1]:=o;
  end;
end;

function AddSafeCallStub(script: TStrings; functionname: string; targetself: boolean):string; //a function that can be called from any stack alignment
begin
  if functionname[1]='[' then
    result:='ceinternal_autofree_safecallstub_for_'+copy(functionname,2,length(functionname)-2)
  else
    result:='ceinternal_autofree_safecallstub_for_'+functionname;

  script.add('');
  script.insert(0,'alloc('+result+',512)'); //Let's place bets how many people are going to remark that this is what breaks their code and not because they didn't allocate enough memory properly...


  script.add(result+':');
  if processhandler.is64Bit{$ifdef cpu64} or targetself{$endif} then
  begin
    script.add('pushfq //save flags');
    script.add('push rax');
    script.add('mov rax,rsp');
    script.add('and rsp,fffffffffffffff0   //align stack');

    script.add('sub rsp,2a0 //allocate local space for scratchspace, the registers, and sse registers. And keep alignment');

    script.add('//store state');
    script.add('fxsave qword [rsp+20]');
    script.add('mov [rsp+220],rbx');
    script.add('mov [rsp+228],rcx');
    script.add('mov [rsp+230],rdx');
    script.add('mov [rsp+238],rsi');
    script.add('mov [rsp+240],rdi');
    script.add('mov [rsp+248],rax //rsp');
    script.add