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
    script.add('mov [rsp+250],rbp');
    script.add('mov [rsp+258],r8');
    script.add('mov [rsp+260],r9');
    script.add('mov [rsp+268],r10');
    script.add('mov [rsp+270],r11');
    script.add('mov [rsp+278],r12');
    script.add('mov [rsp+280],r13');
    script.add('mov [rsp+288],r14');
    script.add('mov [rsp+290],r15');

    script.add('//[rsp+248]+0=original rax');
    script.add('//[rsp+248]+8=original flags');

    script.add('//call lua function');
    script.add('lea rcx,[rsp+20]  //pointer to the saved state  ([rcx+248-20]+0=rax   [rcx+248-20]+0=flags)');
    script.add('call '+functionname);

    script.add('//restore registers (could have been changed by the function on purpose)');
    script.add('mov r15,[rsp+290]');
    script.add('mov r14,[rsp+288]');
    script.add('mov r13,[rsp+280]');
    script.add('mov r12,[rsp+278]');
    script.add('mov r11,[rsp+270]');
    script.add('mov r10,[rsp+268]');
    script.add('mov r9,[rsp+260]');
    script.add('mov r8,[rsp+258]');
    script.add('mov rbp,[rsp+250]');
    script.add('mov rdi,[rsp+240]');
    script.add('mov rsi,[rsp+238]');
    script.add('mov rdx,[rsp+230]');
    script.add('mov rcx,[rsp+228]');
    script.add('mov rbx,[rsp+220]');

    script.add('fxrstor qword [rsp+20]');

    script.add('mov rsp,[rsp+248] //restore rsp');
    script.add('pop rax');
    script.add('popfq');
    script.add('ret');

  end
  else
  begin
    script.add('pushfd //save flags');
    script.add('push eax');
    script.add('mov eax,esp');
    script.add('and esp,fffffff0   //align stack');
    script.add('sub esp,220 //allocate local space for scratchspace, the registers, and sse registers. And keep alignment');

    script.add('//store state');
    script.add('fxsave [esp]');
    script.add('mov [esp+200],ebx');
    script.add('mov [esp+204],ecx');
    script.add('mov [esp+208],edx');
    script.add('mov [esp+20c],esi');
    script.add('mov [esp+210],edi');
    script.add('mov [esp+214],eax //rsp');
    script.add('mov [esp+218],ebp');

    script.add('//[esp+214]+0=original eax');
    script.add('//[esp+214]+4=original eflags');

    script.add('//call lua function');
    script.add('mov eax,esp'); //just to be safe
    script.add('push eax');
    script.add('call '+functionname);
    script.add('add esp,4');

    script.add('//restore registers (could have been changed by the function on purpose)');
    script.add('mov ebp,[esp+218]');
    script.add('mov edi,[rsp+210]');
    script.add('mov esi,[rsp+20c]');
    script.add('mov edx,[rsp+208]');
    script.add('mov ecx,[rsp+204]');
    script.add('mov ebx,[rsp+200]');

    script.add('fxrstor [rsp]');

    script.add('mov esp,[rsp+214] //restore rsp');
    script.add('pop eax');
    script.add('popfd');
    script.add('ret');
  end;
end;



procedure AutoAssemblerCCodePass2(var dataForPass2: TAutoAssemblerCodePass2Data; symbollist: TSymbolListHandler);
//right after the allocs have been done
var
  secondarylist,errorlog: tstringlist;
  i,j,k: integer;
  bytes: tmemorystream;

  jmpbytes, nopfiller: array of byte;

  a, newAddress: ptruint;
  syminfo, nextsyminfo: PCESymbolInfo;
  bw: size_t;
  psize: integer;

  phandle: THandle;

  _tcc: TTCC;
  s,s2: string;

  tempsymbollist: TStringlist;

  oldprotection: dword;
  tccregions: TTCCRegionList;

  writesuccess: boolean;
  writesuccess2: boolean;

begin
  secondarylist:=TStringList.create;
  bytes:=tmemorystream.create;

  tempsymbollist:=tstringlist.create;

  errorlog:=tstringlist.create;
  tccregions:=TTCCRegionList.Create;

  dataForPass2.cdata.address:=align(dataForPass2.cdata.address,16);

  try
    if dataForPass2.cdata.targetself then
    begin
      _tcc:=tccself;
      phandle:=GetCurrentProcess;
      {$ifdef cpu64}
      psize:=8;
      {$else}
      psize:=4;
      {$endif}
    end
    else
    begin
      phandle:=processhandle;
{$ifdef windows}
      if getConnection<>nil then
        _tcc:=tcc_linux
      else
{$endif}
       _tcc:=tcc;

      psize:=processhandler.pointersize;
    end;


    for i:=0 to length(dataForPass2.cdata.references)-1 do
      secondarylist.AddObject(dataForPass2.cdata.references[i].name, tobject(dataForPass2.cdata.references[i].address));


    if dataForPass2.cdata.nodebug=false then
      dataForPass2.cdata.sourceCodeInfo:=TSourceCodeInfo.create;




    if _tcc.compileScript(dataForPass2.cdata.cscript.Text, dataForPass2.cdata.address, bytes, tempsymbollist, tccregions, dataForPass2.cdata.sourceCodeInfo, errorlog, secondarylist, dataForPass2.cdata.targetself ) then
    begin
      if bytes.Size>dataForPass2.cdata.bytesize then
      begin
        tccregions.Clear;

        //this will be a slight memoryleak but whatever
        //allocate 4x the amount of memory needed
{$ifdef windows}
        if dataForPass2.cdata.kernelAlloc then
          newAddress:=ptruint(KernelAlloc(4*bytes.size))
        else
{$endif}
        begin
          if SystemSupportsWritableExecutableMemory then
            newAddress:=ptruint(VirtualAllocEx(phandle,nil,4*bytes.size,mem_reserve or mem_commit, PAGE_EXECUTE_READWRITE))
          else
            newAddress:=ptruint(VirtualAllocEx(phandle,nil,bytes.size,mem_reserve or mem_commit, PAGE_READWRITE)); //these systems already waste memory as is so no *4

        end;

        if newAddress<>0 then
        begin
          dataForPass2.cdata.address:=newAddress;
          dataForPass2.cdata.bytesize:=4*bytes.size;

          //try again
          tempsymbollist.clear;
          bytes.clear;
          secondarylist.Clear;
          errorlog.clear;
          if _tcc.compileScript(dataForPass2.cdata.cscript.text, dataForPass2.cdata.address, bytes, tempsymbollist, tccregions, dataForPass2.cdata.sourceCodeInfo, errorlog, secondarylist, dataForPass2.cdata.targetself )=false then
          begin
            //wtf? something really screwed up here
{$ifdef windows}
            if dataForPass2.cdata.kernelAlloc then
              KernelFree(newAddress)
            else
{$endif}
              VirtualFreeEx(phandle, pointer(newAddress), 0,MEM_FREE);
            raise exception.create('3rd time failure of c-code');
          end;


          if bytes.Size>dataForPass2.cdata.bytesize then
          begin
{$ifdef windows}
            if dataForPass2.cdata.kernelAlloc then
              KernelFree(newAddress)
            else
{$endif}
              VirtualFreeEx(phandle, pointer(newAddress), 0,MEM_FREE);

            raise exception.create('(Unexplained and unmitigated code growth)');
          end;
        end
        else
          raise exception.create('Failure allocating memory for the C-code');
      end;

      //still here so compilation is within the given parameters


      if not SystemSupportsWritableExecutableMemory then //could have been made execute readonly earlier, undo that here
        virtualprotectex(processhandle,pointer(dataforpass2.cdata.address),bytes.size,PAGE_READWRITE,oldprotection);



      OutputDebugString('Writing c-code to '+dataforpass2.cdata.address.ToHexString+' ( '+bytes.size.ToString+' bytes )');
      writesuccess:=writeProcessMemory(phandle, pointer(dataforpass2.cdata.address),bytes.memory, bytes.size,bw);

      {$ifdef darwin}
      if (writesuccess) then
      begin
        outputdebugstring('success. Wrote:');
        s:='';
        for i:=0 to bytes.size-1 do
        begin
          s:=s+pbyte(bytes.memory)[i].ToHexString(2)+' ';
        end;

        outputdebugstring(s);
      end;
      {$endif}


      //fill in links
      for i:=0 to length(dataForPass2.cdata.linklist)-1 do
      begin
        j:=secondarylist.IndexOf(dataforpass2.cdata.linklist[i].name);
        if j=-1 then
        begin
          raise exception.create('Failure to link '+dataForPass2.cdata.linklist[i].fromname+' due to missing reference');
        end;

        k:=tempsymbollist.IndexOf(dataForPass2.cdata.linklist[i].fromname);
        if k=-1 then
        begin
          raise exception.create('Failure to link '+dataForPass2.cdata.linklist[i].fromname+' due to it missing in the c-code');
        end;

        a:=ptruint(tempsymbollist.Objects[k]);
        writesuccess2:=writeProcessMemory(phandle, pointer(dataForPass2.cdata.references[j].address),@a,psize,bw);
      end;


      for i:=0 to length(dataForPass2.cdata.symbols)-1 do
      begin
        j:=tempsymbollist.IndexOf(dataForPass2.cdata.symbols[i].name);
        if j<>-1 then
          dataforpass2.cdata.symbols[i].address:=ptruint(tempsymbollist.Objects[j])
        else
        begin
          //not found. prefixed ?
          if dataForPass2.cdata.symbolPrefix<>'' then
          begin
            s:=dataForPass2.cdata.symbols[i].name;
            if s.StartsWith(dataForPass2.cdata.symbolPrefix+'.') then
            begin
              s:=copy(s,length(dataForPass2.cdata.symbolPrefix)+2);
              j:=tempsymbollist.IndexOf(s);
              if j<>-1 then
                dataforpass2.cdata.symbols[i].address:=ptruint(tempsymbollist.Objects[j])
            end;
          end;
        end;
      end;


      if symbollist<>nil then //caller wants the list
      begin
        for i:=0 to tempsymbollist.count-1 do
        begin
          s:=tempsymbollist[i];
          if s.StartsWith('ceinternal_autofree_cfunction') then continue; //strip the ceinternal_autofree_cfunction

          if dataForPass2.cdata.symbolPrefix<>'' then
          begin
            symbollist.AddSymbol('',dataForPass2.cdata.symbolPrefix+'.'+tempsymbollist[i], ptruint(tempsymbollist.Objects[i]), 1);
            symbollist.AddSymbol('',tempsymbollist[i], ptruint(tempsymbollist.Objects[i]), 1,true);
          end
          else
            symbollist.AddSymbol('',tempsymbollist[i], ptruint(tempsymbollist.Objects[i]), 1);
        end;
      end;

      if not SystemSupportsWritableExecutableMemory then
      begin
        //apply protections
        for i:=0 to tccregions.Count-1 do
          virtualprotectex(processhandle, pointer(tccregions[i].address), tccregions[i].size, tccregions[i].protection,oldprotection);
      end;

      if not writesuccess then
        raise exception.create('Failure writing the generated c-code to memory');


      if not writesuccess2 then
        raise exception.create('Failure writing referenced addresses');
    end
    else
    begin
      raise Exception.Create('ccode section compilation failed:'+errorlog.text);
    end;


  finally
    freeandnil(errorlog);

    freeandnil(bytes);
    freeandnil(secondarylist);

    freeandnil(dataForPass2.cdata.cscript);

    freeandnil(tempsymbollist);
    freeandnil(tccregions);
  end;
end;


procedure ParseCBlockSpecificParameters(s: string; var dataForPass2: TAutoAssemblerCodePass2Data);
var
  i,j: integer;
  params: array of string;
  us: string;

begin
  params:=s.Split([' ',',']);
  for i:=0 to length(params)-1 do
  begin
    us:=uppercase(params[i]);
{$ifdef windows}
    if (us='KALLOC') or (us='KERNELMODE') or (us='KERNEL') then
      DataForPass2.cdata.kernelAlloc:=true;
{$endif}

    if (us='NODEBUG') then
      DataForPass2.cdata.nodebug:=true;

    if copy(us,1,7)='PREFIX=' then
      DataForPass2.cdata.symbolPrefix:=copy(params[i],8);
  end;

end;

procedure AutoAssemblerCBlockPass1(Script: TStrings; var i: integer; var dataForPass2: TAutoAssemblerCodePass2Data);
//just copy the lines from script to cscript
var
  s: string;
begin
  if dataForPass2.cdata.cscript=nil then
    dataForPass2.cdata.cscript:=tstringlist.create;

  dataForPass2.cdata.cscript.add('//{$C} block at line '+ptruint(script.Objects[i]).ToString);;

  s:=trim(script[i]);
  s:=copy(s,5,length(s)-5);
  ParseCBlockSpecificParameters(s, dataForPass2);


  script.delete(i); //remove the {$C} line
  while i<script.Count-1 do
  begin
    if uppercase(script[i])='{$ASM}' then
    begin
      //finished
      script.delete(i);
      dataForPass2.cdata.cscript.add(''); //just a seperator to make debugging easier
      exit;
    end;

    dataForPass2.cdata.cscript.AddObject(script[i], script.Objects[i]);
    script.Delete(i);
  end;
end;

procedure AutoAssemblerCCodePass1(script: TStrings; parameters: TLuaCodeParams; var i: integer; var dataForPass2: TAutoAssemblerCodePass2Data; syntaxcheckonly: boolean; targetself: boolean);
var
  j,k: integer;
  tst: ptruint;
  s: string;
  endpos, scriptstart, scriptend: integer;
  scriptstartlinenr: integer;

  stubcounter: integer=0;

  cscript, imports, errorlog: tstringlist;
  functionname: string;

  refnr: integer;


  usesXMMType: boolean=false;

  ms: TMemorystream;
  bytesizeneeded: integer;

begin

  s:=trim(script[i]);
  s:=copy(s,9,length(s)-9);
  ParseCBlockSpecificParameters(s, dataForPass2);

  if dataForPass2.cdata.cscript=nil then
    dataForPass2.cdata.cscript:=tstringlist.create;



  scriptstartlinenr:=ptruint(script.Objects[i]);

  scriptstart:=i;
  j:=i+1;
  while j<script.Count-1 do
  begin
    if uppercase(script[j])='{$ASM}' then
      break;

    inc(j);
  end;
  scriptend:=j;



  cscript:=dataForPass2.cdata.cscript; //shortens the code
  functionname:='ceinternal_autofree_cfunction_at_line'+inttostr(ptruint(script.objects[scriptstart]));
  cscript.add('void '+functionname+'(void *parameters)');
  cscript.add('{');
  cscript.add('  //get the values from the parameter pointer');
  //load the values from parameters pointer

  if processhandler.is64Bit {$ifdef cpu64}or targetself{$endif} then
  begin
    for j:=0 to length(parameters)-1 do
    begin
      case parameters[j].contextitem of
        0: s:='unsigned long long '+parameters[j].varname+'=*(unsigned long long *)*(unsigned long long *)((unsigned long long)parameters+0x228);'; //RAX
        1..5, 7..15: s:='unsigned long long '+parameters[j].varname+'=*(unsigned long long*)((unsigned long long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+');'; //RBX..R15
        6: s:='unsigned long long '+parameters[j].varname+'=*(unsigned long long*)((unsigned long long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+')+24;'; //RSP
        16: s:='float '+p