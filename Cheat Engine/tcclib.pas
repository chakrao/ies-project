unit tcclib;

{$mode objfpc}{$H+}


interface

uses
  {$ifdef windows}windows,{$endif}
  {$ifdef darwin}macport, dl,macportdefines, {$endif}
  Classes, SysUtils, syncobjs, maps, math, Generics.Collections;


type
  TTCCTarget=(x86_64,i386{$ifdef windows}, x86_64_sysv, i386_sysv{$endif} {$ifdef darwin},aarch64{$endif});
  PTCCState=pointer;
  {$ifdef standalonetest}
  TSymbolListHandler=pointer;
  {$endif}

  TTCCRegionInfo=record
     address: ptruint;
     size: integer;
     protection: dword;
  end;

  TTCCRegionList=specialize tlist<TTCCRegionInfo>;



  TTCCMemorystream=class(TMemoryStream)
  private
    protections: array of TTCCRegionInfo;

    base: ptruint; //this unit owns the TTCCMemorystream so it can access base no problem, the caller units not so much
  protected
    function Realloc(var NewCapacity: PtrInt): Pointer; override;
  public
  end;


  TCCStabEntry=packed record
    {
         unsigned int n_strx;         /* index into string table of name */
         unsigned char n_type;         /* type of symbol */
         unsigned char n_other;        /* misc info (usually empty) */
         unsigned short n_desc;        /* description field */
         unsigned int n_value;        /* value of symbol */
     }
    n_strx: integer;
    n_type: byte;
    n_other: byte;
    n_desc: word;
    n_value: dword;
  end;
  PCCStabEntry=^TCCStabEntry;


  TLineNumberInfo=record
    address: ptruint;
    functionaddress: ptruint;
    linenr: integer;
    sourcecode: pchar; //just this line
    sourcefile: tstrings; //the sourcecode this line belongs to   (contains address as well)
  end;
  PLineNumberInfo=^TLineNumberInfo;

  TLocalVariableInfo=record
    name: string;
    offset: integer;//stackframe offset
    vartype: integer;
    ispointer: boolean;
  end;

  TSourceCodeInfo=class(TObject)
  private
    fprocessid: dword;
    AddressToLineNumberInfo: tmap;
    sources: TStringlist;
    minaddress: ptruint;
    maxaddress: ptruint;

    stabdata: pointer; //for later whan parsing types and local vars
    stabsize: integer;
    stabstrsize: integer;

    stab: PCCStabEntry;
    stabstr: pchar;

    fullyParsed: boolean;

    parsedsource: array of record
      sourcefile: string;
      minaddress: ptruint;
      maxaddress: ptruint;

      functions: array of record
        functionname: string;
        functionaddress: ptruint;
        functionstop: ptruint;


        //the following is only filled in on a full parse
        lexblocks: array of record
          startaddress: ptruint;
          stopaddress: ptruint;
          level: integer;
        end;

        stackvars: array of record
          varstr: string;
          varname: string;
          offset: integer;
          lexblock: integer;
          ispointer: boolean;

          typenr: integer;
        end;

        parameters: array of record
          varstr: string;
          varname: string;
          offset: integer;

          typenr: integer;
          ispointer: boolean;
        end;
      end;
    end;


    procedure AddSource(sourcefilename: string; sourcecode: tstrings); //sourcecode string objects passed become owned by TSourceCodeInfo and will be destroyed when it gets destroyed
    function GetSource(sourcefilename: string): tstrings;


    procedure parseLineNumbers(symbols: tstrings; stringsources: tstrings);
    procedure addLineInfo(functionaddress, address: ptruint; linenr: integer; sourceline: string; sourcefile: tstrings);
  public
    procedure outputDebugInfo(o: tstrings);
    procedure parseFullStabData;

    function getLineInfo(address: ptruint): PLineNumberInfo;
    function getVariableInfo(varname: string; currentaddress: ptruint; out varinfo: TLocalVariableInfo): boolean;

    procedure getRange(out start: ptruint; out stop: ptruint);

    procedure register;
    procedure unregister;



    property processID: dword read fProcessID;

    constructor create;
    destructor destroy; override;
  end;

  TOpenFileCallback=function(filename: pchar; openflag: integer): integer; stdcall;
  TReadFileCallback=function(fileHandle: integer; destination: pointer; maxcharcount: integer):integer;  stdcall;
  TCloseFileCallback=function(fileHandle: integer): integer;  stdcall;

  TTCC=class(TObject)
  private
    cs: TCriticalSection; static;
    working: boolean;

    new: function():PTCCState; cdecl;
    parse_args: function(s:PTCCState; pargc: pinteger; pargv: pchar; optind: integer):integer; cdecl;//  //(TCCState *s, int *pargc, char ***pargv, int optind)
    set_options: procedure(s: PTCCState; str: pchar); cdecl;
    set_lib_path: procedure(s: PTCCState; path: pchar); cdecl;
    add_include_path: procedure(s: PTCCState; path: pchar); cdecl;
    set_error_func:procedure(s: PTCCState; error_opaque: pointer; functionToCall: pointer); cdecl;
    set_symbol_lookup_func:procedure(s:PTCCState; userdata: pointer; functionToCall: pointer); cdecl;
    set_binary_writer_func:procedure(s:PTCCState; userdata: pointer; functionToCall: pointer); cdecl;

    set_output_type:function(s: PTCCState; output_type: integer): integer; cdecl;
    compile_string:function(s: PTCCState; buf: pchar): integer; cdecl;


    get_symbol:function(s: PTCCState; name: pchar):pointer; cdecl;
    get_symbols:function(s: PTCCState; userdata: pointer; functionToCall: pointer):pointer; cdecl;

    get_stab: function(s: PTCCState; output: pointer; out outputlength: integer): integer; cdecl;

    delete:procedure(s: PTCCState); cdecl;
    add_file:function(s: PTCCState; filename: pchar): integer; cdecl;
    output_file:function(s: PTCCState; filename: pchar): integer; cdecl;

    relocate:function(s: PTCCState; address: ptruint): integer; cdecl; //address=0 gets size, address=1 let's tcc decide (nope) address>1 write there using the binary writer


    add_symbol:function(s: PTCCState; name: pchar; val: pointer): integer; cdecl;
    install_filehook: procedure (OpenFileCallBack: TOpenFileCallback; ReadFileCallback: TReadFileCallback; CloseFileCallback: TCloseFileCallback); cdecl;

    procedure setupCompileEnvironment(s: PTCCState; textlog: tstrings; targetself: boolean=false; nodebug: boolean=false);
    procedure parseStabData(s: PTCCState; symbols: Tstrings; sourcecodeinfo: TSourceCodeInfo; stringsources: tstrings=nil);
  public
    function testcompileScript(script: string; var bytesize: integer; referencedSymbols: TStrings; symbols: TStrings; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil): boolean;
    function compileScript(script: string; address: ptruint; output: tstream; symbollist: TStrings; regionList: TTCCRegionList=nil; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; secondaryLookupList: tstrings=nil; targetself: boolean=false): boolean;
    function compileScripts(scripts: tstrings; address: ptruint; output: tstream; symbollist: TStrings; regionList: TTCCRegionList=nil; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false): boolean;
    function compileProject(files: tstrings; address: ptruint; output: tstream; symbollist: TStrings; regionList: TTCCRegionList=nil; sourcecodeinfo: TSourceCodeInfo=nil; textlog: tstrings=nil; targetself: boolean=false): boolean;
    constructor create(target: TTCCTarget);
  end;


  function tcc: TTCC;
{$ifdef windows}
  function tcc_linux: TTCC;
{$endif}
  function tccself: TTCC;



  procedure tcc_addCIncludePath(path: string);
  procedure tcc_removeCIncludePath(path: string);

  {$ifdef darwin}  //test
  var
    tccrosetta: TTCC; //for compiling in c code
  {$endif}


implementation

uses forms,dialogs, StrUtils, Contnrs {$ifndef standalonetest}, symbolhandler, ProcessHandlerUnit,
  newkernelhandler, CEFuncProc, sourcecodehandler, MainUnit, globals{$endif};
const
  TCC_RELOCATE_AUTO=pointer(1); //relocate
  TCC_OUTPUT_MEMORY  = 1; { output will be run in memory (default) }
  TCC_OUTPUT_EXE     = 2; { executable file }
  TCC_OUTPUT_DLL     = 3; { dynamic library }
  TCC_OUTPUT_OBJ     = 4; { object file }
  TCC_OUTPUT_PREPROCESS = 5; { only preprocess (used internally) }


var
  initDone: boolean;
  tcc32: TTCC;
  {$ifdef cpu64}
  tcc64: TTCC;
  {$endif}


  {$ifdef windows}
  tcc32_linux: TTCC;
  {$ifdef cpu64}
  tcc64_linux: TTCC;
  {$endif} //cpu64
  {$endif} //windows

  additonalIncludePaths: tstringlist;

procedure UpdateMinMax(address: ptruint; var minaddress: ptruint; var maxaddress: ptruint);
begin
  if minaddress=0 then
    minaddress:=address
  else
    if address<minaddress then
      minaddress:=add