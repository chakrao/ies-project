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



    property processID: dword re