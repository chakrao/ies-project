unit userbytedisassembler;
{
Lets the user input bytes and those will then get disassembled
}

{$mode ObjFPC}{$H+}

interface

uses
  {$ifdef darwin}
  macport,
  {$else}
  windows,
  {$endif}
  Classes, SysUtils, disassembler,math;

type
  TUserByteDisassembler=class(TDisassembler)
  private
    bytes: pointer;
    bytecount: integer;
  protected
    function readMemory(address: ptruint; destination: pointer; size: integer): integer; override;
  public
    procedure setBy