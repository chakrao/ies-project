unit libcepack;

{$mode ObjFPC}{$H+}

//library to pack/unpack files in the CE install distribution

interface

uses
  Classes, SysUtils;

procedure cepackfile(source, destination: stri