unit cheatecoins;

{
Cheat-E coin handler and simple anti debug code

WARNING: If you read this you will lose all bragging rights on how you bypassed the cheat-e-coin system
You can of course say you didn't read this, but you'll know...

Now if you didn't read this to bypass the cheat-e-coin system but just want to see how it works, have fun

(May crash on some people's systems, due to security)
}

{$mode delphi}

interface

{$IFDEF windows}
uses
  jwawindows, windows, newkernelhandler, Classes, SysUtils, dialogs, betterControls;

procedure EnableCheatEcoinSystem;

procedure decreaseCheatECoinCount; stdcall;
function getCheatECoinCount: integer;

function checkCoinStatus: integer;
{$ENDIF}

implementation

{$IFDEF windows}
uses forms, frmMicrotransactionsUnit, ceregistry, luahandler, mainunit2;

var
  _DecreaseCount: integer;
  something: integer;
  _GetCount: integer;
  c2: pinteger;
  c3: pinteger; //last decrease value

  mainthreadhandle: THandle;
  context: PCONTEXT;
  contextmem: pointer;

threadvar
  actualcount: pinteger;

function antidebug1: integer; stdcall; //always returns 0xcececece
begin
  result:=GetTickCount64;
end;

function antidebug2(x: dword): dword; stdcall; //jumps to antidebug3 when executed (so returns x+0x100)
begin
  if x>5 then
    raise exception.create('It''s not that easy')
  else
    result:=10;
end;

function antidebug3(x: dword): dword; stdcall;
begin
  result:=x+$100;
end;

procedure setupContext;
begin
  context^.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  if GetThreadContext(mainthreadhandle,context^) then
  begin

    context.Dr0:=ptruint(@_DecreaseCount);
    context.dr1:=ptruint(@_GetCount);
    context.dr2:=ptruint(@antidebug1);
    context.dr3:=ptruint(@antidebug2);
    context.dr7:=$fd