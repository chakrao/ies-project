unit frmDebuggerAttachTimeoutUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, syncobjs, betterControls;

type

  { TfrmDebuggerAttachTimeout }

  TfrmDebuggerAttachTimeout = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    lblStatus: TLabel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    event: TEvent;
  end;



implementation

uses DebuggerInterfaceAPIWrapper;

{ TfrmDebuggerAttachTimeout }

procedure TfrmDebuggerAttachTimeout.FormShow(Sender: TObject);
begin
  constraints.MinWidth:=canvas.TextWidth('xxx xxxxx xxxxxx xx xxxxxx x xxxxxx xxxx xx xxxxx xxxx xxxxx xxx xxxx');
end;

procedure TfrmDebuggerAttachTimeout.Timer1Timer(Sender: TObject);
var r: TWaitResult;
begin
  r:=event.WaitFor(1);

  if r=wrSignaled then
  begin
    modalresult:=mrok;
    exit;
  end
  else
  if r<>wrTimeout then
  begin
    modalresult:=mrAbort;
    exit;
  end;

  if CurrentDebuggerInterface<>nil then
    lblStatus.caption:=CurrentDebuggerInterface.debuggerAttachStatus;
end;

initialization
  {$I frmDebuggerAttachTimeoutUnit.lrs}

end.

