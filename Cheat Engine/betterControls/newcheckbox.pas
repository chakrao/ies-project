unit newCheckbox;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, StdCtrls, LCLType, Graphics, LMessages,
  Controls, BetterControlColorSet;

type
  TNewCheckBox=class(StdCtrls.TCheckBox)
  private
    painted: boolean;
    fCanvas: TCanvas;
    fCustomDraw: boolean;
    fOnPaint: TNotifyEvent;
    autosizewidth: integer;
    procedure pp(var msg: TMessage); message WM_NOTIFY;

  protected
    procedure DefaultCustomPaint;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean=false; WithThemeSpace: boolean=true); override;
  published
   { property CustomDraw: boolean read fCustomDraw write fCustomDraw;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    property Canvas: TCanvas read fCanvas;  }
  end;


implementation

uses forms, win32proc, betterControls;

procedure TNewCheckBox.GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean=false; WithThemeSpace: boolean=true);
var r: trect;
  x: integer;
  dpiscale: single;
  w,h: integer;
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);

  if ShouldAppsUseDarkMode and (font<>nil) then
  begin
    if fcanvas=nil then
    begin
      fcanvas:=TControlCanvas.Create;
      TControlCanvas(FCanvas).Control := Self;

      //beep;
    end;


    dpiscale:=Screen.PixelsPerInch/96;
    fcanvas.font:=font; //.size:=font.size;
    r:=rect(trunc(dpiscale)-1,trunc(3*dpiscale),(trunc(dpiscale)-1)*2+PreferredHeight-trunc((3*dpiscale)*2),(trunc(dpiscale)-1)+PreferredHeight-trunc((3*dpiscale)));
    x:=r.right+trunc(3*dpiscale);

//    x:=x+fcanvas.TextWidth(caption);
    MeasureTextForWnd(Handle, Text, w,h);
    x:=x+w;

    PreferredWidth:=x+4;
  end;

end;

procedure TNewCheckBox.pp(var msg: TMessage);
var
  p1: LPNMHDR;
begin
  p1:=LPNMHDR(msg.lparam);
  if p1^.code=UINT(NM_CUSTOMDRAW) then
  begin
    asm
    nop
    end;


  end;
end;

procedure TNewCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  painted:=false;

  inherited MouseMove(shift, x,y);
  if ShouldAppsUseDarkMode then
  begin
    if not painted then
      repaint;
  end;
end;

procedure TNewCheckBox.FontChanged(Sender: TObject);
begin
  if ShouldAppsUseDarkMode then
  begin

    if self=nil then exit;

    if fcanvas<>nil then
    begin
      fCanvas.Font.BeginUpdate;
      try
        fCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
        fCanvas.Font := Font;
      finally
        fCanvas.Font.EndUpdate;
      end;
    end;

  end;
  inherited FontChanged(Sender);
en