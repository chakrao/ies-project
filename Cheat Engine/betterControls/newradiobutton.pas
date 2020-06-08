unit newRadioButton;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, StdCtrls, LCLType, Graphics, LMessages, Controls;

type
  TNewRadioButton=class(StdCtrls.TRadioButton)
  private
    painted: boolean;
    fCanvas: TCanvas;
    fCustomDraw: boolean;
    fOnPaint: TNotifyEvent;

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
    property Canvas: TCanvas read fCanvas; }
  end;


implementation

uses forms, win32proc, betterControls;

procedure TNewRadioButton.GetPreferredSize(var PreferredWidth, PreferredHeight: integer; Raw: boolean=false; WithThemeSpace: boolean=true);
var r: trect;
  x: integer;
  dpiscale: single;
  w,h: integer;
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);

  if ShouldAppsUseDarkMode and (font<>nil) then
  begin
    dpiscale:=Screen.PixelsPerInch/96;
    fcanvas.font.size:=font.size;
    r:=rect(trunc(dpiscale)-1,trunc(3*dpiscale),(trunc(dpiscale)-1)*2+PreferredHeight-trunc((3*dpiscale)*2),(trunc(dpiscale)-1)+PreferredHeight-trunc((3*dpiscale)));
    x:=r.right+trunc(3*dpiscale