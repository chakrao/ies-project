unit newColorBox;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, stdctrls, controls, messages, lmessages,
  Win32Extra, LCLClasses,LCLProc, colorbox;

type
  TNewColorBox=class(TColorBox)
  private
    creatingBrush: boolean;
  protected
    procedure CreateBrush; override;
  public
  end;


implementation

uses graphics, Menus, Win32WSMenus, betterControls, newComboBox;




procedure TNewColorBox.CreateBrush;
var
  cbi: TComboBoxINFO;
begin
  if ShouldA