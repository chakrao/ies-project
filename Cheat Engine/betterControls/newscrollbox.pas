unit newScrollBox;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, Forms;

type
  TNewScrollBox=class(TScrollBox)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewScrollBox.ChildHandlesCreated;
begin
  inherited ChildHan