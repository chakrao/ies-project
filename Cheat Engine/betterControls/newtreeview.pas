unit newTreeView; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls,  ComCtrls, CommCtrl;

type
  TNewTreeView=class(TTreeView)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls, Graphics;

procedure TNewTreeView.C