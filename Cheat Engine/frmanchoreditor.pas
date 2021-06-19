
unit frmAnchorEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Spin, DPIHelper, propedits, betterControls;

type

  { TAnchorEditor }

  TAnchorEditor = class(TForm)
    cbBottomAnchorEnabled: TCheckBox;
    cbBottomSideControl: TComboBox;
    cbLeftAnchorEnabled: TCheckBox;
    cbLeftSideControl: TComboBox;
    cbRightAnchorEnabled: TCheckBox;
    cbRightSideControl: TComboBox;
    cbTopAnchorEnabled: TCheckBox;
    cbTopSideControl: TComboBox;
    gbRight: TGroupBox;
    gbTop: TGroupBox;
    gbBottom: TGroupBox;
    gbLeft: TGroupBox;
    gbBorderSpace: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    seTopBorderspace: TSpinEdit;
    seBottomBorderspace: TSpinEdit;
    seRightBorderspace: TSpinEdit;
    seBorderspace: TSpinEdit;
    sbLeftLeft: TSpeedButton;
    sbLeftCenter: TSpeedButton;
    sbLeftRight: TSpeedButton;
    sbRightLeft: TSpeedButton;
    sbRightCenter: TSpeedButton;
    sbRightRight: TSpeedButton;
    sbTopTop: TSpeedButton;
    sbTopCenter: TSpeedButton;
    sbTopBottom: TSpeedButton;
    seLeftBorderspace: TSpinEdit;
    sbBottomTop: TSpeedButton;
    sbBottomCenter: TSpeedButton;
    sbBottomBottom: TSpeedButton;
    procedure cbAnchorEnabledChange(Sender: TObject);
    procedure cbControlSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbAnchorSideClick(Sender: TObject);
    procedure seBorderspaceChange(Sender: TObject);
  private
    currentSelection: TControl;
    siblings: TStringList;
    updatecount: integer;
    procedure setAnchorEditorState(state: boolean);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
  public
    procedure updateControlStatus;
    procedure setSelection(ASelection: TPersistentSelectionList);
    procedure OnObjectPropertyChanged(Sender: TObject);
  end;

var
  AnchorEditor: TAnchorEditor;


implementation

{ TAnchorEditor }

uses math;



procedure TAnchorEditor.setAnchorEditorState(state: boolean);
  procedure setControlState(c: TWinControl; state: boolean);
  var i: integer;
  begin
    for i:=0 to c.ControlCount-1 do
    begin
      if c.Controls[i] is TWinControl then
        setControlState(TWinControl(c.Controls[i]),state);

      c.Controls[i].enabled:=state;
    end;

    if c<>self then
      c.enabled:=state;
  end;
begin
  BeginFormUpdate;
  setControlState(self, state);
  EndFormUpdate;
end;

procedure TAnchorEditor.updateControlStatus;
var
  i: integer;
  p: TWincontrol;
begin
  inc(updatecount);
  try
    p:=currentSelection.Parent;

    if siblings=nil then
      siblings:=TStringlist.create;

    siblings.Clear;
    siblings.AddObject('<nil>',nil);
    siblings.addObject(p.name, p);
    for i:=0 to p.ControlCount-1 do
    begin
      if (p.controls[i]<>currentSelection) and (p.controls[i].Name<>'') then
        siblings.AddObject(p.controls[i].Name,p.controls[i]);
    end;

    cbLeftSideControl.Items:=siblings;
    cbTopSideControl.Items:=siblings;
    cbRightSideControl.Items:=siblings;
    cbBottomSideControl.Items:=siblings;

    i:=min(8,siblings.count);
    cbLeftSideControl.DropDownCount:=i;
    cbTopSideControl.DropDownCount:=i;
    cbRightSideControl.DropDownCount:=i;
    cbBottomSideControl.DropDownCount:=i;

    case currentSelection.AnchorSideLeft.Side of
      asrLeft: sbLeftLeft.Down:=true;
      asrCenter: sbLeftCenter.Down:=true;
      asrRight: sbLeftRight.Down:=true;
    end;

    case currentSelection.AnchorSideTop.Side of
      asrTop: sbTopTop.Down:=true;
      asrCenter: sbTopCenter.Down:=true;
      asrBottom: sbTopBottom.Down:=true;
    end;

    case currentSelection.AnchorSideRight.Side of
      asrLeft: sbRightLeft.Down:=true;
      asrCenter: sbRightCenter.Down:=true;
      asrRight: sbRightRight.Down:=true;
    end;

    case currentSelection.AnchorSideBottom.Side of
      asrTop: sbBottomTop.Down:=true;
      asrCenter: sbBottomCenter.Down:=true;
      asrBottom: sbBottomBottom.Down:=true;
    end;

    if currentSelection.AnchorSideLeft.Control<>nil then
      cbLeftSideControl.Text:=currentSelection.AnchorSideLeft.Control.Name
    else
      cbLeftSideControl.Text:='';

    if currentSelection.AnchorSideTop.Control<>nil then
      cbTopSideControl.Text:=currentSelection.AnchorSideTop.Control.Name
    else
      cbTopSideControl.Text:='';

    if currentSelection.AnchorSideRight.Control<>nil then
      cbRightSideControl.Text:=currentSelection.AnchorSideRight.Control.Name
    else
      cbRightSideControl.Text:='';

    if currentSelection.AnchorSideBottom.Control<>nil then
      cbBottomSideControl.Text:=currentSelection.AnchorSideBottom.Control.Name
    else
      cbBottomSideControl.Text:='';

    cbLeftAnchorEnabled.checked:=akLeft in currentSelection.Anchors;
    cbTopAnchorEnabled.checked:=akTop in currentSelection.Anchors;
    cbRightAnchorEnabled.checked:=akRight in currentSelection.Anchors;
    cbBottomAnchorEnabled.checked:=akBottom in currentSelection.Anchors;


    seLeftBorderspace.value:=currentSelection.BorderSpacing.Left;
    seTopBorderspace.value:=currentSelection.BorderSpacing.Top;
    seRightBorderspace.value:=currentSelection.BorderSpacing.Right;
    seBottomBorderspace.value:=currentSelection.BorderSpacing.Bottom;
    seBorderspace.value:=currentSelection.BorderSpacing.Around;
  finally
    dec(updatecount);
  end;

end;

procedure TAnchorEditor.setSelection(ASelection: TPersistentSelectionList);
begin
  currentSelection:=nil;

  if ASelection.Count=1 then
  begin
    if ASelection.Items[0] is tcontrol then
      currentSelection:=tcontrol(ASelection.Items[0]);
  end;

  if (currentselection=nil) or (currentselection is TCustomForm) then
  begin
    currentSelection:=nil;
    setAnchorEditorState(false)
  end
  else
  begin
    setAnchorEditorState(true);

    updateControlStatus;


  end;
end;

procedure TAnchorEditor.OnObjectPropertyChanged(Sender: TObject);
var