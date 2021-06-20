unit frmDebugSymbolStructureListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, symbolhandlerstructs, betterControls;

type

  { TfrmDebugSymbolStructureList }

  TfrmDebugSymbolStructureList = class(TForm)
    btnSelect: TButton;
    btnSearch: TButton;
    edtSearch: TEdit;
    lblCount: TLabel;
    Label2: TLabel;
    lvStructlist: TListView;
    procedure btnSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvStructlistData(Sender: TObject; Item: TListItem);
    procedure lvStructlistDblClick(Sender: TObject);
  private
    foriginallist: tstringlist