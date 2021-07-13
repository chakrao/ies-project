// Copyright Cheat Engine. All Rights Reserved.

unit frmSourceDisplayUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, SynEdit, SynEditMarks, SynHighlighterCpp, disassembler,
  MemoryBrowserFormUnit, tcclib, betterControls, SynGutterBase, debugeventhandler,
  BreakpointTypeDef;

type

  { TfrmSourceDisplay }

  TfrmSourceDisplay = class(TForm)
    itInfo: TIdleTimer;
    ilDebug: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    N1: TMenuItem;
    PopupMenu1: TPopupMenu;
    seSource: TSynEdit;
    SynCppSyn1: TSynCppSyn;
    tbDebug: TToolBar;
    tbRun: TToolButton;
    tbRunTill: TToolButton;
    tbSeparator1: TToolButton;
    tbSeparator2: TToolButton;
    tbSeparator3: TToolButton;
    tbStepInto: TToolButton;
    tbStepOut: TToolButton;
    tbStepOver: TToolButton;
    tbToggleBreakpoint: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itInfoTimer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure seSourceGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure seSourceMouseEnter(Sender: TObject);
    procedure seSourceMouseLeave(Sender: TObject);
    procedure seSourceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tbRunClick(Sender: TObject);
    procedure tbRunTillClick(Sender: TObject);
    procedure tbStepIntoClick(Sender: TObject);
    procedure tbStepOutClick(Sender: TObject);
    procedure tbStepOverClick(Sender: TObject);
    procedure tbToggleBreakpointClick(Sender: TObject);
  private
    hintwindow:THintWindow;
    LoadedPosition: boolean;
    SourceCodeInfo: TSourceCodeInfo;

    d: TDisassembler;
    stepIntoThread: TDebugThreadHandler;
    stepIntoCountdown: integer; //counter to make sure it doesn't step too long (15 instructions max)

    function StepIntoHandler(sender: TDebugThreadHandler; bp: PBreakpoint): boolean;
  public
    function updateMarks:boolean; //returns true if the current breakpoint matches a line in this source
  end;

  function getSourceViewForm(lni: PLineNumberInfo): TfrmSourceDisplay;   //creates or shows an existing sourcedisplay form

  procedure ApplySourceCodeDebugUpdate; //called by memoryview when a breakpoint is triggered and causes a break

implementation

uses maps, debughelper, cedebugger, CEFuncProc, SynHighlighterAA,
  debuggertypedefinitions, sourcecodehandler, ProcessHandlerUnit, byteinterpreter,
  commonTypeDefs, NewKernelHandler, globals;

{ TfrmSourceDisplay }

var sourceDisplayForms: 