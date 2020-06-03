//Copyright Cheat Engine. All rights reserved
unit CECustomButton;  //more customizable button (not theme friendly)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, controls, ExtCtrls, betterControls;

type
  TCECustomButtonState=(sbsHighlighted, sbsDown, sbsHasFocus);
  TCECustomButtonStates=set of TCECustomButtonState;

  TCECustomButton=class(TCustomControl)
  private
    //state: T
    fAlignment : TAlignment;

    fScaled: boolean;
    fDrawFocusRect: boolean;
    fFocusElipseColor: TColor;

    hasSetRounding: boolean;
    froundingX: integer;
    froundingY: integer;
    fCustomDrawn: boolean;
    fgrowFont: boolean;
    fShowPrefix: boolean;

    fButtonColor: Tcolor;
    fButtonHighlightedColor: TColor;
    fButtonDownColor: TColor;
    fDrawBorder: boolean;
    fBorderColor: Tcolor;
    fbordersize: integer;
    fFocusedSize: integer;

    fbuttonAnimationSpeed: dword;

    btnstate: TCECustomButtonStates;

    autofontsize: integer;

    //animated
    timer: TTimer;
    //default button animation fields
    animationStart: qword;
    animationLength: integer; //in milliseconds
    animationStartColor: TColor;
    animationStopColor: TColor;
    lastanimationcolor: TColor;

    fFramesPerSecond: integer;

{    fimagelist: Tpicture;
    fCurrentFrame: integer;

    fOnLastFrame: TNotifyEvent;  }

    procedure setRoundingX(x: integer);
    procedure setRoundingY(y: integer);
    procedure setCustomDrawn(state: boolean);
    procedure setShowPrefix(state: boolean);
    procedure setGrowFont(state: boolean);
    procedure setDrawBorder(state: boolean);

    procedure setFramesPerSecond(fps: integer);

    procedure timertimer(sender: TObject);
    procedure setBorderColor(c: Tcolor);
    procedure setBorderSize(size: integer);
    procedure setFocusedSize(size: integer);
    procedure setButtonColor(c: TColor);
    procedure setDrawFocusRect(state: boolean);
  protected


    procedure ChildHandlesCreated; override;

    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    function getCaption: string; virtual;
    procedure setCaption(c: string); virtual;
  public
    procedure SetFocus; override;

    procedure startAnimatorTimer;   //starts the timer based on the current fps.  note: does not stop if CustomDrawnn is true
    procedure stopAnimatorTimer;
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
  published
    property ShowPrefix: boolean read fShowPrefix write setShowPrefix;
    property Alignment: TAlignment read fAlignment write fAlignment;
    property DrawBorder: Boolean read fDrawBorder write setDrawBorder;
    property BorderColor: TColor read fBorderColor write setBorderColor;
    property BorderSize: integer read fBorderSize write setBorderSize;
    property ButtonColor: TColor read fButtonColor write setButtonColor;
    property ButtonHighlightedColor: TColor read fButtonHighlightedColor write fButtonHighlightedColor;
    property ButtonDownColor: TColor read fButtonDownColor write fButtonDownColor;

    property RoundingX: integer read froundingX write setRoundingX;
    property RoundingY: integer read froundingY write setRoundingY;
    property CustomDrawn: boolean read fCustomDrawn write setCustomDrawn;
    property GrowFont: boolean read fGrowFont write setGrowFont;
    property FramesPerSecond: integer read fFramesPerSecond write setFramesPerSecond;
    property ButtonAnimationSpeed: dword read fbuttonAnimationSpeed write fbuttonAnimationSpeed;
    property DrawFocusRect: boolean read fDrawFocusRect write setDrawFocusRect;
    property FocusedSize: integer read fFocusedSize write setFocusedSize;
    property FocusElipseColor: tcolor read fFocusElipseColor write fFocusElipseColor;
    property Scaled: boolean read fScaled write fScaled default true;

    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    //todo: property Cancel;
    property Caption: string read getCaption write setCaption;
    property Color;
    property Constraints;
    //todo: property Default;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    //todo: property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;


  {
    property framelist: TList read fimagelist write fimagelist; //if set it's an animated button instead of a rounded button
    property currentFrame: integer read fCurrentFrame write fCurrentFrame;

    //0=no automated animation
    property onLastFrame: TNotifyEvent read fOnLastFrame write fOnLastFrame; //when the last frame of the imagelist has been drawn.  The user can then swap the imagelist, or leave it be , and/or change the currentframe back to 0

    }
  end;

implementation

uses forms;

procedure TCECustomButton.timertimer(sender: TObject);
begin
  Invalidate;
end;

procedure TCECustomButton.stopAnimatorTimer;
begin
  if timer<>nil then timer.enabled:=false;
end;

procedure TCECustomButton.startAnimatorTimer;
begin
  if fFramesPerSecond>0 then
  begin
    if timer=nil then
      timer:=TTimer.create(self);

    timer.OnTimer:=@timertimer;
    timer.interval:=1000 div fFramesPerSecond;
    timer.enabled:=true;
  end
  else
    if timer<>nil then freeandnil(timer);
end;

procedure TCECustomButton.setFramesPerSecond(fps: integer);
begin
  fFramesPerSecond:=fps;
  if (fFramesPerSecond<=0) and (timer<>nil) then
    freeandnil(timer);

  if timer<>nil then
    timer.interval:=1000 div fps
end;

procedure TCECustomButton.setRoundingX(x: integer);
begin
  if x>=0 then
  begin
    hasSetRounding:=true;
    froundingX:=x;

    if AutoSize then DoAutoSize;
    invalidate;
  end;
end;

procedure TCECustomButton.setRoundingY(y: integer);
begin
  if y>=0 then
  begin
    hasSetRounding:=true;
    froundingY:=y;

    if AutoSize then DoAutoSize;
    invalidate;
  end;
end;

procedure TCECustomButton.setBorderColor(c: tcolor);
begin
  fBorderColor:=c;
  invalidate;
end;

procedure TCECustomButton.setBorderSize(size: integer);
begin
  fBorderSize:=size;
  invalidate;
end;

procedure TCECustomButton.setFocusedSize(size: integer);
begin
  fFocusedSize:=size;
  invalidate;
end;

procedure TCECustomButton.setButtonColor(c: tcolor);
begin
  fButtonColor:=c;
  Invalidate;
end;

procedure TCECustomButton.setCustomDrawn(state: boolean);
begin
  fCustomDrawn:=state;
  invalidate;
end;

procedure TCECustomButton.setGrowFont(state: boolean);
begin
  fgrowFont:=state;
  invalidate;
end;

procedure TCECustomButton.setDrawBorder(state: boolean);
begin
  fDrawBorder:=state;
  invalidate;;
end;

procedure TCECustomButton.setShowPrefix(state: boolean);
begin
  fShowPrefix:=state;
  invalidate;;
end;

procedure TCECustom