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
    property Button