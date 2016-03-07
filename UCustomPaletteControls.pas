unit UCustomPaletteControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCustomControls, Graphics, StdCtrls, ExtCtrls, Controls;

type
  TWidthEdit = class(TACustomEdit)
  public
    constructor Create(AParent: TComponent; ATop, ALeft: integer);
  private
    FIncButton: TACustomButton;
    FDecButton: TACustomButton;
    procedure SetWidth(AWidth: integer);
    function GetWidth: integer;
    procedure ButtonClickEvent(Sender: TObject);
  published
    property Width: integer read GetWidth write SetWidth;
  end;

  TToolShapes = class
  public
    FPenShape: TACustomShape;
    FBrushShape: TACustomShape;
    constructor Create(AParent: TComponent; ATop, ALeft: integer);
  private
    procedure SetPenShapeColor(AColor: TColor);
    function GetPenShapeColor: TColor;
    procedure SetBrushShapeColor(AColor: TColor);
    function GetBrushShapeColor: TColor;
  published
    property PenColor: TColor read GetPenShapeColor write SetPenShapeColor;
    property BrushColor: TColor read GetBrushShapeColor write SetBrushShapeColor;
  end;

  BStyleCBoxItem = record
    Name: string;
    Style: TBrushStyle;
  end;

  TBrushStylesCBox = class(TACustomCBox)
  public
    constructor Create(AParent: TComponent; ATop, ALeft: integer);
  private
    BrushStyles: array of BStyleCBoxItem;
    procedure InitStyle(AName: string; AStyle: TBrushStyle);
    procedure SetBrushStyle(AStyle: TBrushStyle);
    function GetBrushStyle: TBrushStyle;
  published
    property Style: TBrushStyle read GetBrushStyle write SetBrushStyle;
  end;

  DblClickEvent = procedure(Sender: TObject) of object;
  MouseDownEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer) of object;

  TRandomShape = class(TShape)
  public
    constructor Create(AParent: TComponent; ATop, ALeft: integer;
      ADblClickEvent: DblClickEvent; AMouseDownEvent: MouseDownEvent);
  end;

implementation

constructor TWidthEdit.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft, 30, 15, '1', True, taRightJustify);
  FIncButton := TACustomButton.Create(AParent, ATop, ALeft - 15 - 1, 15, 23, '+');
  FDecButton := TACustomButton.Create(AParent, ATop, ALeft + 30 + 1, 15, 23, '-');
  FIncButton.Tag := 1;
  FDecButton.Tag := -1;
  FIncButton.OnClick := @ButtonClickEvent;
  FDecButton.OnClick := @ButtonClickEvent;
end;

procedure TWidthEdit.SetWidth(AWidth: integer);
begin
  if AWidth <= 1 then
    AWidth := 1;
  if AWidth >= 99 then
    AWidth := 99;
  Self.Text := IntToStr(AWidth);
end;

function TWidthEdit.GetWidth: integer;
begin
  Result := StrToInt(Self.Text);
end;

procedure TWidthEdit.ButtonClickEvent(Sender: TObject);
begin
  Self.Width := Self.Width + TButton(Sender).Tag;
end;

constructor TToolShapes.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  FPenShape := TACustomShape.Create(AParent, ATop, ALeft, 30, 30,
    clBlack, psSolid, clBlack, bsSolid);
  FBrushShape := TACustomShape.Create(AParent, ATop + 15, ALeft + 15,
    30, 30, clBlack, psSolid, clWhite, bsSolid);
end;

procedure TToolShapes.SetPenShapeColor(AColor: TColor);
begin
  FPenShape.Brush.Color := AColor;
end;

function TToolShapes.GetPenShapeColor: TColor;
begin
  Result := FPenShape.Brush.Color;
end;

procedure TToolShapes.SetBrushShapeColor(AColor: TColor);
begin
  FBrushShape.Brush.Color := AColor;
end;

function TToolShapes.GetBrushShapeColor: TColor;
begin
  Result := FBrushShape.Brush.Color;
end;

constructor TBrushStylesCBox.Create(AParent: TComponent; ATop, ALeft: integer);
var
  i: integer;
begin
  InitStyle('Horizontal', bsHorizontal);
  InitStyle('Solid', bsSolid);
  InitStyle('Clear', bsClear);
  InitStyle('Vertical', bsVertical);
  InitStyle('FDiagonal', bsFdiagonal);
  InitStyle('BDiagonal', bsBDiagonal);
  InitStyle('Cross', bsCross);
  InitStyle('Diagonal Cross', bsDiagCross);
  inherited Create(AParent, ATop, ALeft, 95, 20, True);
  for i := 0 to High(BrushStyles) do
    Self.Items.Add(BrushStyles[i].Name);
  Self.ItemIndex := 2;
end;

procedure TBrushStylesCBox.InitStyle(AName: string; AStyle: TBrushStyle);
begin
  Setlength(BrushStyles, Length(BrushStyles) + 1);
  BrushStyles[High(BrushStyles)].Name := AName;
  BrushStyles[High(BrushStyles)].Style := AStyle;
end;

procedure TBrushStylesCBox.SetBrushStyle(AStyle: TBrushStyle);
var
  i: integer;
begin
  for i := 0 to High(BrushStyles) do
    if AStyle = BrushStyles[i].Style then
      Self.ItemIndex := i;
end;

function TBrushStylesCBox.GetBrushStyle: TBrushStyle;
begin
  Result := BrushStyles[Self.ItemIndex].Style;
end;

constructor TRandomShape.Create(AParent: TComponent; ATop, ALeft: integer;
  ADblClickEvent: DblClickEvent; AMouseDownEvent: MouseDownEvent);
begin
  inherited Create(AParent);
  Self.OnMouseDown := AMouseDownEvent;
  Self.OnDblClick := ADblClickEvent;
  Self.Pen.Color := clBlack;
  Self.Pen.Width := 1;
  Self.Top := ATop;
  Self.Left := ALeft;
  Self.Width := 12;
  Self.Height := 12;
  Self.Parent := TWinControl(AParent);
  Self.Brush.Color := Random(256 * 256 * 256);
end;

end.
