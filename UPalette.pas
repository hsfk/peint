unit UPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  StdCtrls, Dialogs, UCustomControls, UObjectMove, UTools;

type
  BrushStyleCbox = record
    Name: string;
    Style: TBrushStyle;
  end;

  TPaletteCBox = class(TACustomCBox)
  public
    BrushStyles: array of BrushStyleCbox;
    constructor Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
  private
    FScene: TCanvas;
    procedure InitStyle(AName: string; AStyle: TbrushStyle);
    procedure OnSelectEvent(Sender: TObject);
  published
    function GetBrushStyle: TBrushStyle;
  end;

  TPaletteShape = class(TShape)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent);
    function GetBrushColor: TColor;
    function GetPenColor: TColor;
  private
    FScene: TCanvas;
    FBrushShape: TShape;
    FPenShape: TShape;
    procedure CreatePalette(AParent: TComponent; ATop, ALeft: integer);
    procedure MouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DblClickEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

  TPaletteEdit = class(TACustomEdit)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
    function GetPenWidth: integer;
  private
    FScene: TCanvas;
    FIncButton: TACustomButton;
    FDecButton: TACustomButton;
    procedure ButtonClickEvent(Sender: TObject);
    procedure OnChangeEvent(Sender: TObject);
    procedure EditingDoneEvent(Sender: TObject);
  end;

  TPalette = class(TACustomPanel)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
    procedure LoadToolState;
    procedure CurrentToolInvalidate;
  private
    FCurrentTool: TLabel;
    FScene: TCanvas;
    FPaletteCBox: TPaletteCBox;
    FPaletteShape: TPaletteShape;
    FPaletteEdit: TPaletteEdit;
    FIsExtended: boolean;
    FExtButton: TCustomButton;
    procedure ExtButtonClickEvent(Sender: TObject);
  end;

var
  Palette: TPalette;

implementation

constructor TPalette.Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
begin
  FScene := Scene;
  FIsExtended := True;
  inherited Create(AParent, ATop, ALeft, 155, 203);
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;
  FCurrentTool := TACustomLabel.Create(Self, 69, 5, 30, 15, 'Pen');
  FExtButton := TACustomButton.Create(Self, 69, 120, 30, 15, '▲');
  FExtButton.OnClick := @ExtButtonClickEvent;
  FPaletteShape := TPaletteShape.Create(FScene, Self);
  FPaletteCBox := TPaletteCBox.Create(FScene, Self, 42, 55);
  FPaletteEdit := TPaletteEdit.Create(FScene, Self, 15, 105);
end;

procedure TPalette.LoadToolState;
begin
  FScene.Pen.Width := FPaletteEdit.GetPenWidth;
  FScene.Pen.Color := FPaletteShape.GetPenColor;
  //  FScene.Pen.Style:=FPaletteShape.Ge;
  FScene.Brush.Color := FPaletteShape.GetBrushColor;
  FScene.Brush.Style := FPaletteCBox.GetBrushStyle;
end;

procedure TPalette.CurrentToolInvalidate;
begin
  FCurrentTool.Caption := Tools[CurrentToolIndex].Name;
end;

procedure TPalette.ExtButtonClickEvent(Sender: TObject);
begin
  if FIsExtended = True then begin
    Self.Height := 88;
    TButton(Sender).Caption := '▼';
    FIsExtended := False;
  end
  else if FIsExtended = False then begin
    Self.Height := 203;
    TButton(Sender).Caption := '▲';
    FIsExtended := True;
  end;
end;

constructor TPaletteShape.Create(Scene: TCanvas; AParent: TComponent);
var
  i: integer;
  j: integer;
begin
  FScene := Scene;
  Randomize;
  for i := 0 to 9 do
    for j := 0 to 12 do
      CreatePalette(AParent, i * 11 + 88, j * 11 + 5);
  FPenShape := TACustomShape.Create(AParent, 20, 5, 30, 30, clBlack,
    psSolid, clBlack, bsSolid);
  FPenShape.OnMouseDown := @PenShapeMouseDownEvent;
  FBrushShape := TACustomShape.Create(AParent, 35, 20, 30, 30, clBlack,
    psSolid, clWhite, bsSolid);
  FBrushShape.OnMouseDown := @BrushShapeMouseDownEvent;
end;

procedure TPaletteShape.CreatePalette(AParent: TComponent; ATop, ALeft: integer);
var
  RColor: TColor;
  ColorShape: TShape;
begin
  RColor := Random(256 * 256 * 256);
  ColorShape := TShape.Create(AParent);
  with ColorShape do begin
    OnMouseDown := @MouseDownEvent;
    OnDblClick := @DblClickEvent;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Top := ATop;
    Left := ALeft;
    Width := 12;
    Height := 12;
    Parent := TWinControl(AParent);
    Brush.Color := RColor;
  end;
end;

procedure TPaletteShape.MouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PStyle: TBrushStyle;
begin
  if Button = mbLeft then begin
    FScene.Pen.Color := TShape(Sender).Brush.Color;
    FPenShape.Brush.Color := TShape(Sender).Brush.Color;
  end;
  if Button = mbRight then begin
    PStyle := FScene.Brush.Style;
    FScene.Brush.Color := TShape(Sender).Brush.Color;
    FBrushShape.Brush.Color := TShape(Sender).Brush.Color;
    FScene.Brush.Style := PStyle;
  end;
end;

procedure TPaletteShape.DblClickEvent(Sender: TObject);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    Canvas.Pen.Color := Tshape(Sender).Brush.Color;
    FPenShape.Brush.Color := Tshape(Sender).Brush.Color;
  end;
end;

procedure TPaletteShape.PenShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    FScene.Pen.Color := Tshape(Sender).Brush.Color;
  end;
end;

procedure TPaletteShape.BrushShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
  PrevStyle: TBrushStyle;
begin
  ColorDialog := TColorDialog.Create(self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    PrevStyle := FScene.brush.style;
    FScene.Brush.Color := Tshape(Sender).Brush.Color;
    FScene.Brush.Style := PrevStyle;
  end;
end;

function TPaletteShape.GetBrushColor: TColor;
begin
  Result := FBrushShape.Brush.Color;
end;

function TPaletteShape.GetPenColor: TColor;
begin
  Result := FPenShape.Brush.Color;
end;

constructor TPaletteCBox.Create(Scene: TCanvas; AParent: TComponent;
  ATop, ALeft: integer);
var
  i: integer;
begin
  FScene := Scene;
  InitStyle('Horizontal', bsHorizontal);
  InitStyle('Solid', bsSolid);
  InitStyle('Clear', bsClear);
  InitStyle('Vertical', bsVertical);
  InitStyle('FDiagonal', bsFdiagonal);
  InitStyle('BDiagonal', bsBDiagonal);
  InitStyle('Cross', bsCross);
  InitStyle('Diagonal Cross', bsDiagCross);
  inherited Create(AParent, ATop, ALeft, 95, 20, True);
  Self.OnSelect := @OnSelectEvent;
  for i := 0 to high(BrushStyles) do
    Self.Items.Add(BrushStyles[i].Name);
  Self.ItemIndex := 2;
end;

procedure TPaletteCBox.InitStyle(AName: string; AStyle: TBrushStyle);
begin
  setlength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do begin
    Name := AName;
    Style := AStyle;
  end;
end;

procedure TPaletteCBox.OnSelectEvent(Sender: TObject);
begin
  FScene.Brush.Style := BrushStyles[Self.ItemIndex].Style;
end;

function TPaletteCBox.GetBrushStyle: TBrushStyle;
begin
  Result := BrushStyles[Self.ItemIndex].Style;
end;

constructor TPaletteEdit.Create(Scene: TCanvas; AParent: TComponent;
  ATop, ALeft: integer);
begin
  FScene := Scene;
  inherited Create(AParent, ATop, ALeft, 30, 15, '1', True, taRightJustify);
  Self.OnChange := @OnChangeEvent;
  Self.OnEditingDone := @EditingDoneEvent;
  FIncButton := TACustomButton.Create(AParent, ATop, ALeft - 15 - 1, 15, 23, '+');
  FIncButton.Tag := 1;
  FIncButton.OnClick := @ButtonClickEvent;
  FDecButton := TACustomButton.Create(AParent, ATop, ALeft + 30 + 1, 15, 23, '-');
  FDecButton.Tag := -1;
  FDecButton.OnClick := @ButtonClickEvent;
end;

procedure TPaletteEdit.ButtonClickEvent(Sender: TObject);
begin
  Self.Text := IntToStr(StrToInt(self.Text) + TButton(Sender).Tag);
  EditingDoneEvent(self);
end;

procedure TPaletteEdit.OnChangeEvent(Sender: TObject);
begin
  if (Self.Text = '') or (StrToInt(Self.Text) < 1) then
    Self.Text := '1';
  if StrToInt(self.Text) > 99 then
    Self.Text := '99';
end;

procedure TPaletteEdit.EditingDoneEvent(Sender: TObject);
begin
  FScene.Pen.Width := StrToInt(Self.Text);
end;

function TPaletteEdit.GetPenWidth: integer;
begin
  Result := StrToInt(Self.Text);
end;

end.
