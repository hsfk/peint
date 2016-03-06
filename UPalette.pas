unit UPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  StdCtrls, Dialogs;

type
  BrushStyleCbox = record
    Name: string;
    Style: TBrushStyle;
  end;

  TPaletteCBox = class(TComboBox)
  public
    BrushStyles: array of BrushStyleCbox;
    constructor Create(Parent_: TComponent; Top_, Left_: integer; Scene: TCanvas);
  private
    FScene: TCanvas;
    procedure InitStyle(Name_: string; Style_: TbrushStyle);
    procedure OnSelectEvent(Sender: TObject);
  end;

  TPaletteShape = class(TShape)
  public
    BrushShape: TShape;
    constructor Create(Parent_: TComponent; Scene: TCanvas);
  private
    FScene: TCanvas;
    PenShape: TShape;
    procedure CreatePalette(Parent_: TComponent; Top_, Left_: integer);
    procedure CreatePenShape(Parent_: TComponent; Top_, Left_: integer);
    procedure CreateBrushShape(Parent_: TComponent; Top_, Left_: integer);
    procedure MouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DblClickEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

  TPalette = class(TPanel)
  public
    PaletteCBox: TPaletteCBox;
    PaletteShape: TPaletteShape;
    constructor Create(Parent_: TComponent; Scene: TCanvas);
  private
    FScene: TCanvas;
    IsExtended: boolean;
    procedure CreateExtentionButton(Parent_: TComponent; Top_, Left_: integer);
    procedure ExtButtonClickEvent(Sender: TObject);
  end;

  TPaletteEdit = class(TEdit)
    constructor Create(Parent_: TComponent; Top_, Left_: integer; Scene: TCanvas);
  private
    FScene: TCanvas;
    procedure CreateIncButton(Parent_: TComponent; Top_, Left_: integer);
    procedure CreateDecButton(Parent_: TComponent; Top_, Left_: integer);
    procedure ButtonClickEvent(Sender: TObject);
    procedure OnChangeEvent(Sender: TObject);
    procedure EditingDoneEvent(Sender: TObject);
  end;

implementation

constructor TPalette.Create(Parent_: TComponent; Scene: TCanvas);
begin
  FScene := Scene;
  IsExtended := True;
  inherited Create(Parent_);
  with Self do begin
    Top := 0;
    Parent := twincontrol(Parent_);
    Width := 155;
    Height := 203;
    Left := 660;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
  end;
  CreateExtentionButton(Self, 68, 120);
  PaletteShape := TPaletteShape.Create(Self, FScene);
  PaletteCBox := TPaletteCBox.Create(Self, 42, 55, FScene);
  TPaletteEdit.Create(Self, 15, 105, FScene);
end;

procedure TPalette.CreateExtentionButton(Parent_: TComponent; Top_, Left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(Parent_);
  with Button do begin
    Parent := twincontrol(Parent_);
    OnClick := @ExtButtonClickEvent;
    Top := Top_;
    Left := Left_;
    Width := 30;
    Height := 15;
    Caption := '▲';
  end;
end;

procedure TPalette.ExtButtonClickEvent(Sender: TObject);
begin
  if IsExtended = True then begin
    Self.Height := 88;
    TButton(Sender).Caption := '▼';
    IsExtended := False;
  end
  else if IsExtended = False then begin
    Self.Height := 203;
    TButton(Sender).Caption := '▲';
    IsExtended := True;
  end;
end;

constructor TPaletteShape.Create(Parent_: TComponent; Scene: TCanvas);
var
  i: integer;
  j: integer;
begin
  FScene := Scene;
  Randomize;
  for i := 0 to 9 do
    for j := 0 to 12 do
      CreatePalette(Parent_, i * 11 + 88, j * 11 + 5);
  CreatePenShape(Parent_, 20, 5);
  CreateBrushShape(Parent_, 35, 20);
end;

procedure TPaletteShape.CreatePalette(Parent_: TComponent; Top_, Left_: integer);
var
  RColor: TColor;
  ColorShape: TShape;
begin
  ColorShape := TShape.Create(Parent_);
  RColor := Random(256 * 256 * 256);
  with ColorShape do begin
    OnMouseDown := @MouseDownEvent;
    OnDblClick := @DblClickEvent;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Top := Top_;
    Left := Left_;
    Width := 12;
    Height := 12;
    Parent := twincontrol(Parent_);
    Brush.Color := RColor;
  end;
end;

procedure TPaletteShape.CreatePenShape(Parent_: TComponent; Top_, Left_: integer);
begin
  PenShape := TShape.Create(Parent_);
  with PenShape do begin
    OnMouseDown := @PenShapeMouseDownEvent;
    Brush.Color := clBlack;
    Top := Top_;
    Left := Left_;
    Width := 30;
    Height := 30;
    Parent := twincontrol(Parent_);
  end;
end;

procedure TPaletteShape.CreateBrushShape(Parent_: TComponent; Top_, Left_: integer);
begin
  BrushShape := TShape.Create(Parent_);
  with BrushShape do begin
    OnMouseDown := @BrushShapeMouseDownEvent;
    Brush.Color := clWhite;
    Top := Top_;
    Left := Left_;
    Width := 30;
    Height := 30;
    Parent := twincontrol(Parent_);
  end;
end;

procedure TPaletteShape.MouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PStyle: TBrushStyle;
begin
  if Button = mbLeft then begin
    FScene.Pen.Color := Tshape(Sender).Brush.Color;
    PenShape.Brush.Color := Tshape(Sender).Brush.Color;
  end;
  if Button = mbRight then begin
    PStyle := FScene.Brush.Style;
    FScene.Brush.Color := Tshape(Sender).Brush.Color;
    BrushShape.Brush.Color := Tshape(Sender).Brush.Color;
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
    PenShape.Brush.Color := Tshape(Sender).Brush.Color;
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

constructor TPaletteCBox.Create(Parent_: TComponent; Top_, Left_: integer;
  Scene: TCanvas);
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

  inherited Create(Parent_);
  with self do begin
    ReadOnly := True;
    OnSelect := @OnSelectEvent;
    Parent := twincontrol(Parent_);
    Top := Top_;
    Left := Left_;
    Width := 95;
    Height := 20;
  end;
  for i := 0 to high(BrushStyles) do
    Self.Items.Add(BrushStyles[i].Name);
  Self.ItemIndex := 2;
end;

procedure TPaletteCBox.InitStyle(Name_: string; Style_: TBrushStyle);
begin
  setlength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do begin
    Name := Name_;
    Style := Style_;
  end;
end;

procedure TPaletteCBox.OnSelectEvent(Sender: TObject);
begin
  FScene.Brush.Style := BrushStyles[Self.ItemIndex].Style;
end;

constructor TPaletteEdit.Create(Parent_: TComponent; Top_, Left_: integer;
  Scene: TCanvas);
begin
  FScene := Scene;
  inherited Create(Parent_);
  with Self do begin
    Parent := twincontrol(Parent_);
    OnEditingDone := @EditingDoneEvent;
    OnChange := @OnChangeEvent;
    NumbersOnly := True;
    Top := Top_;
    Left := Left_;
    Width := 30;
    Alignment := taRightJustify;
    Caption := '1';
  end;
  CreateIncButton(Parent_, Top_, Left_ - 15 - 1);
  CreateDecButton(Parent_, Top_, Left_ + 30 + 1);
end;

procedure TPaletteEdit.CreateIncButton(Parent_: TComponent; Top_, Left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(Parent_);
  with Button do begin
    Parent := twincontrol(Parent_);
    OnClick := @ButtonClickEvent;
    Top := Top_;
    Left := Left_;
    Width := 15;
    Height := 23;
    Tag := 1;
    Caption := '+';
  end;
end;

procedure TPaletteEdit.CreateDecButton(Parent_: TComponent; Top_, Left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(Parent_);
  with Button do begin
    Parent := twincontrol(Parent_);
    OnClick := @ButtonClickEvent;
    Top := Top_;
    Left := Left_;
    Width := 15;
    Height := 23;
    Tag := -1;
    Caption := '-';
  end;
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

end.
