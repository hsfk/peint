unit UPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  StdCtrls, UToolsManager, Dialogs;

type
  BrushStyleCbox = record
    Name: string;
    Style: TBrushStyle;
  end;

  TPalette = class(TPanel)
  public
    constructor Create(parent_: TComponent);
  private
    isExtended: boolean;
    procedure CreateExtentionButton(parent_: TComponent; top_, left_: integer);
    procedure ExtButtonClickEvent(Sender: TObject);
  end;

  TPaletteShape = class(TShape)
  public
    constructor Create(parent_: TComponent);
  private
    PenShape, BrushShape: TShape;
    procedure CreatePalette(parent_: TComponent; top_, left_: integer);
    procedure CreatePenShape(parent_: TComponent; top_, left_: integer);
    procedure CreateBrushShape(parent_: TComponent; top_, left_: integer);
    procedure MouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DblClickEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

  TPaletteCBox = class(TComboBox)
    constructor Create(parent_: TComponent; top_, left_: integer);
  private
    BrushStyles: array of BrushStyleCbox;
    procedure InitStyle(name_: string; style_: TbrushStyle);
    procedure OnSelectEvent(Sender: TObject);
  end;

  TPaletteEdit = class(TEdit)
    constructor Create(parent_: TComponent; top_, left_: integer);
  private
    procedure CreateIncButton(parent_: TComponent; top_, left_: integer);
    procedure CreateDecButton(parent_: TComponent; top_, left_: integer);
    procedure ButtonClickEvent(Sender: TObject);
    procedure OnChangeEvent(Sender: TObject);
    procedure EditingDoneEvent(Sender: TObject);
  end;

implementation

constructor TPalette.Create(parent_: TComponent);
begin
  isExtended := True;
  inherited Create(parent_);
  with self do begin
    Top := 160;
    Parent := twincontrol(parent_);
    Width := 155;
    Height := 203;
    brush.Color := clDkGray;
    BevelInner := bvNone;
  end;
  CreateExtentionButton(self, 68, 120);
  TPaletteShape.Create(self);
  TPaletteCBox.Create(self, 42, 55);
  TPaletteEdit.Create(self, 15, 105);
end;

procedure TPalette.CreateExtentionButton(parent_: TComponent; top_, left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(parent_);
  with Button do begin
    Parent := twincontrol(parent_);
    OnClick := @ExtButtonClickEvent;
    top := top_;
    left := left_;
    Width := 30;
    Height := 15;
    Caption := '▲';
  end;
end;

procedure TPalette.ExtButtonClickEvent(Sender: TObject);
begin
  if isExtended = True then begin
    self.Height := 88;
    TButton(Sender).Caption := '▼';
    isExtended := False;
  end
  else if isExtended = False then begin
    self.Height := 203;
    TButton(Sender).Caption := '▲';
    isExtended := True;
  end;
end;

constructor TPaletteShape.Create(parent_: TComponent);
var
  i, j: integer;
begin
  randomize;
  for i := 0 to 9 do
    for j := 0 to 12 do
      CreatePalette(parent_, i * 11 + 88, j * 11 + 5);
  CreatePenShape(parent_, 20, 5);
  CreateBrushShape(parent_, 35, 20);
end;

procedure TPaletteShape.CreatePalette(parent_: TComponent; top_, left_: integer);
var
  RColor: TColor;
  ColorShape: TShape;
begin
  ColorShape := TShape.Create(parent_);
  RColor := random(256 * 256 * 256);
  with ColorShape do begin
    OnMouseDown := @MouseDownEvent;
    OnDblClick := @DblClickEvent;
    pen.Color := clBlack;
    pen.Width := 1;
    top := top_;
    left := left_;
    Width := 12;
    Height := 12;
    parent := twincontrol(parent_);
    brush.Color := RColor;
  end;
end;

procedure TPaletteShape.CreatePenShape(parent_: TComponent; top_, left_: integer);
begin
  PenShape := TShape.Create(parent_);
  with PenShape do begin
    OnMouseDown := @PenShapeMouseDownEvent;
    brush.Color := clBlack;
    top := top_;
    left := left_;
    Width := 30;
    Height := 30;
    parent := twincontrol(parent_);
  end;
end;

procedure TPaletteShape.CreateBrushShape(parent_: TComponent; top_, left_: integer);
begin
  BrushShape := TShape.Create(parent_);
  with BrushShape do begin
    OnMouseDown := @BrushShapeMouseDownEvent;
    brush.Color := clWhite;
    top := top_;
    left := left_;
    Width := 30;
    Height := 30;
    parent := twincontrol(parent_);
  end;
end;

procedure TPaletteShape.MouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PStyle: TBrushStyle;
begin
  if Button = mbLeft then begin
    ToolsManager.FCanvas.canvas.pen.color := Tshape(Sender).Brush.color;
    PenShape.Brush.Color := Tshape(Sender).Brush.color;
  end;
  if Button = mbRight then begin
    PStyle := ToolsManager.FCanvas.canvas.brush.style;
    ToolsManager.FCanvas.canvas.brush.color := Tshape(Sender).Brush.color;
    BrushShape.Brush.Color := Tshape(Sender).Brush.color;
    ToolsManager.FCanvas.canvas.brush.style := PStyle;
  end;
end;

procedure TPaletteShape.DblClickEvent(Sender: TObject);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.color := ColorDialog.Color;
    ToolsManager.FCanvas.Canvas.pen.color := Tshape(Sender).Brush.color;
    PenShape.Brush.Color := Tshape(Sender).Brush.color;
  end;
end;

procedure TPaletteShape.PenShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.color := ColorDialog.Color;
    ToolsManager.FCanvas.Canvas.pen.color := Tshape(Sender).Brush.color;
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
    TShape(Sender).Brush.color := ColorDialog.Color;
    PrevStyle := ToolsManager.FCanvas.Canvas.brush.style;
    ToolsManager.FCanvas.Canvas.brush.color := Tshape(Sender).Brush.color;
    ToolsManager.FCanvas.Canvas.brush.style := PrevStyle;
  end;
end;

constructor TPaletteCBox.Create(parent_: TComponent; top_, left_: integer);
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

  inherited Create(parent_);
  with self do begin
    ReadOnly := True;
    OnSelect := @OnSelectEvent;
    parent := twincontrol(parent_);
    top := top_;
    left := left_;
    Width := 95;
    Height := 20;
  end;
  for i := 0 to high(BrushStyles) do
    self.Items.Add(brushstyles[i].Name);
  self.ItemIndex := 2;
end;

procedure TPaletteCBox.InitStyle(name_: string; style_: TBrushStyle);
begin
  setlength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do begin
    Name := name_;
    style := style_;
  end;
end;

procedure TPaletteCBox.OnSelectEvent(Sender: TObject);
begin
  ToolsManager.FCanvas.canvas.brush.Style := BrushStyles[self.ItemIndex].Style;
end;

constructor TPaletteEdit.Create(parent_: TComponent; top_, left_: integer);
begin
  inherited Create(parent_);
  with self do begin
    parent := twincontrol(parent_);
    OnEditingDone := @EditingDoneEvent;
    OnChange := @OnChangeEvent;
    NumbersOnly := True;
    top := top_;
    left := left_;
    Width := 30;
    Alignment := taRightJustify;
    Caption := '1';
  end;
  CreateIncButton(parent_, top_, left_ - 15 - 1);
  CreateDecButton(parent_, top_, left_ + 30 + 1);
end;

procedure TPaletteEdit.CreateIncButton(parent_: TComponent; top_, left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(parent_);
  with Button do begin
    Parent := twincontrol(parent_);
    OnClick := @ButtonClickEvent;
    top := top_;
    left := left_;
    Width := 15;
    Height := 23;
    tag := 1;
    Caption := '+';
  end;
end;

procedure TPaletteEdit.CreateDecButton(parent_: TComponent; top_, left_: integer);
var
  Button: TButton;
begin
  Button := TButton.Create(parent_);
  with Button do begin
    Parent := twincontrol(parent_);
    OnClick := @ButtonClickEvent;
    top := top_;
    left := left_;
    Width := 15;
    Height := 23;
    Tag := -1;
    Caption := '-';
  end;
end;

procedure TPaletteEdit.ButtonClickEvent(Sender: TObject);
begin
  self.Text := IntToStr(StrToInt(self.Text) + TButton(Sender).Tag);
  EditingDoneEvent(self);
end;

procedure TPaletteEdit.OnChangeEvent(Sender: TObject);
begin
  if (self.Text = '') or (StrToInt(self.Text) < 1) then
    self.Text := '1';
  if StrToInt(self.Text) > 99 then
    self.Text := '99';
end;

procedure TPaletteEdit.EditingDoneEvent(Sender: TObject);
begin
  ToolsManager.FCanvas.Canvas.pen.Width := StrToInt(self.Text);
end;

end.
