unit UEditPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCustomControls, UFigure, UPalette, Graphics, Controls,
  UObjectMove, StdCtrls, Dialogs, ExtCtrls, UHistory;

type
  TEditPanel = class
  public
    constructor Create(AParent: TComponent);
    procedure LoadFigure(Figure: TFigure);
  private
    FParent : TComponent;
    BrushStyles: array of BrushStyleCbox;
    FPenShape: TACustomShape;
    FBrushShape: TACustomShape;
    FFigure: TFigure;
    FIncButton: TACustomButton;
    FDecButton: TACustomButton;
    FPenWidthEdit: TACustomEdit;
    FStyleCBox: TACustomCBox;
    procedure InitStyle(AName: string; AStyle: TBrushStyle);
    procedure OnSelectEvent(Sender: TObject);
    procedure ButtonClickEvent(Sender: TObject);
    procedure OnChangeEvent(Sender: TObject);
    procedure EditingDoneEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

implementation


constructor TEditPanel.Create(AParent: TComponent);
var
  i: integer;
begin
  FParent := AParent;
  InitStyle('Horizontal', bsHorizontal);
  InitStyle('Solid', bsSolid);
  InitStyle('Clear', bsClear);
  InitStyle('Vertical', bsVertical);
  InitStyle('FDiagonal', bsFdiagonal);
  InitStyle('BDiagonal', bsBDiagonal);
  InitStyle('Cross', bsCross);
  InitStyle('Diagonal Cross', bsDiagCross);
  FStyleCBox := TACustomCBox.Create(AParent, 280, 5, 100, 15, True);
  for i := 0 to high(BrushStyles) do
    FStyleCBox.Items.Add(BrushStyles[i].Name);

  FStyleCBox.ItemIndex := 2;
  FStyleCbox.OnSelect := @OnSelectEvent;
  FPenShape := TACustomShape.Create(AParent, 280, 5 + 102, 30, 30, clBlack,
    psSolid, clBlack, bsSolid);
  FPenShape.OnMouseDown := @PenShapeMouseDownEvent;
  FBrushShape := TACustomShape.Create(AParent, 295, 20 + 102, 30, 30,
    clBlack, psSolid, clWhite, bsSolid);
  FBrushShape.OnMouseDown := @BrushShapeMouseDownEvent;

  FPenWidthEdit := TACustomEdit.Create(AParent, 310, 5, 30, 15, '1', True, taRightJustify);
  FPenWidthEdit.OnChange := @OnChangeEvent;
  FPenWidthEdit.OnEditingDone := @EditingDoneEvent;
  FIncButton := TACustomButton.Create(AParent, 310, 35, 15, 23, '+');
  FIncButton.Tag := 1;
  FIncButton.OnClick := @ButtonClickEvent;
  FDecButton := TACustomButton.Create(AParent, 310, 49, 15, 23, '-');
  FDecButton.Tag := -1;
  FDecButton.OnClick := @ButtonClickEvent;
end;

procedure TEditPanel.LoadFigure(Figure: TFigure);
var
  i: integer;
begin
  FFigure := Figure;
  FPenWidthEdit.Text := IntToStr(FFigure.PenWidth);
  FPenShape.Brush.Color := FFigure.PenColor;
  FBrushShape.Brush.Color := FFigure.BrushColor;
  for i := 0 to Length(BrushStyles) - 1 do
    if FFigure.BrushStyle = BrushStyles[i].Style then
      FStyleCBox.ItemIndex := i;
end;

procedure TEditPanel.InitStyle(AName: string; AStyle: TBrushStyle);
begin
  SetLength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do begin
    Name := AName;
    Style := AStyle;
  end;
end;

procedure TEditPanel.OnSelectEvent(Sender: TObject);
begin
  FFigure.BrushStyle := BrushStyles[FStyleCBox.ItemIndex].Style;
  History.Show;
end;

procedure TEditPanel.ButtonClickEvent(Sender: TObject);
begin
  FPenWidthEdit.Text := IntToStr(StrToInt(FPenWidthEdit.Text) + TButton(Sender).Tag);
  EditingDoneEvent(FPenWidthEdit);
end;

procedure TEditPanel.OnChangeEvent(Sender: TObject);
begin
  if (FPenWidthEdit.Text = '') or (StrToInt(FPenWidthEdit.Text) < 1) then
    FPenWidthEdit.Text := '1';
  if StrToInt(FPenWidthEdit.Text) > 99 then
    FPenWidthEdit.Text := '99';
end;

procedure TEditPanel.EditingDoneEvent(Sender: TObject);
begin
  FFigure.PenWidth := StrToInt(FPenWidthEdit.Text);
  History.Show;
end;

procedure TEditPanel.PenShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(FParent);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    FFigure.PenColor := TShape(Sender).Brush.Color;
  end;
  History.Show;
end;

procedure TEditPanel.BrushShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
  PrevStyle: TBrushStyle;
begin
  ColorDialog := TColorDialog.Create(FParent);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    PrevStyle := FFigure.BrushStyle;
    FFigure.BrushColor := Tshape(Sender).Brush.Color;
    FFigure.BrushStyle := PrevStyle;
  end;
  History.Show;
end;

end.
