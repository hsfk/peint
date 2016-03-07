unit UPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  StdCtrls, Dialogs, UCustomControls, UObjectMove, UCustomPaletteControls;

type

  TPalette = class(TACustomPanel)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
    procedure LoadToolState;
    procedure SetCurrentTool(AName: string);
  private
    FCurrentTool: TLabel;
    FScene: TCanvas;
    FIsExtended: boolean;
    FExtButton: TCustomButton;
    FToolShapes: TToolShapes;
    FPWidthEdit: TWidthEdit;
    FBStylesCbox: TBrushStylesCBox;
    procedure MouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DblClickEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ExtButtonClickEvent(Sender: TObject);
    procedure BStyleCBoxSelectEvent(Sender: TObject);
    procedure PWidthEditChangeEvent(Sender: TObject);
  end;

var
  Palette: TPalette;

implementation

constructor TPalette.Create(Scene: TCanvas; AParent: TComponent; ATop, ALeft: integer);
var
  i: integer;
  j: integer;
  ColorShape: TRandomShape;
begin
  FScene := Scene;
  FIsExtended := True;
  inherited Create(AParent, ATop, ALeft, 155, 203);
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;

  Randomize;
  for i := 0 to 9 do
    for j := 0 to 12 do
      ColorShape := TRandomShape.Create(Self, 88 + i * 11, 5 + j *
        11, @DblClickEvent, @MouseDownEvent);

  FToolShapes := TToolShapes.Create(Self, 20, 5);
  FPWidthEdit := TWidthEdit.Create(Self, 15, 105);
  FPWidthEdit.OnChange := @PWidthEditChangeEvent;
  FBStylesCbox := TBrushStylesCBox.Create(Self, 42, 55);
  FBStylesCbox.OnSelect := @BStyleCBoxSelectEvent;

  FCurrentTool := TACustomLabel.Create(Self, 69, 5, 30, 15, 'Pen');
  FExtButton := TACustomButton.Create(Self, 69, 120, 30, 15, '▲');
  FExtButton.OnClick := @ExtButtonClickEvent;
end;

procedure TPalette.LoadToolState;
begin
  FScene.Pen.Width := FPWidthEdit.Width;
  FScene.Pen.Color := FToolShapes.PenColor;
  FScene.Brush.Color := FToolShapes.BrushColor;
  FScene.Brush.Style := FBStylesCbox.Style;
end;

procedure TPalette.SetCurrentTool(AName: string);
begin
  FCurrentTool.Caption := AName;
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

procedure TPalette.BStyleCBoxSelectEvent(Sender: TObject);
begin
  FScene.Brush.Style := FBStylesCbox.Style;
end;

procedure TPalette.PWidthEditChangeEvent(Sender: TObject);
begin
  FScene.Pen.Width := FPWidthEdit.Width;
end;

procedure TPalette.MouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  PStyle: TBrushStyle;
begin
  if Button = mbLeft then begin
    FScene.Pen.Color := TShape(Sender).Brush.Color;
    FToolShapes.PenColor := TShape(Sender).Brush.Color;
  end;
  if Button = mbRight then begin
    PStyle := FScene.Brush.Style;
    FScene.Brush.Color := TShape(Sender).Brush.Color;
    FToolShapes.BrushColor := TShape(Sender).Brush.Color;
    FScene.Brush.Style := PStyle;
  end;
end;

procedure TPalette.DblClickEvent(Sender: TObject);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    Canvas.Pen.Color := Tshape(Sender).Brush.Color;
    FToolShapes.PenColor := TShape(Sender).Brush.Color;
  end;
end;

procedure TPalette.PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    FScene.Pen.Color := Tshape(Sender).Brush.Color;
  end;
end;

procedure TPalette.BrushShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
  PrevStyle: TBrushStyle;
begin
  ColorDialog := TColorDialog.Create(self);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    PrevStyle := FScene.brush.style;
    FScene.Brush.Color := TShape(Sender).Brush.Color;
    FScene.Brush.Style := PrevStyle;
  end;
end;

end.
