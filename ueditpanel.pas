unit UEditPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCustomControls, UFigure, Graphics, Controls,
  UObjectMove, StdCtrls, Dialogs, ExtCtrls, UHistory, UCustomPaletteControls;

type
  TFigureEdit = class
  public
    constructor Create(AParent: TComponent; ATop, ALeft: integer);
    procedure LoadFigure(Figure: TFigure);
  private
    FParent: TComponent;
    FFigure: TFigure;
    FToolShapes: TToolShapes;
    FPWidthEdit: TWidthEdit;
    FBStylesCbox: TBrushStylesCBox;
    procedure OnSelectEvent(Sender: TObject);
    procedure PWidthEditChangeEvent(Sender: TObject);
    procedure PenShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BrushShapeMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

implementation

constructor TFigureEdit.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  FParent := AParent;
  FToolShapes := TToolShapes.Create(AParent, ATop, ALeft + 102);
  FToolShapes.FPenShape.OnMouseDown := @PenShapeMouseDownEvent;
  FToolShapes.FBrushShape.OnMouseDown := @BrushShapeMouseDownEvent;
  FBStylesCbox := TBrushStylesCBox.Create(AParent, ATop, ALeft);
  FBStylesCbox.OnSelect := @OnSelectEvent;
  FPWidthEdit := TWidthEdit.Create(AParent, ATop + 30, ALeft + 15);
  FPWidthEdit.OnChange := @PWidthEditChangeEvent;
end;

procedure TFigureEdit.LoadFigure(Figure: TFigure);
begin
  FFigure := Figure;
  FPWidthEdit.Width := FFigure.PenWidth;
  FToolShapes.PenColor := FFigure.PenColor;
  FToolShapes.BrushColor := FFigure.BrushColor;
end;

procedure TFigureEdit.OnSelectEvent(Sender: TObject);
begin
  FFigure.BrushStyle := FBStylesCbox.Style;
  History.Show;
end;

procedure TFigureEdit.PWidthEditChangeEvent(Sender: TObject);
begin
  FFigure.PenWidth := FPWidthEdit.Width;
  History.Show;
end;

procedure TFigureEdit.PenShapeMouseDownEvent(Sender: TObject;
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

procedure TFigureEdit.BrushShapeMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColorDialog: TColorDialog;
  PrevStyle: TBrushStyle;
begin
  ColorDialog := TColorDialog.Create(FParent);
  if ColorDialog.Execute then begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    PrevStyle := FFigure.BrushStyle;
    FFigure.BrushColor := TShape(Sender).Brush.Color;
    FFigure.BrushStyle := PrevStyle;
  end;
  History.Show;
end;

end.
