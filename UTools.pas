unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigure, Graphics, UHistory, UZoom, Controls, UPointUtils;

type
  ToolClass = class of TTool;

  Tool_ = record
    Recreate: boolean;
    Tool: ToolClass;
    Name: string;
  end;

  TTool = class
  public
    constructor Create(Scene: TCanvas); virtual;
    procedure Start(Point: TPoint; MButton: TMouseButton); virtual;
    procedure Continue(Point: TPoint; Shift: TShiftState); virtual;
    procedure Stop; virtual;
  private
    FScene: TCanvas;
  end;

  TPaintingTool = class(TTool)
  public
    procedure Start(Point: TPoint; MButton: TMouseButton); override;
    procedure Continue(Point: TPoint; Shift: TShiftState); override;
  private
    Figure: TFigure;
  end;

  TPenTool = class(TPaintingTool)
    constructor Create(Scene: TCanvas); override;
  end;

  TPolyLineTool = class(TPaintingTool)
    constructor Create(Scene: TCanvas); override;
    procedure Start(Point: TPoint; MButton: TMouseButton); override;
    procedure Continue(Point: TPoint; Shift: TShiftState); override;
  end;

  TLineTool = class(TPaintingTool)
    constructor Create(Scene: TCanvas); override;
  end;

  TRectangleTool = class(TPaintingTool)
    constructor Create(Scene: TCanvas); override;
  end;

  TEllipseTool = class(TPaintingTool)
    constructor Create(Scene: TCanvas); override;
  end;

  TZoomTool = class(TTool)
  public
    procedure Start(Point: TPoint; MButton: TMouseButton); override;
  end;

  THandTool = class(TTool)
  public
    procedure Start(Point: TPoint; MButton: TMouseButton); override;
    procedure Continue(Point: TPoint; Shift: TShiftState); override;
  private
    PreviousScreenLocation: TPoint;
    Anchor: TPoint;
    Offset: TPoint;
  end;

var
  Tools: array of Tool_;
  Tool: TTool;
  CurrentToolIndex: integer;

implementation

constructor TTool.Create(Scene: TCanvas);
begin
  FScene := Scene;
end;

procedure TTool.Start(Point: TPoint; MButton: TMouseButton);
begin { An empty method by default }
end;

procedure TTool.Continue(Point: TPoint; Shift: TShiftState);
begin { An empty method by default }
end;

procedure TTool.Stop;
begin { An empty method by default }
end;

procedure TPaintingTool.Start(Point: TPoint; MButton: TMouseButton);
begin
  Figure.Add(Zoom.ToGlobal(Point));
  History.Insert(Figure);
end;

procedure TPaintingTool.Continue(Point: TPoint; Shift: TShiftState);
begin
  Figure.Add(Zoom.ToGlobal(Point));
  History.ReplaceLast(Figure);
end;

constructor TPenTool.Create(Scene: TCanvas);
begin
  inherited Create(Scene);
  Figure := TPolyLine.Create(Scene, 'Pen');
end;

constructor TPolyLineTool.Create(Scene: TCanvas);
begin
  inherited Create(Scene);
  Figure := TPolyLine.Create(Scene, 'Poly line');
  History.Insert(Figure);
end;

procedure TPolyLineTool.Start(Point: TPoint; MButton: TMouseButton);
begin
  Figure.Add(Zoom.ToGlobal(Point));
  History.ReplaceLast(Figure);
end;

procedure TPolyLineTool.Continue(Point: TPoint; Shift: TShiftState);
begin { Empty method }
end;

constructor TLineTool.Create(Scene: TCanvas);
begin
  inherited Create(Scene);
  Figure := TLine.Create(Scene, 'Line');
end;

constructor TRectangleTool.Create(Scene: TCanvas);
begin
  inherited Create(Scene);
  Figure := TRectangle.Create(Scene, 'Rectangle');
end;

constructor TEllipseTool.Create(Scene: TCanvas);
begin
  inherited Create(Scene);
  Figure := TEllipse.Create(Scene, 'Ellipse');
end;

procedure TZoomTool.Start(Point: TPoint; MButton: TMouseButton);
begin
  Zoom.SetZoom(Point);
  if MButton = mbLeft then
    Zoom.ZoomIn;
  if MButton = mbRight then
    Zoom.ZoomOut;
end;

procedure THandTool.Start(Point: TPoint; MButton: TMouseButton);
begin
  PreviousScreenLocation := Zoom.GetPrevScreenLocation;
  Anchor := Point;
end;

procedure THandTool.Continue(Point: TPoint; Shift: TShiftState);
begin
  Offset := Zoom.ToGlobal(Anchor) - Zoom.ToGlobal(Point);
  Zoom.SetPrevScreenLocation(PreviousScreenLocation + Offset);
end;

procedure InitTool(Tool: ToolClass; IsRecreatingRequired: boolean; Name: string);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)].Tool := Tool;
  Tools[High(Tools)].Recreate := IsRecreatingRequired;
  Tools[High(Tools)].Name := Name;
end;

initialization

  InitTool(TPenTool, True,'Pen');
  InitTool(TPolyLineTool, False,'Poly line');
  InitTool(TLineTool, True,'Line');
  InitTool(TRectangleTool, True,'Rectangle');
  InitTool(TEllipseTool, True,'Ellipse');
  InitTool(TZoomTool, True,'Zoom');
  InitTool(THandTool, True,'Hand');

end.
