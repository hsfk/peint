unit UFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UZoom, UPointUtils, UMainSceneUtils;

type

  TFigure = class
  public
    constructor Create(Scene: TCanvas; ToolName: string);
    procedure Draw; virtual; abstract;
    procedure HighLight;
    procedure Add(Point: TPoint); virtual; abstract;
    function CopyFigure(Figure: TFigure): TFigure;
  private
    FName: string;
    FScene: TCanvas;
    FPoints: TPoints;
    FMinPoint: TPoint; { Кеширование мин\макс точек для быстрого подсвечивания }
    FMaxPoint: TPoint;
    FPenWidth: integer;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    procedure SaveToolState;
    procedure LoadToolState;
  published
    property Name: string read FName write FName;
    property PenWidth: integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
  end;

  TTwoPointFigure = class(TFigure)
  public
    procedure Add(Point: TPoint); override;
  end;

  TPolyLine = class(TFigure)
  public
    procedure Draw; override;
    procedure Add(Point: TPoint); override;
  end;

  TLine = class(TTwoPointFigure)
  public
    procedure Draw; override;
  end;

  TRectangle = class(TTwoPointFigure)
  public
    procedure Draw; override;
  end;

  TEllipse = class(TTwoPointFigure)
  public
    procedure Draw; override;
  end;

implementation

constructor TFigure.Create(Scene: TCanvas; ToolName: string);
begin
  FScene := Scene;
  FName := ToolName;
  FMinPoint := max;
  FMaxPoint := min;
end;

procedure TFigure.HighLight;
begin
  MainSceneUtils.SaveToolState;
  FScene.Pen.Color := clRed;
  FScene.Pen.Width := 2;
  FScene.Brush.Style := bsClear;
  FScene.Rectangle(
    Zoom.ToScene(FMinPoint - 3).x,
    Zoom.ToScene(FMinPoint - 3).y,
    Zoom.ToScene(FMaxPoint + 3).x,
    Zoom.ToScene(FMaxPoint + 3).y);
  MainSceneUtils.LoadToolState;
end;

function TFigure.CopyFigure(Figure: TFigure): TFigure;
var ATmpPoint : TPoint;
    BTmpPoint : TPoint;
begin
  ATmpPoint := Figure.FMinPoint;
  BTmpPoint := Figure.FMaxPoint;
  Result := Figure.Create(FScene, Figure.Name);
 // Result.FPoints := Copy(Figure.FPoints, 0, Length(Figure.FPoints));
  Result.FMinPoint := ATmpPoint;
  Result.FMaxPoint := BTmpPoint;
  Result.PenWidth := Figure.FPenWidth;
  Result.PenStyle := Figure.FPenStyle;
  Result.PenColor := Figure.FPenColor;
  Result.BrushStyle := Figure.FBrushStyle;
  Result.BrushColor := Figure.FBrushColor;
end;

procedure TFigure.SaveToolState;
begin
  FPenWidth := FScene.Pen.Width;
  FPenStyle := FScene.Pen.Style;
  FPenColor := FScene.Pen.Color;
  FBrushStyle := FScene.Brush.Style;
  FBrushColor := FScene.Brush.Color;
end;

procedure TFigure.LoadToolState;
begin
  FScene.Pen.Width := FPenWidth;
  FScene.Pen.Color := FPenColor;
  FScene.Pen.Style := FPenStyle;
  FScene.Brush.Color := FBrushColor;
  FScene.Brush.Style := FBrushStyle;
end;

procedure TTwoPointFigure.Add(Point: TPoint);
begin
  if Length(FPoints) = 0 then begin
    SetLength(FPoints, 2);
    FPoints[0] := Point;
    FMinPoint := Point;
    SaveToolState;
  end;
  FPoints[1] := Point;
  FMaxPoint := Point;
end;

procedure TPolyLine.Draw;
begin
  LoadToolState;
  FScene.Polyline(Zoom.ToScene(FPoints));
end;

procedure TPolyLine.Add(Point: TPoint);
begin
  if Length(FPoints) = 0 then
    SaveToolState;
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := Point;
  SaveMin(FMinPoint, Point);
  SaveMax(FMaxPoint, Point);
end;

procedure TLine.Draw;
begin
  LoadToolState;
  FScene.Line(Zoom.ToScene(FPoints[0]), Zoom.ToScene(FPoints[1]));
end;

procedure TRectangle.Draw;
begin
  LoadToolState;
  FScene.Rectangle(
    Zoom.ToScene(FPoints[0]).x,
    Zoom.ToScene(FPoints[0]).y,
    Zoom.ToScene(FPoints[1]).x,
    Zoom.ToScene(FPoints[1]).y);
end;

procedure TEllipse.Draw;
begin
  LoadToolState;
  FScene.Ellipse(
    Zoom.ToScene(FPoints[0]).x,
    Zoom.ToScene(FPoints[0]).y,
    Zoom.ToScene(FPoints[1]).x,
    Zoom.ToScene(FPoints[1]).y);
end;


end.
