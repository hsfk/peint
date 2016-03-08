unit UFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UZoom, UPointUtils, USceneUtils;

type

  TFigure = class
  public
    constructor Create(Scene: TCanvas; ToolName: string);
    procedure Draw(Scene: TCanvas); virtual; abstract;
    procedure HighLight;
    procedure Add(Point: TPoint); virtual; abstract;
    function CopyFigure(Figure: TFigure): TFigure;

    function GetMaxPoint: TPoint;
    function GetMinPoint: TPoint;
    function GetPoint(Index: integer): TPoint;
  private
    FName: string;
    FMainScene: TCanvas;
    FPoints: TPoints;
    FMinPoint: TPoint; { Кеширование мин\макс точек для быстрого подсвечивания }
    FMaxPoint: TPoint;
    FPenWidth: integer;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    procedure SaveToolState;
    procedure LoadToolState(Scene: TCanvas);
    function GetDataLength: longword;
  published
    property Name: string read FName write FName;
    property PenWidth: integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property DataLength: longword read GetDataLength;
  end;

  TTwoPointFigure = class(TFigure)
  public
    procedure Add(Point: TPoint); override;
  end;

  TPolyLine = class(TFigure)
  public
    procedure Draw(Scene: TCanvas); override;
    procedure Add(Point: TPoint); override;
  end;

  TLine = class(TTwoPointFigure)
  public
    procedure Draw(Scene: TCanvas); override;
  end;

  TRectangle = class(TTwoPointFigure)
  public
    procedure Draw(Scene: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
  public
    procedure Draw(Scene: TCanvas); override;
  end;

implementation

constructor TFigure.Create(Scene: TCanvas; ToolName: string);
begin
  FMainScene := Scene;
  FName := ToolName;
  FMinPoint := max;
  FMaxPoint := min;
end;

procedure TFigure.HighLight;
begin
  SceneUtils.SaveToolState;
  SceneUtils.SetToolState(FMainScene, 2, clRed, psSolid, clWhite, bsClear);
  FMainScene.Rectangle(
    Zoom.ToScene(FMinPoint - PenWidth div 2).x,
    Zoom.ToScene(FMinPoint - PenWidth div 2).y,
    Zoom.ToScene(FMaxPoint + PenWidth div 2).x,
    Zoom.ToScene(FMaxPoint + PenWidth div 2).y);
  SceneUtils.LoadToolState;
end;

function TFigure.CopyFigure(Figure: TFigure): TFigure;
var
  ATmpPoint: TPoint;
  BTmpPoint: TPoint;
begin
  ATmpPoint := Figure.FMinPoint;
  BTmpPoint := Figure.FMaxPoint;
  Result := Figure.Create(FMainScene, Figure.Name);
  Result.FMinPoint := ATmpPoint;
  Result.FMaxPoint := BTmpPoint;
  Result.PenWidth := Figure.FPenWidth;
  Result.PenStyle := Figure.FPenStyle;
  Result.PenColor := Figure.FPenColor;
  Result.BrushStyle := Figure.FBrushStyle;
  Result.BrushColor := Figure.FBrushColor;
end;

function TFigure.GetMinPoint: TPoint;
begin
  Result := FMinPoint;
end;

function TFigure.GetMaxPoint: TPoint;
begin
  Result := FMaxPoint;
end;

function TFigure.GetPoint(Index: integer): TPoint;
begin
  Result := FPoints[Index];
end;

procedure TFigure.SaveToolState;
begin
  FPenWidth := FMainScene.Pen.Width;
  FPenStyle := FMainScene.Pen.Style;
  FPenColor := FMainScene.Pen.Color;
  FBrushStyle := FMainScene.Brush.Style;
  FBrushColor := FMainScene.Brush.Color;
end;

procedure TFigure.LoadToolState(Scene: TCanvas);
begin
  SceneUtils.SetToolState(Scene, FPenWidth, FPenColor, FPenStyle,
    FBrushColor, FBrushStyle);
end;

function TFigure.GetDataLength: longword;
begin
  Result := Length(FPoints);
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

procedure TPolyLine.Draw(Scene: TCanvas);
begin
  LoadToolState(Scene);
  Scene.Polyline(Zoom.ToScene(FPoints));
end;

procedure TPolyLine.Add(Point: TPoint);
begin
  SaveToolState;
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := Point;
  SaveMin(FMinPoint, Point);
  SaveMax(FMaxPoint, Point);
end;

procedure TLine.Draw(Scene: TCanvas);
begin
  LoadToolState(Scene);
  Scene.Line(Zoom.ToScene(FPoints[0]), Zoom.ToScene(FPoints[1]));
end;

procedure TRectangle.Draw(Scene: TCanvas);
begin
  LoadToolState(Scene);
  Scene.Rectangle(
    Zoom.ToScene(FPoints[0]).x,
    Zoom.ToScene(FPoints[0]).y,
    Zoom.ToScene(FPoints[1]).x,
    Zoom.ToScene(FPoints[1]).y);
end;

procedure TEllipse.Draw(Scene: TCanvas);
begin
  LoadToolState(Scene);
  Scene.Ellipse(
    Zoom.ToScene(FPoints[0]).x,
    Zoom.ToScene(FPoints[0]).y,
    Zoom.ToScene(FPoints[1]).x,
    Zoom.ToScene(FPoints[1]).y);
end;


end.
