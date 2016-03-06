unit UFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UZoom, UPointUtils;

type

  TFigure = class
  public
    constructor Create(Scene: TCanvas);
    procedure Draw; virtual; abstract;
    procedure Add(Point: TPoint); virtual; abstract;
    function CopyFigure(Figure: TFigure): TFigure;
  private
    FScene: TCanvas;
    FPoints: TPoints;
    FPenWidth: integer;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    procedure InitFirstTwoPoints(Point: TPoint);
    procedure SaveToolState;
    procedure LoadToolState;
  published
    property PenWidth: integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenColor: TColor read FPenColor write FPenColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushColor: TColor read FBrushColor write FBrushColor;
  end;

  TPolyLine = class(TFigure)
  public
    procedure Draw; override;
    procedure Add(Point: TPoint); override;
  end;

  TLine = class(TFigure)
  public
    procedure Draw; override;
    procedure Add(Point: TPoint); override;
  end;

  TRectangle = class(TFigure)
  public
    procedure Draw; override;
    procedure Add(Point: TPoint); override;
  end;

  TEllipse = class(TFigure)
  public
    procedure Draw; override;
    procedure Add(Point: TPoint); override;
  end;

implementation

constructor TFigure.Create(Scene: TCanvas);
begin
  FScene := Scene;
end;

function TFigure.CopyFigure(Figure: TFigure): TFigure;
begin
  Result := Figure.Create(FScene);
  Result.FPoints := Copy(Figure.FPoints, 0, Length(Figure.FPoints));
  Result.PenWidth := Figure.FPenWidth;
  Result.PenStyle := Figure.FPenStyle;
  Result.PenColor := Figure.FPenColor;
  Result.BrushStyle := Figure.FBrushStyle;
  Result.BrushColor := Figure.FBrushColor;
end;

procedure TFigure.InitFirstTwoPoints(Point: TPoint);
begin
  SetLength(FPoints, 2);
  FPoints[0] := Point;
  SaveToolState;
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
end;

procedure TLine.Draw;
begin
  LoadToolState;
  FScene.Line(Zoom.ToScene(FPoints[0]), Zoom.ToScene(FPoints[1]));
end;

procedure TLine.Add(Point: TPoint);
begin
  if Length(FPoints) = 0 then
    InitFirstTwoPoints(Point);
  FPoints[1] := Point;
end;

procedure TRectangle.Add(Point: TPoint);
begin
  if Length(FPoints) = 0 then
   InitFirstTwoPoints(Point);
  FPoints[1] := Point;
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

procedure TEllipse.Add(Point: TPoint);
begin
    if Length(FPoints) = 0 then
   InitFirstTwoPoints(Point);
  FPoints[1] := Point;
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
