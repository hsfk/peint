unit UMainSceneUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TMainSceneUtils = class
  public
    constructor Create(Scene: TCanvas);
    procedure SetToolState(PWidth: integer; PColor: TColor; PStyle: TPenStyle;
      BColor: TColor; BStyle: TBrushStyle);
    procedure ClearScene;
    procedure SaveToolState;
    procedure LoadToolState;
  private
    FScene: TCanvas;
    FPrevPWidth: integer;
    FPrevPColor: TColor;
    FPrevPStyle: TPenStyle;
    FPrevBColor: TColor;
    FPrevBStyle: TBrushStyle;
  end;

var
  MainSceneUtils: TMainSceneUtils;

implementation

constructor TMainSceneUtils.Create(Scene: TCanvas);
begin
  FScene := Scene;
end;

procedure TMainSceneUtils.SetToolState(PWidth: integer; PColor: TColor;
  PStyle: TPenStyle; BColor: TColor; BStyle: TBrushStyle);
begin
  FScene.Pen.Width := PWidth;
  FScene.Pen.Color := PColor;
  FScene.Pen.Style := PStyle;
  FScene.Brush.Color := BColor;
  FScene.Brush.Style := BStyle;
end;

procedure TMainSceneUtils.ClearScene;
begin
  SaveToolState;
  SetToolState(1, clWhite,psSolid,clWhite,bsSolid);
  FScene.Rectangle(0, 0, FScene.Width, FScene.Height);
  LoadToolState;
end;

procedure TMainSceneUtils.SaveToolState;
begin
  FPrevPWidth := FScene.Pen.Width;
  FPrevPColor := FScene.Pen.Color;
  FPrevPStyle := FScene.Pen.Style;
  FPrevBColor := FScene.Brush.Color;
  FPrevBStyle := FSCene.Brush.Style;
end;

procedure TMainSceneUtils.LoadToolState;
begin
  FScene.Pen.Width := FPrevPWidth;
  FScene.Pen.Color := FPrevPColor;
  FScene.Pen.Style := FPrevPStyle;
  FScene.Brush.Color := FPrevBColor;
  FScene.Brush.Style := FPrevBStyle;
end;

end.
