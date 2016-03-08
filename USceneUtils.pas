unit USceneUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TSceneUtils = class
  public
    constructor Create(MainScene: TCanvas);
    procedure SetToolState(Scene: TCanvas; PWidth: integer; PColor: TColor;
      PStyle: TPenStyle; BColor: TColor; BStyle: TBrushStyle);
    procedure ClearMainScene;
    procedure ClearScene(Scene : TCanvas);
    procedure SaveToolState;
    procedure LoadToolState;
  private
    FMainScene: TCanvas;
    FPrevPWidth: integer;
    FPrevPColor: TColor;
    FPrevPStyle: TPenStyle;
    FPrevBColor: TColor;
    FPrevBStyle: TBrushStyle;
  end;

var
  SceneUtils: TSceneUtils;

implementation

constructor TSceneUtils.Create(MainScene: TCanvas);
begin
  FMainScene := MainScene;
end;

procedure TSceneUtils.SetToolState(Scene: TCanvas; PWidth: integer;
  PColor: TColor; PStyle: TPenStyle; BColor: TColor; BStyle: TBrushStyle);
begin
  Scene.Pen.Width := PWidth;
  Scene.Pen.Color := PColor;
  Scene.Pen.Style := PStyle;
  Scene.Brush.Color := BColor;
  Scene.Brush.Style := BStyle;
end;

procedure TSceneUtils.ClearMainScene;
begin
  SaveToolState;
  ClearScene(FMainScene);
  LoadToolState;
end;

procedure TSceneUtils.ClearScene(Scene : TCanvas);
begin
  SetToolState(Scene, 1, clWhite, psSolid, clWhite, bsSolid);
  FMainScene.Rectangle(0, 0, Scene.Width, Scene.Height);
end;

procedure TSceneUtils.SaveToolState;
begin
  FPrevPWidth := FMainScene.Pen.Width;
  FPrevPColor := FMainScene.Pen.Color;
  FPrevPStyle := FMainScene.Pen.Style;
  FPrevBColor := FMainScene.Brush.Color;
  FPrevBStyle := FMainScene.Brush.Style;
end;

procedure TSceneUtils.LoadToolState;
begin
  FMainScene.Pen.Width := FPrevPWidth;
  FMainScene.Pen.Color := FPrevPColor;
  FMainScene.Pen.Style := FPrevPStyle;
  FMainScene.Brush.Color := FPrevBColor;
  FMainScene.Brush.Style := FPrevBStyle;
end;

end.
