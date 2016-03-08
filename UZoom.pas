unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, UPointUtils, ExtCtrls;

type

  TZoom = class
  public
    constructor Create(Parent: TComponent; MainScene: TPaintBox);
    procedure Update;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure SetZoom(Point: TPoint);
    function GetPrevScreenLocation: TPoint;
    procedure SetPrevScreenLocation(Point: TPoint);
    function ToGlobal(LocalPoint: TPoint): TPoint;
    function ToScene(Points: TPoints): TPoints;
    function ToScene(Point: TPoint): TPoint; overload;
    procedure MainSceneFullScreen;
    procedure MainSceneToDefault;
  private
    FParent: TComponent;
    FDefWidth: integer;
    FDefHeight: integer;
    FMainScene: TPaintBox;
    FZoomValue: integer;
    FPrevScreenLocation: TPoint;
    FCurrentScreenLocation: TPoint;
    FWidth: integer;
    FHeight: integer;
    procedure RangeCheck(var Point: Tpoint);
  published
    property Value: integer read FZoomValue;
  end;

var
  Zoom: TZoom;

implementation

constructor TZoom.Create(Parent: TComponent; MainScene: TPaintBox);
begin
  FParent := Parent;
  FMainScene := MainScene;
  FDefWidth := FMainScene.Width;
  FDefHeight := FMainScene.Height;
  FZoomValue := 0;
  FPrevScreenLocation := null;
  Update;
end;

procedure TZoom.Update;
begin
  FWidth := FMainScene.Width;
  FHeight := FMainScene.Height;
end;

procedure TZoom.ZoomIn;
begin
  MainSceneFullScreen;
  FCurrentScreenLocation.x := FCurrentScreenLocation.x - FWidth div 4;
  FCurrentScreenLocation.y := FCurrentScreenLocation.y - FHeight div 4;
  RangeCheck(FCurrentScreenLocation);
  FCurrentScreenLocation := ToGlobal(FCurrentScreenLocation);
  FPrevScreenLocation := FCurrentScreenLocation;
  if FZoomValue < 8 then
    FZoomValue += 1;
end;

procedure TZoom.ZoomOut;
begin
  FCurrentScreenLocation := FCurrentScreenLocation / (1 shl FZoomValue);
  FPrevScreenLocation := FPrevScreenLocation - FCurrentScreenLocation;
  RangeCheck(FPrevScreenLocation);
  if FZoomValue > 0 then
    FZoomValue -= 1;
  if FZoomValue = 0 then begin
    FPrevScreenLocation := null;
    MainSceneToDefault;
  end;
end;

procedure TZoom.SetZoom(Point: TPoint);
begin
  FCurrentScreenLocation := Point;
end;

function TZoom.GetPrevScreenLocation: TPoint;
begin
  Result := FPrevScreenLocation;
end;

procedure TZoom.SetPrevScreenLocation(Point: TPoint);
begin
  FPrevScreenLocation := Point;
end;

function TZoom.ToGlobal(LocalPoint: TPoint): TPoint;
begin
  Result := FPrevScreenLocation + LocalPoint / (1 shl FZoomValue);
end;

function TZoom.ToScene(Points: TPoints): TPoints;
var
  i: integer;
begin
  SetLength(Result, Length(Points));
  for i := 0 to Length(Points) - 1 do
    Result[i] := ToScene(Points[i]);
end;

function TZoom.ToScene(Point: TPoint): TPoint;
begin
  Result := (Point - FPrevScreenLocation) * (1 shl FZoomValue);
end;

procedure TZoom.MainSceneFullScreen;
begin
  FMainScene.Width := TWinControl(FParent).Width;
  FMainScene.Height := TWinControl(FParent).Height;
  FMainScene.Top := 0;
  FMainScene.Left := 0;
  Update;
end;

procedure TZoom.MainSceneToDefault;
begin
  FMainScene.Width := FDefWidth;
  FMainScene.Height := FDefHeight;
  FMainScene.Top := TWinControl(FParent).Height div 2 - FDefHeight div 2;
  FMainScene.Left := TWinControl(FParent).Width div 2 - FDefWidth div 2;
  Update;
end;

procedure TZoom.RangeCheck(var Point: Tpoint);
begin
  //if Point.x < 0 then
  //  Point.x := 0;
  //if Point.y < 0 then
  //  Point.y := 0;
end;

end.
