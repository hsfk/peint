unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, UPointUtils;

type
  TZoom = class
  public
    constructor Create(Parent: TComponent);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure SetZoom(Point: TPoint);
    function GetPrevScreenLocation: TPoint;
    procedure SetPrevScreenLocation(Point : TPoint);
    function ToGlobal(LocalPoint: TPoint): TPoint;
    function ToScene(Points: TPoints): TPoints;
    function ToScene(Point: TPoint): TPoint; overload;
  private
    ZoomValue: integer;
    PrevScreenLocation: TPoint;
    CurrentScreenLocation: TPoint;
    FWidth: integer;
    FHeight: integer;
    procedure RangeCheck(var Point: Tpoint);
  published
    property GetZoomValue: integer read ZoomValue;
  end;

var
  Zoom: TZoom;

implementation

constructor TZoom.Create(Parent: TComponent);
begin
  ZoomValue := 0;
  PrevScreenLocation := null;
  FWidth := twincontrol(Parent).Width;
  FHeight := twincontrol(Parent).Height;
end;

procedure TZoom.ZoomIn;
begin
  CurrentScreenLocation.x := CurrentScreenLocation.x - FWidth div 4;
  CurrentScreenLocation.y := CurrentScreenLocation.y - FHeight div 4;
  RangeCheck(CurrentScreenLocation);
  CurrentScreenLocation := ToGlobal(CurrentScreenLocation);
  PrevScreenLocation := CurrentScreenLocation;
  if ZoomValue < 8 then
    ZoomValue += 1;
end;

procedure TZoom.ZoomOut;
begin
  CurrentScreenLocation := CurrentScreenLocation / (1 shl ZoomValue);
  PrevScreenLocation := PrevScreenLocation - CurrentScreenLocation;
  RangeCheck(PrevScreenLocation);
  if ZoomValue > 0 then
    ZoomValue -= 1;
  if ZoomValue = 0 then
    PrevScreenLocation := null;
end;

procedure TZoom.SetZoom(Point: TPoint);
begin
  CurrentScreenLocation := Point;
end;

function TZoom.GetPrevScreenLocation: TPoint;
begin
  Result := PrevScreenLocation;
end;

procedure TZoom.SetPrevScreenLocation(Point : TPoint);
begin
  PrevScreenLocation := Point;
end;

function TZoom.ToGlobal(LocalPoint: TPoint): TPoint;
begin
  Result := PrevScreenLocation + LocalPoint / (1 shl ZoomValue);
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
  Result := (Point - Zoom.PrevScreenLocation) * (1 shl ZoomValue);
end;

procedure TZoom.RangeCheck(var Point: Tpoint);
begin
  if Point.x < 0 then
    Point.x := 0;
  if Point.y < 0 then
    Point.y := 0;
end;

end.
