unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs;

type

  TZoom = class
  public
    CurrentX: integer;
    CurrentY: integer;
    ZoomValue: integer;
    PreviousX: integer;
    PreviousY: integer;
    constructor Create(Parent_: TComponent);
    procedure CoordAllignment(var X1, X2, Y1, Y2: integer);
    procedure ZoomOut;
    procedure ZoomIn;
    procedure SetZoom(X, Y: integer);
    function GetGlobalX(LocalX: integer): integer;
    function GetGlobalY(LocalY: integer): integer;
    function GetZoomValue: integer;
  private
    Width: integer;
    Height: integer;
  end;

var
  Zoom: TZoom;

implementation

constructor TZoom.Create(Parent_: TComponent);
begin
  CurrentX := 0;
  CurrentY := 0;
  PreviousX := 0;
  PreviousY := 0;
  ZoomValue := 0;
  Width := twincontrol(Parent_).Width;
  Height := twincontrol(Parent_).Height;
end;

procedure TZoom.CoordAllignment(var X1, X2, Y1, Y2: integer);
begin
  if Zoom.ZoomValue = 0 then begin
    X1 -= Zoom.PreviousX;
    X2 -= Zoom.PreviousX;
    Y1 -= Zoom.PreviousY;
    Y2 -= Zoom.PreviousY;
  end;
end;

procedure TZoom.ZoomOut;
begin
  CurrentX := CurrentX div (1 shl (ZoomValue));
  CurrentY := CurrentY div (1 shl (ZoomValue));
  PreviousX := PreviousX - CurrentX;
  PreviousY := PreviousY - CurrentY;
  if ZoomValue > 0 then
    ZoomValue -= 1;
  if PreviousX < 0 then
    PreviousX := 0;
  if PreviousY < 0 then
    PreviousY := 0;
  if ZoomValue = 0 then begin
    PreviousX := 0;
    PreviousY := 0;
  end;
end;

procedure TZoom.ZoomIn;
begin
  CurrentX := CurrentX - Width div 4;
  CurrentY := CurrentY - Height div 4;
  if CurrentX < 0 then
    CurrentX := 0;
  if CurrentY < 0 then
    CurrentY := 0;
  if ZoomValue < 8 then
    ZoomValue += 1;
  CurrentX := PreviousX + CurrentX div (1 shl (ZoomValue - 1));
  CurrentY := PreviousY + CurrentY div (1 shl (ZoomValue - 1));
  PreviousX := CurrentX;
  PreviousY := CurrentY;
end;

procedure TZoom.SetZoom(X, Y: integer);
begin
  CurrentX := X;
  CurrentY := Y;
end;

function TZoom.GetGlobalX(LocalX: integer): integer;
begin
  Result := PreviousX + LocalX div (1 shl (ZoomValue));
end;

function TZoom.GetGlobalY(LocalY: integer): integer;
begin
  Result := PreviousY + LocalY div (1 shl (ZoomValue));
end;

function TZoom.GetZoomValue: integer;
begin
  Result := ZoomValue;
end;

end.
