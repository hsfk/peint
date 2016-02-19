unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs;

type

  TZoom = class
  public
    z_x, z_y, z_n, z_px, z_py, cx, cy: integer;
    constructor Create(parent_: TComponent);
    procedure ZoomOut(x, y: integer);
    procedure ZoomIn(x, y: integer);
    procedure SetZoom(x, y: integer);
    function GetGlobalX(loc_x: integer): integer;
    function GetGlobalY(loc_y: integer): integer;
    function GetZoomValue: integer;
  private
    Width, Height: integer;
  end;

var
  Zoom: TZoom;

implementation

constructor TZoom.Create(parent_: TComponent);
begin
  z_x := 0;
  z_y := 0;
  z_px := 0;
  z_py := 0;
  z_n := 0;
  Width := twincontrol(parent_).Width;
  Height := twincontrol(parent_).Height;
end;

procedure TZoom.ZoomOut(x, y: integer);
begin
  SetZoom(x, y);
  cx := z_px + cx div (1 shl (z_n + 1));
  cy := z_py + cy div (1 shl (z_n + 1));
  if z_n > 0 then
    z_n -= 1;
  z_px := cx - (Width div 4) div (1 shl z_n);
  z_py := cy - (Height div 4) div (1 shl z_n);
  if z_px < 0 then
    z_px := 0;
  if z_py < 0 then
    z_py := 0;
  if z_n = 0 then begin
    z_px := 0;
    z_py := 0;
  end;
end;

procedure TZoom.ZoomIn(x, y: integer);
begin
  SetZoom(x, y);
  z_x := cx - Width div 4;
  z_y := cy - Height div 4;
  if z_x < 0 then
    z_x := 0;
  if z_y < 0 then
    z_y := 0;
  if z_n < 8 then
    z_n += 1;
  z_x := z_px + z_x div (1 shl (z_n - 1));
  z_y := z_py + z_y div (1 shl (z_n - 1));
  z_px := z_x;
  z_py := z_y;
end;

procedure TZoom.SetZoom(x, y: integer);
begin
  cx := x;
  cy := y;
end;

function TZoom.GetGlobalX(loc_x: integer): integer;
begin
  Result := z_px + loc_x div (1 shl (z_n));
end;

function TZoom.GetGlobalY(loc_y: integer): integer;
begin
  Result := z_py + loc_y div (1 shl (z_n));
end;

function TZoom.GetZoomValue: integer;
begin
  Result := z_n;
end;

end.
