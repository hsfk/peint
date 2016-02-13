unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, Forms, Dialogs, UHistory;

type

  TZoomSBar = class
  public
    constructor Create(parent_: TComponent);
    procedure Invalidate;
  private
    isVisible: boolean;
    HBar, VBar: TScrollBar;
    procedure OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

  TZoom = class
  public
    z_x, z_y, z_n, z_px, z_py, cx, cy: integer;
    constructor Create(parent_ : TComponent;var SomeCanvas: tbitmap);
    procedure ZoomOut(x,y : integer);
    procedure ZoomIn(x,y : integer);
    procedure SetZoom(x, y: integer);
    function GetGlobalX(loc_x: integer): integer;
    function GetGlobalY(loc_y: integer): integer;
    function GetZoomValue: integer;
  private
    FCanvas: tbitmap;
    ScrollBar: TZoomSBar;
  end;

  var
  Zoom: TZoom;

implementation

constructor TZoom.Create(parent_ : TComponent;var SomeCanvas: tbitmap);
begin
  FCanvas := SomeCanvas;
  z_x := 0;
  z_y := 0;
  z_px := 0;
  z_py := 0;
  z_n := 0;
  ScrollBar := TZoomSBar.Create(parent_);
end;

procedure TZoom.ZoomOut(x,y : integer);
begin
  SetZoom(x,y)  ;
  cx := z_px + cx div (1 shl (z_n + 1));
  cy := z_py + cy div (1 shl (z_n + 1));
  if z_n > 0 then
    z_n -= 1;
  z_px := cx - (FCanvas.Width div 4) div (1 shl z_n);
  z_py := cy - (FCanvas.Height div 4) div (1 shl z_n);
  if z_px < 0 then
    z_px := 0;
  if z_py < 0 then
    z_py := 0;
  if z_n = 0 then begin
    z_px := 0;
    z_py := 0;
  end;
  ScrollBar.Invalidate;
  History.Show(z_px, z_py, z_n);
end;

procedure TZoom.ZoomIn(x,y : integer);
begin
    SetZoom(x,y)  ;
  z_x := cx - FCanvas.Width div 4;
  z_y := cy - FCanvas.Height div 4;
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
  ScrollBar.Invalidate;
  History.Show(z_x, z_y, z_n);
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

constructor TZoomSBar.Create(parent_: TComponent);
var w_width,w_height : integer;
begin
  w_width := twincontrol(parent_).width;
  w_height := twincontrol(parent_).height;
  isVisible := True;
  HBar := TScrollBar.Create(parent_);
  with HBar do begin
    Visible := isVisible;
    Width := w_width - 2;
    Height := 15;
    top := w_height - 20 - 15 - 1;
    left := 1;
    parent := twincontrol(parent_);
    max := w_width;
    PageSize := w_width;
    OnScroll := @OnChangeEvent;
  end;
  VBar := TScrollBar.Create(parent_);
  with VBar do begin
    Visible := isVisible;
    Kind := sbVertical;
    Width := 15;
    Height := w_height - 20 - 15;
    top := 0;
    left := w_width - 15 - 1;
    parent := twincontrol(parent_);
    max := w_height;
    PageSize := w_height;
    OnScroll := @OnChangeEvent;
  end;
end;

procedure TZoomSBar.Invalidate;
begin
  HBar.PageSize := HBar.Max div (1 shl Zoom.z_n);
  VBar.PageSize := VBar.Max div (1 shl Zoom.z_n);
  HBar.Position := Zoom.z_px;
  VBar.Position := Zoom.z_py;
end;

procedure TZoomSBar.OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  with zoom do begin
    z_px := Hbar.position;
    z_py := VBar.position;
    History.Show(z_px, z_py, zoom.z_n);
  end;
end;

end.
