unit UScrollBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, Forms, Dialogs, UZoom, UTools;

type

  TZoomScrollBar = class
  public
    constructor Create(parent_: TComponent);
    procedure Invalidate;
  private
    isVisible: boolean;
    HBar, VBar: TScrollBar;
    procedure OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

implementation

constructor TZoomScrollBar.Create(parent_: TComponent);
var
  w_width, w_height: integer;
begin
  w_width := twincontrol(parent_).Width;
  w_height := twincontrol(parent_).Height;
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

procedure TZoomScrollBar.Invalidate;
begin
  HBar.PageSize := HBar.Max div (1 shl Zoom.z_n);
  VBar.PageSize := VBar.Max div (1 shl Zoom.z_n);
  HBar.Position := Zoom.z_px;
  VBar.Position := Zoom.z_py;
end;

procedure TZoomScrollBar.OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  with zoom do begin
    z_px := Hbar.position;
    z_py := VBar.position;
    ToolsDataUtils.ShowHistory(z_px, z_py, z_n);
  end;
end;

end.
