unit UScrollBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, Forms, Dialogs,
  UTools, UZoom, UPointUtils, UHistory, UCustomControls;

type

  TZoomScrollBar = class
  public
    constructor Create(AParent: TComponent);
    procedure Invalidate;
  private
    FWidth: integer;
    FHeight: integer;
    FHBar: TACustomSBar;
    FVBar: TACustomSBar;
    procedure OnScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

implementation

constructor TZoomScrollBar.Create(AParent: TComponent);
begin
  FWidth := TWinControl(AParent).Width;
  FHeight := TWinControl(AParent).Height;
  FHBar := TACustomSBar.Create(AParent, FHeight - 36, 1, FWidth -
    2, 15, 0, FWidth, FWidth, True);
  FHBar.OnScroll := @OnScrollEvent;
  FVBar := TACustomSBar.Create(AParent, 0, FWidth - 16, FHeight -
    35, 15, 0, FHeight, FHeight, True);
  FVBar.Kind := sbVertical;
  FVBar.OnScroll := @OnScrollEvent;
end;

procedure TZoomScrollBar.Invalidate;
begin
  FHBar.PageSize := FHBar.Max div (1 shl Zoom.GetZoomValue);
  FVBar.PageSize := FVBar.Max div (1 shl Zoom.GetZoomValue);
  FHBar.Position := Zoom.GetPrevScreenLocation.x;
  FVBar.Position := Zoom.GetPrevScreenLocation.y;
end;

procedure TZoomScrollBar.OnScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  Zoom.SetPrevScreenLocation(ToPoint(FHBar.Position, FVBar.Position));
  History.Show;
end;

end.
