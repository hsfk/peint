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
    FHBar: TACustomSBar;
    FVBar: TACustomSBar;
    procedure OnScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

implementation

constructor TZoomScrollBar.Create(AParent: TComponent);
var
  AWidth: integer;
  AHeight: integer;
begin
  AWidth := twincontrol(AParent).Width;
  AHeight := twincontrol(AParent).Height;
  FHBar := TACustomSBar.Create(AParent, AHeight - 36, 1, AWidth -
    2, 15, 0, AWidth, AWidth, True);
  FHBar.OnScroll := @OnScrollEvent;
  FVBar := TACustomSBar.Create(AParent, 0, AWidth - 16, AHeight -
    35, 15, 0, AHeight, AHeight, True);
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
  //with Zoom do begin
  //  PreviousX := FHBar.position;
  //  PreviousY := FVBar.position;
  //  ToolsDataUtils.ShowHistory(PreviousX, PreviousY);
  //end;
end;

end.
