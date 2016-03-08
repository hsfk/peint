unit UScrollBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Forms, UCustomControls, Controls,
  UZoom, UPointUtils, UHistory, ExtCtrls;

type
  TZoomScrollBar = class
  public
    constructor Create(AParent: TComponent; MainScene: TPaintBox);
    procedure Update;
  private
    FMainScene: TPaintBox;
    FWidth: integer;
    FHeight: integer;
    FHBar: TACustomSBar;
    FVBar: TACustomSBar;
    procedure OnScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

var
  ScrollBar: TZoomScrollBar;

implementation

constructor TZoomScrollBar.Create(AParent: TComponent; MainScene: TPaintBox);
begin
  FMainScene := MainScene;
  FWidth := TWinControl(AParent).Width;
  FHeight := TWinControl(AParent).Height;
  FHBar := TACustomSBar.Create(AParent, FHeight - 15, 1, FWidth -
    2, 15, 0, FWidth, FWidth, True);
  FHBar.OnScroll := @OnScrollEvent;
  FVBar := TACustomSBar.Create(AParent, 0, FWidth - 16, FHeight -
    15, 15, 0, FHeight, FHeight, True);
  FVBar.Kind := sbVertical;
  FVBar.OnScroll := @OnScrollEvent;
  FHBar.Anchors := [akLeft, akRight, akBottom];
  FVBar.Anchors := [akRight, akTop, akBottom];
end;

procedure TZoomScrollBar.Update;
begin
  FHBar.PageSize := FHBar.Max div (1 shl Zoom.Value);
  FVBar.PageSize := FVBar.Max div (1 shl Zoom.Value);
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
