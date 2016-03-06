unit UScrollBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, Forms, Dialogs, UTools;

type

  TZoomScrollBar = class
  public
    constructor Create(Parent_: TComponent);
    procedure Invalidate;
  private
    isVisible: boolean;
    HorizontalBar: TScrollBar;
    VerticalBar: TScrollBar;
    procedure OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

implementation

constructor TZoomScrollBar.Create(Parent_: TComponent);
var
  Width_: integer;
  Height_: integer;
begin
  Width_ := twincontrol(Parent_).Width;
  Height_ := twincontrol(Parent_).Height;
  isVisible := True;
  HorizontalBar := TScrollBar.Create(Parent_);
  with HorizontalBar do begin
    Visible := isVisible;
    Width := Width_ - 2;
    Height := 15;
    Top := Height_ - 20 - 15 - 1;
    Left := 1;
    Parent := twincontrol(Parent_);
    Max := Width_;
    PageSize := Width_;
    OnScroll := @OnChangeEvent;
  end;
  VerticalBar := TScrollBar.Create(Parent_);
  with VerticalBar do begin
    Visible := isVisible;
    Kind := sbVertical;
    Width := 15;
    Height := Height_ - 20 - 15;
    Top := 0;
    Left := Width_ - 15 - 1;
    Parent := twincontrol(Parent_);
    Max := Height_;
    PageSize := Height_;
    OnScroll := @OnChangeEvent;
  end;
end;

procedure TZoomScrollBar.Invalidate;
begin
  //with Zoom do begin
  //  HorizontalBar.PageSize := HorizontalBar.Max div (1 shl ZoomValue);
  //  VerticalBar.PageSize := VerticalBar.Max div (1 shl ZoomValue);
  //  HorizontalBar.Position := PreviousX;
  //  VerticalBar.Position := PreviousY;
  //end;
end;

procedure TZoomScrollBar.OnChangeEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  //with Zoom do begin
  //  PreviousX := HorizontalBar.position;
  //  PreviousY := VerticalBar.position;
  //  ToolsDataUtils.ShowHistory(PreviousX, PreviousY);
  //end;
end;

end.
