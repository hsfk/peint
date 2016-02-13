unit UObjectMove;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls;

type
  TObjectMove = class
    constructor Create(border_width, border_height: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);
  private
    isMoving: boolean;
    px, py, b_width, b_height: integer;
    procedure ObjectMove(Obj: Twincontrol;x, y: integer);
  end;

implementation

constructor TObjectMove.Create(border_width, border_height: integer);
begin
  isMoving := False;
  b_width := border_width;
  b_height := border_height;
end;

procedure TObjectMove.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  isMoving := True;
  px := x;
  py := y;
end;

procedure TObjectMove.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if isMoving = True then begin
    ObjectMove(twincontrol(Sender),x, y);
  end;
end;

procedure TObjectMove.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  isMoving := False;
end;

procedure TObjectMove.ObjectMove(Obj: Twincontrol;x, y: integer);
var
  newx, newy: integer;
begin
  newx := obj.Left + x - px;
  newy := obj.top + y - py;
  if (newx) >= (b_width - obj.Width) then
    obj.Left := b_width - obj.Width
  else if (newx) <= 0 then
    obj.left := 0
  else
    obj.Left := newx;
  if (newy) >= (b_height - obj.Height - 20) then
    obj.top := b_height - obj.Height - 20
  else if (newy) <= 0 then
    obj.top := 0
  else
    obj.top := newy;
end;

end.
