unit UObjectMove;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls;

type
  TObjectMove = class
    constructor Create(BorderWidth_, BorderHeight_: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    isMoving: boolean;
    PreviousX: integer;
    PreviousY: integer;
    BorderWidth: integer;
    BorderHeight: integer;
    procedure ObjectMove(Object_: Twincontrol; X, Y: integer);
  end;

implementation

constructor TObjectMove.Create(BorderWidth_, BorderHeight_: integer);
begin
  isMoving := False;
  BorderWidth := BorderWidth_;
  BorderHeight := BorderHeight_;
end;

procedure TObjectMove.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isMoving := True;
  PreviousX := X;
  PreviousY := Y;
end;

procedure TObjectMove.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if isMoving = True then begin
    ObjectMove(twincontrol(Sender), X, Y);
  end;
end;

procedure TObjectMove.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isMoving := False;
end;

procedure TObjectMove.ObjectMove(Object_: Twincontrol; X, Y: integer);
var
  NewX: integer;
  NewY: integer;
begin
  NewX := Object_.Left + X - PreviousX;
  NewY := Object_.Top + Y - PreviousY;
  if NewX >= (BorderWidth - Object_.Width) then
    Object_.Left := BorderWidth - Object_.Width
  else if NewX <= 0 then
    Object_.Left := 0
  else
    Object_.Left := NewX;
  if NewY >= (BorderHeight - Object_.Height - 20) then
    Object_.Top := BorderHeight - Object_.Height - 20
  else if NewY <= 0 then
    Object_.Top := 0
  else
    Object_.Top := NewY;
end;

end.
