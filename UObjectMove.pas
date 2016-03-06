unit UObjectMove;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, UPointUtils;

type
  TObjectMove = class
    constructor Create(ABorderWidth, ABorderHeight: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    FIsMoving: boolean;
    FPreviousPos: TPoint;
    FNewPosition: TPoint;
    FBuffer: integer;
    FBorderWidth: integer;
    FBorderHeight: integer;
    procedure ObjectMove(AObject: Twincontrol; Position: TPoint);
    procedure BorderCheck(var ObjectPos: integer; ObjectSide, NewPos, Border: integer);
  end;

var
  PanelMove: TObjectMove;

implementation

constructor TObjectMove.Create(ABorderWidth, ABorderHeight: integer);
begin
  FIsMoving := False;
  FBorderWidth := ABorderWidth;
  FBorderHeight := ABorderHeight;
end;

procedure TObjectMove.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FIsMoving := True;
  FPreviousPos := ToPoint(X, Y);
end;

procedure TObjectMove.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if FIsMoving = True then begin
    ObjectMove(twincontrol(Sender), ToPoint(X, Y));
  end;
end;

procedure TObjectMove.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FIsMoving := False;
end;

procedure TObjectMove.ObjectMove(AObject: Twincontrol; Position: TPoint);
begin
  FNewPosition := ToPoint(AObject.Left, AObject.Top) + Position - FPreviousPos;
  FBuffer := AObject.Left;
  BorderCheck(FBuffer, AObject.Width, FNewPosition.x, FBorderWidth);
  AObject.Left := FBuffer;
  FBuffer := AObject.Top;
  BorderCheck(FBuffer, AObject.Height, FNewPosition.y, FBorderHeight);
  AObject.Top := FBuffer;
end;

procedure TObjectMove.BorderCheck(var ObjectPos: integer;
  ObjectSide, NewPos, Border: integer);
begin
  if NewPos >= Border - ObjectSide then
    ObjectPos := Border - ObjectSide
  else if NewPos < 0 then
    ObjectPos := 0
  else
    ObjectPos := NewPos;
end;

end.
