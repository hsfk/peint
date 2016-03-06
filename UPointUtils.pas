unit UPointUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPoints = array of TPoint;

operator +(a, b: TPoint): TPoint; overload; inline;
operator +(a: TPoint; b: integer): TPoint; overload; inline;
operator -(a, b: TPoint): TPoint; overload; inline;
operator -(a: TPoint; b: integer): TPoint; overload; inline;
operator / (a: TPoint; b: integer): TPoint; overload; inline;
operator * (a: TPoint; b: integer): TPoint; overload; inline;
function ToPoint(X, Y: integer): TPoint; inline;
function null: TPoint;
function min: TPoint;
function max: TPoint;
procedure SaveMin(var a: TPoint; b: TPoint); //inline;
procedure SaveMax(var a: TPoint; b: TPoint);// inline;

implementation

operator +(a, b: TPoint): TPoint; overload; inline;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

operator +(a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x + b;
  Result.y := a.y + b;
end;

operator -(a, b: TPoint): TPoint; overload; inline;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;

operator -(a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x - b;
  Result.y := a.y - b;
end;

operator / (a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x div b;
  Result.y := a.y div b;
end;

operator * (a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
end;

function ToPoint(X, Y: integer): TPoint; inline;
begin
  Result.x := X;
  Result.y := Y;
end;

function null: TPoint;
begin
  Result.x := 0;
  Result.y := 0;
end;

function min: TPoint;
begin
  Result.x := -99999;
  Result.y := -99999;
end;

function max: TPoint;
begin
  Result.x := 99999;
  Result.y := 99999;
end;

procedure SaveMin(var a: TPoint; b: TPoint);// inline;
begin
  if b.x < a.x then
    a.x := b.x;
  if b.y < a.y then
    a.y := b.y;
end;

procedure SaveMax(var a: TPoint; b: TPoint);// inline;
begin
  if b.x > a.x then
    a.x := b.x;
  if b.y > a.y then
    a.y := b.y;
end;

end.
