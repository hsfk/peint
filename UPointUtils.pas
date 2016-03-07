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
operator >= (a, b: TPoint): boolean; overload; inline;
operator <= (a, b: TPoint): boolean; overload; inline;
function ToPoint(X, Y: integer): TPoint; inline;
function null: TPoint;
function min: TPoint;
function max: TPoint;
procedure SaveMin(var a: TPoint; b: TPoint); inline;
procedure SaveMax(var a: TPoint; b: TPoint); inline;
function IsIntersect(AMin, AMax, BMin, BMax: TPoint): boolean;
function SegIntersect(AStart, AEnd, BStart, BEnd: integer): boolean;

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

operator >= (a, b: TPoint): boolean; overload; inline;
begin
  Result := False;
  if (a.x >= b.x) and (a.y >= b.y) then
    Result := True;
end;

operator <= (a, b: TPoint): boolean; overload; inline;
begin
  Result := False;
  if (a.x <= b.x) and (a.y <= b.y) then
    Result := True;
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

procedure SaveMin(var a: TPoint; b: TPoint); inline;
begin
  if b.x < a.x then
    a.x := b.x;
  if b.y < a.y then
    a.y := b.y;
end;

procedure SaveMax(var a: TPoint; b: TPoint); inline;
begin
  if b.x > a.x then
    a.x := b.x;
  if b.y > a.y then
    a.y := b.y;
end;

function IsIntersect(AMin, AMax, BMin, BMax: TPoint): boolean;
var
  XIntersect: boolean;
  YIntersect: boolean;
begin
  XIntersect := SegIntersect(AMin.x, AMax.x, BMin.x, BMax.x);
  YIntersect := SegIntersect(AMin.y, AMax.y, BMin.y, BMax.y);
   Result := XIntersect and YIntersect;
end;

function SegIntersect(AStart, AEnd, BStart, BEnd: integer): boolean;
var
  Temp: integer;
begin
  if AStart > AEnd then begin
    Temp := AEnd;
    AEnd := AStart;
    AStart := Temp;
  end;
  if BStart > BEnd then begin
    Temp := BEnd;
    BEnd := BStart;
    BStart := Temp;
  end;
  Result := False;
  if (AStart > BStart) and (AStart < BEnd) or (AEnd > BStart) and (AEnd < BEnd) then
    Result := True;
  if (BStart > AStart) and (BStart < AEnd) or (BEnd > AStart) and (BEnd < AEnd) then
    Result := True;
end;

end.
