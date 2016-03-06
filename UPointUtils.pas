unit UPointUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPoints = array of TPoint;

operator + (a, b: TPoint): TPoint; overload;
operator - (a, b: TPoint): TPoint; overload;
operator - (a: TPoint; b: integer): TPoint; overload;
operator / (a: TPoint; b: integer): TPoint; overload;
operator * (a: TPoint; b: integer): TPoint; overload;
function ToPoint(X, Y: integer): TPoint;
function null: TPoint;

implementation

operator + (a, b: TPoint): TPoint; overload;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

operator - (a, b: TPoint): TPoint; overload;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;

operator - (a: TPoint; b: integer): TPoint; overload;
begin
  Result.x := a.x - b;
  Result.y := a.y - b;
end;

operator / (a: TPoint; b: integer): TPoint; overload;
begin
  Result.x := a.x div b;
  Result.y := a.y div b;
end;

operator * (a: TPoint; b: integer): TPoint; overload;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
end;

function ToPoint(X, Y: integer): TPoint;
begin
  Result.x := X;
  Result.y := Y;
end;

function null: TPoint;
begin
  Result.x := 0;
  Result.y := 0;
end;

end.
