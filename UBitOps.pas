unit UBitOps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Cache = array [0..255] of string;
  BitCache = array [0..255] of word;
  LengthCache = array [0..255] of byte;

  TBitOps = class
  public
    function GetBitCache(Routes: cache): BitCache;
    function GetLengthCache(Routes: cache): LengthCache;
  private
    procedure AddBitsToNum(var Number: word; bits: string);
  end;

var
  BCache: BitCache;
  LCache: lengthCache;

implementation

function TBitOps.GetBitCache(Routes: Cache): BitCache;
var
  i: byte;
begin
  for i := 0 to 255 do begin
    Result[i] := 0;
    AddBitsToNum(Result[i], Routes[i]);
  end;
end;

function TBitOps.GetLengthCache(Routes: cache): LengthCache;
var
  i: byte;
begin
  for i := 0 to 255 do
    Result[i] := length(Routes[i]);
end;

procedure TBitOps.AddBitsToNum(var Number: word; bits: string);
 { добавляет несколько бит в конец числа }
var
  i: byte;
begin
  if bits = '' then begin
    Number := 0;
    exit;
  end;

  for i := 0 to length(bits) - 1 do begin
    Number := Number shl 1;
    if bits[i + 1] <> '0' then
      Number := Number xor 1;
  end;
end;

end.
