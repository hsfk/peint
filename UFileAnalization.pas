unit UFileAnalization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  FrequencyTable = array [0..255] of longword;

  TFile = class
  public
    function GetFrequency(var Stream: TmemoryStream): FrequencyTable;
  end;

implementation

function Tfile.GetFrequency(var Stream: TmemoryStream): FrequencyTable;
var
  sizeoffile: int64;
  symbol, i: byte;
begin
  Stream.Seek(0, soBeginning);
  for i := 0 to 255 do
    Result[i] := 0;
  sizeoffile := Stream.Size;

  while SizeOfFile <> 0 do begin
    symbol := Stream.ReadByte;
    Result[symbol] += 1;
    SizeOfFile -= 1;
  end;
  Stream.Seek(0, soBeginning);
end;

end.
