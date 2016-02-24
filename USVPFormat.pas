unit USVPFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTools, UPackUnpack, Graphics, UZoom;

type
  TSVPFormat = class
    constructor Create;
    procedure ExportToSVP(Filename: string);
    procedure ImportFromSVP(Filename: string);
  private
    MStream: TMemoryStream;
    FHuffman: THuffman;
    { SIZE,TOOL_CODE,PCOL_SIZE,PEN_COL,BCOL_SIZE,BRUSH_COL,BRUSH_STYLE,PEN_WIDTH,SIZE,X,Y,X,Y...}
    HISTORY_SIZE: string;
    TOOL_CODE: byte;
    PEN_COLOR: string;
    PEN_COLOR_SIZE: byte;
    BRUSH_COLOR: string;
    BRUSH_COLOR_SIZE: byte;
    BRUSH_STYLE: byte;
    PEN_WIDTH: byte;
    COORD_SIZE: string;
    function BStyleToByte(Style: TBrushStyle): byte;
    function ByteToBStyle(Byte_: byte): TBrushStyle;
    function ValueToByteString(Value: string; Size: byte): string;
    function GetIntFromString(Str: string): int64;
    function ReadBytes(var Stream: TMemoryStream; Amount: byte): string;
  end;

implementation

constructor TSVPFormat.Create;
begin
  HISTORY_SIZE := chr(0);
  TOOL_CODE := 0;
  PEN_COLOR := '';
  BRUSH_COLOR := '';
  BRUSH_STYLE := 0;
  PEN_WIDTH := 0;
  COORD_SIZE := chr(0);
end;

procedure TSVPFormat.ExportToSVP(Filename: string);
var
  i: integer;
  j: integer;
  Data: ToolData;
  Value: string;
begin
  MStream.Free;
  MStream := TMemoryStream.Create;
  HISTORY_SIZE := IntToStr(ToolsDataUtils.GetLength);
  HISTORY_SIZE := ValueToByteString(HISTORY_SIZE, 2); { 2 - sizeof int }
  MStream.Write(HISTORY_SIZE[1], 2);
  for i := 0 to ToolsDataUtils.GetLength - 1 do begin
    Data := ToolsDataUtils.GetData(i);

    TOOL_CODE := Data.NofTool;
    PEN_COLOR := ColorToString(Data.PenColor);
    PEN_COLOR_SIZE := Length(PEN_COLOR);
    BRUSH_COLOR := ColorToString(Data.BrushColor);
    BRUSH_COLOR_SIZE := Length(BRUSH_COLOR);
    BRUSH_STYLE := BStyleToByte(Data.BrushStyle);
    PEN_WIDTH := Data.Width;
    COORD_SIZE := IntToStr(Length(Data.ToolCoords));
    COORD_SIZE := ValueToByteString(COORD_SIZE, 2);

    MStream.WriteByte(TOOL_CODE);
    MStream.WriteByte(PEN_COLOR_SIZE);
    MStream.Write(PEN_COLOR[1], PEN_COLOR_SIZE);
    MStream.WriteByte(BRUSH_COLOR_SIZE);
    MStream.Write(BRUSH_COLOR[1], BRUSH_COLOR_SIZE);
    MStream.WriteByte(BRUSH_STYLE);
    MStream.WriteByte(PEN_WIDTH);

    MStream.Write(COORD_SIZE[1], 2);
    for j := 0 to length(Data.ToolCoords) - 1 do begin
      Value := ValueToByteString(IntToStr(Data.ToolCoords[j]), 2);
      MStream.Write(Value[1], 2);
    end;
  end;

  FHuffman := THuffman.Create(MStream);
  FHuffman.Pack(Filename);
  FHuffman.Free;
end;

procedure TSVPFormat.ImportFromSVP(Filename: string);
var
  HISTORY_SIZE_: integer;
  COORD_SIZE_: word;
  i: integer;
  j: word;
  Coordinates: Coords;
begin
  MStream.Free;
  MStream := TMemoryStream.Create;
  MStream.LoadFromFile(Filename);

  FHuffman := THuffman.Create(MStream);
  MStream := FHuffman.Unpack;
  MStream.Seek(0, soBeginning);
  FHuffman.Free;
  ToolsDataUtils.AllClear;

  HISTORY_SIZE := ReadBytes(MStream, 2);
  HISTORY_SIZE_ := GetIntFromString(HISTORY_SIZE);
  for i := 1 to HISTORY_SIZE_ do begin
    TOOL_CODE := MStream.ReadByte;
    PEN_COLOR_SIZE := MStream.ReadByte;
    PEN_COLOR := ReadBytes(MStream, PEN_COLOR_SIZE);
    BRUSH_COLOR_SIZE := MStream.ReadByte;
    BRUSH_COLOR := ReadBytes(MStream, BRUSH_COLOR_SIZE);
    BRUSH_STYLE := MStream.ReadByte;
    PEN_WIDTH := MStream.ReadByte;
    COORD_SIZE := ReadBytes(MStream, 2);
    COORD_SIZE_ := GetIntFromString(COORD_SIZE);

    SetLength(Coordinates, COORD_SIZE_);
    j := 0;
    while j <= COORD_SIZE_ - 1 do begin
      Coordinates[j] := GetIntFromString(ReadBytes(MStream, 2));
      Coordinates[j + 1] := GetIntFromString(ReadBytes(MStream, 2));
      j += 2;
    end;
    ToolsDataUtils.Add(TOOL_CODE, PEN_WIDTH, StringToColor(PEN_COLOR),
      StringToColor(BRUSH_COLOR), ByteToBStyle(BRUSH_STYLE));
    ToolsDataUtils.AddCoords(Coordinates);
  end;
  ToolsDataUtils.SetHistoryPosition(HISTORY_SIZE_ - 1);
  ToolsDataUtils.ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
end;

function TSVPFormat.BStyleToByte(Style: TBrushStyle): byte;
begin
  case Style of
    bsHorizontal: Result := 0;
    bsSolid: Result := 1;
    bsClear: Result := 2;
    bsVertical: Result := 3;
    bsFdiagonal: Result := 4;
    bsBDiagonal: Result := 5;
    bsCross: Result := 6;
    bsDiagCross: Result := 7;
  end;
end;

function TSVPFormat.ByteToBStyle(Byte_: byte): TBrushStyle;
begin
  case Byte_ of
    0: Result := bsHorizontal;
    1: Result := bsSolid;
    2: Result := bsClear;
    3: Result := bsVertical;
    4: Result := bsFDiagonal;
    5: Result := bsBDiagonal;
    6: Result := bsCross;
    7: Result := bsDiagCross;
  end;
end;

function TSVPFormat.ValueToByteString(Value: string; Size: byte): string;
var
  symbol: byte;
  number: int64;
begin
  Result := '';
  number := StrToInt64(Value);
  while number <> 0 do begin
    symbol := 0;
    symbol := symbol or number;
    Result := chr(symbol) + Result;
    number := number shr 8;
  end;
  while length(Result) < Size do
    Result := chr(0) + Result;
end;

function TSVPFormat.GetIntFromString(Str: string): int64;
var
  c: char;
  Byte_: byte;
  i: byte;
begin
  Result := 0;
  for i := 1 to length(Str) do begin
    c := Str[i];
    Byte_ := Ord(c);
    Result := Result or Byte_;
    if i <> length(Str) then
      Result := Result shl 8;
  end;
end;

function TSVPFormat.ReadBytes(var Stream: TMemoryStream; Amount: byte): string;
var
  Byte_: byte;
  i: byte;
begin
  Result := '';
  for i := 1 to Amount do begin
    Byte_ := Stream.ReadByte;
    Result := Result + chr(Byte_);
  end;
end;

end.
