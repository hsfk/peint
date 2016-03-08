unit USVPFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UHistory, UFigure, Graphics,UFigureHistoryManager;

type
  CFigure = class of TFigure;

  AFigure = record
    Name: string;
    Figure: CFigure;
  end;

  TSVPFormat = class
  public
    constructor Create(FileName: string; Scene: TCanvas);
    procedure ExportToSVP;
    procedure ImportFromSVP;
  private
    FScene: TCanvas;
    FFigures: array of AFigure;
    FFilename: string;
    FFile: Text;
    procedure ExportFigure(Figure: TFigure);
    function ImportFigure: TFigure;
    function ReadBytes(Amount: integer): string;
    function GetFigureIndex(FigureName: string): integer;
    procedure InitFigure(FigureName: string; Figure: CFigure);
    function ReadPoint: TPoint;
    function PointToByteStr(Point: TPoint): string;
    function ValueToByteString(Value: string; Size: integer): string;
    function ByteStringToVal(Value: string): int64;
    function PenStyleToByte(Style: TPenStyle): integer;
    function BrushStyleToByte(Style: TBrushStyle): integer;
    function ByteToPenStyle(AByte: integer): TPenStyle;
    function ByteToBrushStyle(AByte: integer): TBrushStyle;
  end;

implementation

constructor TSVPFormat.Create(FileName: string; Scene: TCanvas);
begin
  FScene := Scene;
  FFilename := FileName;

  InitFigure('Pen', TPolyLine);
  InitFigure('Poly line', TPolyLine);
  InitFigure('Line', TLine);
  InitFigure('Rectangle', TRectangle);
  InitFigure('Ellipse', TEllipse);
end;

procedure TSVPFormat.ExportToSVP;
var
  i: integer;
begin
  Assign(FFile, FFilename);
  Rewrite(FFile);
  Write(FFile, 'SOLG');
  Write(FFile, ValueToByteString(IntToStr(History.DataLength), 2));
  for i := 0 to History.DataLength - 1 do
    ExportFigure(History.GetFigure(i));
  Close(FFile);
end;

procedure TSVPFormat.ImportFromSVP;
var
  i: integer;
  Size: integer;
begin
  Assign(FFile, FFilename);
  Reset(FFile);
  if ReadBytes(4) <> 'SOLG' then
    Exit;
  History.DeleteAll;
  Size := ByteStringToVal(ReadBytes(2));
  for i := 0 to Size - 1 do
    History.Insert(ImportFigure);
  History.Show;
  FigureManager.LoadHistory;
end;

{
|HISTORY SIZE 2B|FIG INDEX 1B|PWIDTH 1B|SIZEOF PCOLOR 1B|PCOLOR STR|PSTYLE 1B|
|SIZEOF BCOLOR 1B|BCOLOR STR|BSTYLE 1B|SIZEOF POINTS 4B|POINTS|...
}
procedure TSVPFormat.ExportFigure(Figure: TFigure);
var
  Color: ShortString;
  i: integer;
begin
  Write(FFile, Chr(GetFigureIndex(Figure.Name)));
  Write(FFile, Chr(Figure.PenWidth));
  Color := ColorToString(Figure.PenColor);
  Write(FFile, Chr(Length(Color)));
  Write(FFile, Color);
  Write(FFile, Chr(PenStyleToByte(Figure.PenStyle)));
  Color := ColorToString(Figure.BrushColor);
  Write(FFile, Chr(Length(Color)));
  Write(FFile, Color);
  Write(FFile, Chr(BrushStyleToByte(Figure.BrushStyle)));
  Write(FFile, ValueToByteString(IntToStr(Figure.DataLength), 4));
  for i := 0 to Figure.DataLength - 1 do
    Write(FFile, PointToByteStr(Figure.GetPoint(i)));
end;

function TSVPFormat.ImportFigure: TFigure;
var
  AChar: char;
  Size: longword;
  i: longword;
begin
  Read(FFile, AChar);
  Result := FFigures[Ord(AChar)].Figure.Create(FScene, FFigures[Ord(AChar)].Name);
  Read(FFile, AChar);
  FScene.Pen.Width := Ord(AChar);
  Read(FFile, AChar);
  FScene.Pen.Color := StringToColor(ReadBytes(Ord(AChar)));
  Read(FFile, AChar);
  FScene.Pen.Style := ByteToPenStyle(Ord(AChar));
  Read(FFile, AChar);
  FScene.Brush.Color := StringToColor(ReadBytes(Ord(AChar)));
  Read(FFile, AChar);
  FScene.Brush.Style := ByteToBrushStyle(Ord(AChar));
  Size := ByteStringToVal(ReadBytes(4));
  for i := 0 to Size - 1 do
    Result.Add(ReadPoint);
end;

function TSVPFormat.ReadBytes(Amount: integer): string;
var
  i: integer;
  AByte: char;
begin
  Result := '';
  for i := 1 to Amount do begin
    Read(FFile, AByte);
    Result := Result + AByte;
  end;
end;

function TSVPFormat.GetFigureIndex(FigureName: string): integer;
var
  i: integer;
begin
  for i := 0 to Length(FFigures) - 1 do
    if FigureName = FFigures[i].Name then
      Exit(i);
end;

procedure TSVPFormat.InitFigure(FigureName: string; Figure: CFigure);
begin
  SetLength(FFigures, Length(FFigures) + 1);
  FFigures[High(FFigures)].Name := FigureName;
  FFigures[High(FFigures)].Figure := Figure;
end;

function TSVPFormat.ReadPoint: TPoint;
begin
  Result.x := ByteStringToVal(ReadBytes(2));
  Result.y := ByteStringToVal(ReadBytes(2));
end;

function TSVPFormat.PointToByteStr(Point: TPoint): string;
begin
  Result := ValueToByteString(IntToStr(Point.x), 2);
  Result := Result + ValueToByteString(IntToStr(Point.y), 2);
end;

function TSVPFormat.ValueToByteString(Value: string; Size: integer): string;
var { Converts n-byte value to string with n symbols }
  AByte: byte;
  Number: int64;
begin
  Result := '';
  Number := StrToInt64(Value);
  while Number <> 0 do begin
    AByte := 0;
    AByte := AByte or Number;
    Result := chr(AByte) + Result;
    Number := Number shr 8;
  end;
  while length(Result) < size do
    Result := chr(0) + Result;
end;

function TSVPFormat.ByteStringToVal(Value: string): int64;
var
  AChar: char;
  AByte: byte;
  i: integer;
begin
  Result := 0;
  for i := 1 to length(Value) do begin
    AChar := Value[i];
    AByte := Ord(AChar);
    Result := Result or AByte;
    if i <> length(Value) then
      Result := Result shl 8;
  end;
end;

function TSVPFormat.PenStyleToByte(Style: TPenStyle): integer;
begin
  Result := 0;
  case Style of
    psClear: Result := 0;
    psDash: Result := 1;
    psDashDot: Result := 2;
    psDashDotDot: Result := 3;
    psSolid: Result := 4;
  end;
end;

function TSVPFormat.BrushStyleToByte(Style: TBrushStyle): integer;
begin
  Result := 0;
  case Style of
    bsClear: Result := 0;
    bsBDiagonal: Result := 1;
    bsCross: Result := 2;
    bsDiagCross: Result := 3;
    bsFDiagonal: Result := 4;
    bsHorizontal: Result := 5;
    bsVertical: Result := 6;
    bsSolid: Result := 7;
  end;
end;

function TSVPFormat.ByteToPenStyle(AByte: integer): TPenStyle;
begin
  Result := psClear;
  case AByte of
    0: Result := psClear;
    1: Result := psDash;
    2: Result := psDashDot;
    3: Result := psDashDotDot;
    4: Result := psSolid;
  end;
end;

function TSVPFormat.ByteToBrushStyle(AByte: integer): TBrushStyle;
begin
  Result := bsClear;
  case AByte of
    0: Result := bsClear;
    1: Result := bsBDiagonal;
    2: Result := bsCross;
    3: Result := bsDiagCross;
    4: Result := bsFDiagonal;
    5: Result := bsHorizontal;
    6: Result := bsVertical;
    7: Result := bsSolid;
  end;
end;

end.
