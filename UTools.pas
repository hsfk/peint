unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs;

type
  Coords = array of integer;
  ToolData = record
    NofTool: integer;
    PenColor: TColor;
    BrushColor: Tcolor;
    BrushStyle: TBrushStyle;
    Width: integer;
    Coord: Coords;
  end;
  TTools = class
  public
    Coord : Coords;
    constructor Create(var holstToDraw: TBitmap);
    procedure BeforeDraw(x, y: integer); virtual;
    procedure Draw(x, y: integer); virtual; abstract;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); virtual; abstract;
    procedure AfterDraw;
  private
    FCanvas: tbitmap;
    FTmpCanvas: tbitmap;
  end;

  TBufferRequired = class(TTools)
  public
    procedure Draw(x, y: integer); override; abstract;
    procedure ExportToSvg(var output: TextFile; Data: ToolData);
     override; abstract;
  end;


  TBufferNotRequired = class(TTools)
  public
    procedure Draw(x, y: integer); override; abstract;
    procedure ExportToSvg(var output: TextFile; Data: ToolData);
      override; abstract;
  end;

  TRectangle = class(TBufferRequired)
  public
    procedure Draw(x, y: integer); override;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); override;
  end;

  TEllipse = class(TBufferRequired)
  public
    procedure Draw(x, y: integer); override;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); override;
  end;

  TPen = class(TBufferNotRequired)
  public
    procedure Draw(x, y: integer); override;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); override;
  end;

  TFill = class(TBufferNotRequired)
  public
    procedure Draw(x, y: integer); override;
    procedure BeforeDraw(x, y: integer); override;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); override;
  end;

implementation
//NOT RELATED TO CLASS
function ColorToHex(color: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x', [byte(color), byte(color shr 8), byte(color shr 16)]);
end;
// CLASS METHODS
constructor TTools.Create(var holstToDraw: TBitmap);
begin
  FCanvas := holstToDraw;
end;

procedure TTools.BeforeDraw(x, y: integer);
var
  r: trect;
begin
  Setlength(Coord,2);
  Coord[0] := x;
  Coord[1] := y;
  FTmpCanvas := tbitmap.Create;
  FTmpCanvas.Width := FCanvas.Width;
  FTmpCanvas.Height := FCanvas.Height;
  r := bounds(0, 0, FCanvas.Width, FCanvas.Height);
  FTmpCanvas.Canvas.CopyRect(rect(0, 0, FTmpCanvas.Width, FTmpCanvas.Height),
    FCanvas.canvas, r);
end;

procedure TTools.AfterDraw;
begin
  FTmpCanvas.free;
end;

procedure TRectangle.Draw(x, y: integer);
var
  r: trect;
begin
  Setlength(Coord, 4);
  Coord[2] := x;
  Coord[3] := y;
  r := bounds(0, 0, FTmpCanvas.Width, FTmpCanvas.Height);
  FCanvas.Canvas.CopyRect(rect(0, 0, FCanvas.Width, FCanvas.Height), FTmpCanvas.canvas, r);
  FCanvas.Canvas.Rectangle(Coord[0], Coord[1], X, Y);
end;

procedure TRectangle.ExportToSvg(var output: TextFile; Data: ToolData);
var
  x, y, wdt, hgt: integer;
begin
  if Data.coord[0] > Data.coord[2] then begin
    x := Data.coord[2];
    wdt := Data.coord[0] - x;
  end
  else begin
    x := Data.Coord[0];
    wdt := Data.Coord[2] - x;
  end;

  if Data.coord[1] > Data.coord[3] then begin
    y := Data.coord[3];
    hgt := Data.coord[1] - y;
  end
  else begin
    y := Data.Coord[1];
    hgt := Data.Coord[3] - y;
  end;
  writeln(output, '  <rect x="', x, '" y="', y, '" width="', wdt, '" height="',
    hgt, '" fill="', ColorToHex(Data.BrushColor), '" stroke="', ColorToHex(
    Data.PenColor), '" stroke-width="', Data.Width, '"  />');
end;

procedure TEllipse.Draw(x, y: integer);
var
  r: trect;
begin
  Setlength(Coord, 4);
  Coord[2] := x;
  Coord[3] := y;
  r := bounds(0, 0, FTmpCanvas.Width, FTmpCanvas.Height);
  FCanvas.Canvas.CopyRect(rect(0, 0, FCanvas.Width, FCanvas.Height), FTmpCanvas.canvas, r);
  FCanvas.Canvas.Ellipse(Coord[0], Coord[1], x, y);
end;

procedure TEllipse.ExportToSvg(var output: TextFile; Data: ToolData);
var
  cx, cy, rx, ry: integer;
begin
  cx := (Data.Coord[0] + Data.coord[2]) div 2;
  cy := (Data.Coord[1] + Data.coord[3]) div 2;
  rx := abs(Data.coord[2] - cx);
  ry := abs(Data.coord[3] - cy);
  writeln(output, '  <ellipse cx="', cx, '" cy="', cy, '" rx="', rx, '" ry="', ry,
    '" fill="', ColorToHex(Data.BrushColor), '" stroke="', ColorToHex(
    Data.PenColor), '" stroke-width="', Data.Width, '"  />');
end;

procedure TPen.Draw(x, y: integer);
var PreX,PreY : integer;
begin
  PreX := Coord[high(Coord) - 1];
  PreY := Coord[high(Coord)];
    Setlength(Coord, length(coord) + 2);
    Coord[high(coord) - 1] := x;
    Coord[high(coord)] := y;
    FCanvas.canvas.line(PreX, PreY, x, y);
end;

procedure TPen.ExportToSvg(var output: TextFile; Data: ToolData);
var
  i: integer;
begin
  writeln(output, '  <polyline fill="none" stroke="', ColorToHex(
    Data.PenColor), '" stroke-width="', Data.Width, '"');
  writeln(output, '  points="');
  i := 0;
  while i <= high(Data.Coord) do begin
    Write(output, Data.coord[i], ',', Data.coord[i + 1], ' ');
    i += 2;
  end;
  Write(output, '" />');
end;

procedure TFill.BeforeDraw(x, y: integer);
var
  pColor: TColor;
begin
  inherited BeforeDraw(x, y);
  pColor := FCanvas.canvas.Pixels[x, y];
  FCanvas.canvas.FloodFill(x, y, pcolor, TFillStyle.fsSurface);
end;

procedure TFill.Draw(x, y: integer);
begin
  //nothing
end;

procedure TFill.ExportToSvg(var output: TextFile; Data: ToolData);
begin
  //nothing
end;

end.
