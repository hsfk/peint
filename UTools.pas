unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, ExtCtrls, UZoom, Controls;

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

  ClassOfInstrument = class of TTools;

  ToolName = record
    Tool: ClassOFInstrument;
    NameOfTool: string;
  end;

  TTools = class
  public
    Coord: Coords;
    constructor Create(var holstToDraw: TBitmap);
    procedure AddToHistory(ToolTag: integer; Canvas_: tbitmap); virtual;
    procedure AddCoordsToHistory(Coordinates: Coords); virtual;
    procedure BeforeDraw(x, y: integer; Button: TMouseButton); virtual;
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

  TSpecTools = class(TTools)
  public
    procedure AddToHistory(ToolTag: integer; Canvas_: tbitmap); override;
    procedure AddCoordsToHistory(Coordinates: Coords); override;
    procedure Draw(x, y: integer); override;
  end;

  TZoom = class(TSpecTools)
  public
    procedure BeforeDraw(x, y: integer; Button: TMouseButton); override;
  end;

  TSelect = class(TSpecTools)
  public
    procedure BeforeDraw(x, y: integer; Button: TMouseButton); override;
  end;

  TArm = class(TSpecTools)
  public
    procedure BeforeDraw(x, y: integer; Button: TMouseButton); override;
    procedure Draw(x, y: integer);
    private
    prevx,prevy : integer;
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
    procedure BeforeDraw(x, y: integer; Button: TMouseButton); override;
    procedure ExportToSvg(var output: TextFile; Data: ToolData); override;
  end;

  TToolsDataUtils = class
  public
    constructor Create(var PaintBox_: TPaintbox; var Holst_: tbitmap);
    procedure ShowHistory(x_param, y_param, delta_param: integer);
    procedure Add(Name, wdt: integer; Pcolor, Bcolor: tcolor; Style: TBrushStyle);
    procedure AddCoords(var Coordinates: Coords);
    procedure Undo(x_param, y_param, delta_param: integer);
    procedure Redo(x_param, y_param, delta_param: integer);
    function GetLength: integer;
    function GetData(i: integer): ToolData;
    function IsFigureExists(x, y, inaccuracy: integer): integer;
    procedure HighLightFigure(num: integer);
    procedure MovFigure(num, x_offset, y_offset: integer);
  private
    PrevPColor, PrevBColor: TColor;
    PrevBStyle: TBrushStyle;
    FPaintBox: TPaintbox;
    FCanvas: tbitmap;
    Data: array of ToolData;
    position, PrevPWidth: integer;
    procedure SaveColors;
    procedure LoadColors;
  end;

var
  ToolsDataUtils: TToolsDataUtils;
  ClassRef: array of ToolName;

implementation

function ColorToHex(color: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x', [byte(color), byte(color shr 8),
    byte(color shr 16)]);
end;

procedure Init(Tool: ClassOfInstrument; NameOfTool: string);
begin
  setlength(ClassRef, length(ClassRef) + 1);
  classref[high(ClassRef)].Tool := Tool;
  classref[high(ClassRef)].NameOfTool := NameOfTool;
end;

constructor TTools.Create(var holstToDraw: TBitmap);
begin
  FCanvas := holstToDraw;
end;

procedure TTools.AddToHistory(ToolTag: integer; Canvas_: tbitmap);
begin
  with Canvas_.Canvas do begin
    ToolsDataUtils.Add(ToolTag, pen.Width, pen.color, Brush.Color, brush.Style);
    ToolsDataUtils.AddCoords(Coord);
  end;
end;

procedure TTools.AddCoordsToHistory(Coordinates: Coords);
begin
  ToolsDataUtils.AddCoords(Coordinates);
end;

procedure TTools.BeforeDraw(x, y: integer; Button: TMouseButton);
var
  r: trect;
begin
  Setlength(Coord, 2);
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
  FTmpCanvas.Free;
end;

{ Tools }
procedure TZoom.BeforeDraw(x, y: integer; Button: TMouseButton);
begin
  with Zoom do begin
    SetZoom(x, y);
    if Button = mbLeft then begin
      ZoomIn(x, y);
      ToolsDataUtils.ShowHistory(z_x, z_y, z_n);
    end;
    if Button = mbRight then begin
      ZoomOut(x, y);
      ToolsDataUtils.ShowHistory(z_px, z_py, z_n);
    end;
  end;
end;

procedure TSelect.BeforeDraw(x, y: integer; Button: TMouseButton);
var
  fig: integer;
begin
  fig := ToolsDataUtils.IsFigureExists(x, y, 3);
  if fig <> -1 then
    ToolsDataUtils.HighLightFigure(fig)
  else
    with Zoom do
      ToolsDataUtils.ShowHistory(z_x, z_y, z_n);
end;

procedure TArm.BeforeDraw(x, y: integer; Button: TMouseButton);
var i : integer;
begin

end;

procedure TArm.Draw(x, y: integer);
begin

end;

procedure TRectangle.Draw(x, y: integer);
var
  r: trect;
begin
  Setlength(Coord, 4);
  Coord[2] := x;
  Coord[3] := y;
  r := bounds(0, 0, FTmpCanvas.Width, FTmpCanvas.Height);
  FCanvas.Canvas.CopyRect(rect(0, 0, FCanvas.Width, FCanvas.Height),
    FTmpCanvas.canvas, r);
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
  FCanvas.Canvas.CopyRect(rect(0, 0, FCanvas.Width, FCanvas.Height),
    FTmpCanvas.canvas, r);
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
var
  PreX, PreY: integer;
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

procedure TFill.BeforeDraw(x, y: integer; Button: TMouseButton);
var
  pColor: TColor;
begin
  inherited BeforeDraw(x, y, Button);
  pColor := FCanvas.canvas.Pixels[x, y];
  FCanvas.canvas.FloodFill(x, y, pcolor, TFillStyle.fsSurface);
end;

procedure TFill.Draw(x, y: integer);
begin { nothing }
end;

procedure TFill.ExportToSvg(var output: TextFile; Data: ToolData);
begin { nothing }
end;

procedure TSpecTools.AddToHistory(ToolTag: integer; Canvas_: tbitmap);
begin { nothing }
end;

procedure TSpecTools.AddCoordsToHistory(Coordinates: Coords);
begin { nothing }
end;

procedure TSpecTools.Draw(x, y: integer);
begin { nothing }
end;

{ ToolsDataUtils }
constructor TToolsDataUtils.Create(var PaintBox_: TPaintbox; var Holst_: tbitmap);
begin
  position := -1;
  FCanvas := Holst_;
  FPaintBox := PaintBox_;
end;

procedure TToolsDataUtils.ShowHistory(x_param, y_param, delta_param: integer);
var
  i, j: integer;
  tool: TTools;
  NullBtn: TmouseButton;
begin
  NullBtn := mbRight;
  delta_param := 1 shl delta_param;
  SaveColors;
  with FCanvas.Canvas do begin
    pen.color := clWhite;
    brush.color := clWhite;
    brush.style := bsSolid;
  end;
  FCanvas.Canvas.rectangle(0, 0, FCanvas.Width, FCanvas.Height);
  for i := 0 to position do begin
    tool := classref[Data[i].Noftool].Tool.Create(FCanvas);
    with Data[i] do begin
      FCanvas.Canvas.pen.color := PenColor;
      FCanvas.Canvas.brush.color := BrushColor;
      FCanvas.Canvas.brush.Style := BrushStyle;
      FCanvas.Canvas.pen.Width := Width;
      tool.BeforeDraw((coord[0] - x_param) * delta_param, (coord[1] - y_param) *
        delta_param, NullBtn);
      j := 2;
      while j <= high(coord) do begin
        tool.Draw((coord[j] - x_param) * delta_param, (coord[j + 1] - y_param) *
          delta_param);
        j += 2;
      end;
      tool.AfterDraw;
    end;
    tool.Free;
  end;
  LoadColors;
  FPaintBox.Invalidate;
end;

procedure TToolsDataUtils.Add(Name, wdt: integer; Pcolor, Bcolor: tcolor;
  Style: TBrushStyle);
begin
  if position < length(Data) - 1 then
    SetLength(Data, Position + 1 + 1)
  else
    setlength(Data, length(Data) + 1);
  position += 1;
  with Data[high(Data)] do begin
    NofTool := Name;
    PenColor := Pcolor;
    BrushColor := Bcolor;
    BrushStyle := Style;
    Width := wdt;
  end;
end;

procedure TToolsDataUtils.AddCoords(var Coordinates: Coords);
begin
  Data[high(Data)].Coord := Coordinates;
end;

procedure TToolsDataUtils.Undo(x_param, y_param, delta_param: integer);
begin
  if position > -1 then
    position -= 1;
  ShowHistory(x_param, y_param, delta_param);
end;

procedure TToolsDataUtils.Redo(x_param, y_param, delta_param: integer);
begin
  if position < high(Data) then
    position += 1;
  ShowHistory(x_param, y_param, delta_param);
end;

function TToolsDataUtils.GetLength: integer;
begin
  Result := length(Data);
end;

function TToolsDataUtils.GetData(i: integer): ToolData;
begin
  Result := Data[i];
end;

function TToolsDataUtils.IsFigureExists(x, y, inaccuracy: integer): integer;
var
  i, j: integer;
begin
  for i := 0 to position do begin
    j := 0;
    with Data[i] do begin
      while j <= high(coord) do begin
        if (abs(x - Coord[j]) <= inaccuracy) and
          (abs(y - Coord[j + 1]) <= inaccuracy) then begin
          Result := i;
          exit;
        end;
        j += 2;
      end;
    end;
  end;
  Result := -1;
end;

procedure TToolsDataUtils.HighLightFigure(num: integer);
var
  i, x1, x2, y1, y2: integer;
begin
  with Data[num] do begin
    x1 := 99999;
    y1 := 99999;
    x2 := -99999;
    y2 := -99999;
    i := 2;
    while i <= high(Coord) do begin
      if Coord[i] < x1 then
        x1 := Coord[i];
      if Coord[i] > x2 then
        x2 := Coord[i];
      if Coord[i + 1] < y1 then
        y1 := Coord[i + 1];
      if Coord[i + 1] > y2 then
        y2 := Coord[i + 1];
      i += 2;
    end;
  end;
  SaveColors;
  FCanvas.Canvas.Pen.Style := psDashDot;
  FCanvas.Canvas.pen.Color := clRed;
  FCanvas.Canvas.Rectangle(x1 - 3, y1 - 3, x2 + 3, y2 + 3);
  FCanvas.Canvas.Pen.Style := psSolid;
  LoadColors;
end;

procedure TToolsDataUtils.MovFigure(num, x_offset, y_offset: integer);
var
  i: integer;
begin
  with Data[num] do begin
    i := 0;
    while i <= high(Coord) do begin
      Coord[i] += x_offset;
      Coord[i + 1] += y_offset;
    end;
  end;
end;

procedure TToolsDataUtils.SaveColors;
begin
  with FCanvas.Canvas do begin
    PrevPWidth := Pen.Width;
    PrevPColor := pen.color;
    PrevBColor := brush.color;
    PrevBStyle := brush.style;
  end;
end;

procedure TToolsDataUtils.LoadColors;
begin
  with FCanvas.Canvas do begin
    pen.Width := PrevPWidth;
    pen.color := PrevPColor;
    brush.color := PrevBColor;
    Brush.Style := PrevBStyle;
  end;
end;

initialization
  Init(TPen, 'Pen');
  Init(TFill, 'Fill');
  Init(TRectangle, 'Rectangle');
  Init(TEllipse, 'Ellipse');
  Init(TZoom, 'Zoom');
  Init(TSelect, 'Select');
  Init(TArm, 'Arm');
end.
