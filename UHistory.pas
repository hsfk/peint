unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UTools, Dialogs, UToolsManager,ExtCtrls,Controls;

type

  THistory = class
  public
    constructor Create(var PPAintBox : TPaintbox;var Pcanvas: tbitmap);
    procedure Show(x_param, y_param, delta_param: integer);
    procedure Add(Name, wdt: integer; Pcolor, Bcolor: tcolor; Style: TBrushStyle);
    procedure AddCoords(var Coordinates: Coords);
    procedure Undo(x_param, y_param, delta_param: integer);
    procedure Redo(x_param, y_param, delta_param: integer);
    function GetLength : integer;
    function GetData(i : integer) : ToolData;
  private
    FPaintBox : TPaintbox;
    FCanvas: tbitmap;
    Data: array of ToolData;
    position: integer;
  end;

var History: THistory;
implementation

constructor THistory.Create(var PPAintBox : TPaintbox;var Pcanvas: tbitmap);
begin
  position := -1;
  FCanvas := PCanvas;
  FPaintBox := PPAintBox;
end;

procedure THistory.Show(x_param, y_param, delta_param: integer);
var
  i, j, PrevPWidth: integer;
  tool: TTools;
  PrevPColor, PrevBColor: TColor;
  PrevBStyle: TBrushStyle;
begin
  delta_param := 1 shl delta_param;
  with FCanvas.Canvas do begin
    PrevPWidth := Pen.Width;
    PrevPColor := pen.color;
    PrevBColor := brush.color;
    PrevBStyle := brush.style;
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
        delta_param);
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
  with ToolsManager.FCanvas.Canvas do begin
    pen.Width := PrevPWidth;
    pen.color := PrevPColor;
    brush.color := PrevBColor;
    Brush.Style := PrevBStyle;
  end;
  FPaintBox.Invalidate;
end;

procedure THistory.Add(Name, wdt: integer; Pcolor, Bcolor: tcolor; Style: TBrushStyle);
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

procedure THistory.AddCoords(var Coordinates: Coords);
begin
  Data[high(Data)].Coord := Coordinates;
end;

procedure THistory.Undo(x_param, y_param, delta_param: integer);
begin
  if position > -1 then
    position -= 1;
  Show(x_param, y_param, delta_param);
end;

procedure THistory.Redo(x_param, y_param, delta_param: integer);
begin
  if position < high(Data) then
    position += 1;
  Show(x_param, y_param, delta_param);
end;

function THistory.GetLength : integer;
begin
  result := length(Data);
end;

function THistory.GetData(i : integer) : ToolData;
begin
  result := Data[i];
end;

end.
