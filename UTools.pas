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
    BrushColor: TColor;
    BrushStyle: TBrushStyle;
    Width: integer;
    ToolCoords: Coords;
  end;

  ClassOfInstrument = class of TTools;

  ToolName = record
    Tool: ClassOFInstrument;
    NameOfTool: string;
  end;

  TTools = class
  public
    ToolCoords: Coords;
    constructor Create(var SceneToDraw: TBitmap);
    procedure ShowHistoryWhenZoomed; virtual;
    procedure ToGlobalCoords(var X, Y: integer); virtual;
    procedure AddToHistory(ToolTag: integer; Scene: tbitmap); virtual;
    procedure AddCoordsToHistory(Coordinates: Coords); virtual;
    procedure BeforeDraw(X, Y: integer; Button: TMouseButton); virtual;
    procedure Draw(X, Y: integer); virtual; abstract;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData); virtual; abstract;
    procedure AfterDraw; virtual;
  private
    FScene: tbitmap;
    FTmpScene: tbitmap;
  end;

  TBufferRequired = class(TTools)
  public
    procedure Draw(X, Y: integer); override; abstract;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData);
      override; abstract;
  end;

  TBufferNotRequired = class(TTools)
  public
    procedure Draw(X, Y: integer); override; abstract;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData);
      override; abstract;
  end;

  TSpecTools = class(TTools)
  public
    procedure AddToHistory(ToolTag: integer; Canvas_: tbitmap); override;
    procedure AddCoordsToHistory(Coordinates: Coords); override;
    procedure Draw(X, Y: integer); override;
  end;

  TZoom = class(TSpecTools)
  public
    procedure ToGlobalCoords(var X, Y: integer); override;
    procedure BeforeDraw(X, Y: integer; Button: TMouseButton); override;
    procedure ShowHistoryWhenZoomed; override;
  end;

  TSelect = class(TSpecTools)
  public
    procedure BeforeDraw(X, Y: integer; Button: TMouseButton); override;
  end;

  TArm = class(TSpecTools)
  public
    procedure BeforeDraw(X, Y: integer; Button: TMouseButton); override;
    procedure Draw(X, Y: integer); override;
    procedure AfterDraw; override;
    procedure ShowHistoryWhenZoomed; override;
  private
    PrevX: integer;
    PrevY: integer;
    XOffset: integer;
    YOffset: integer;
  end;

  TRectangle = class(TBufferRequired)
  public
    procedure Draw(X, Y: integer); override;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData); override;
  end;

  TEllipse = class(TBufferRequired)
  public
    procedure Draw(X, Y: integer); override;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData); override;
  end;

  TPen = class(TBufferNotRequired)
  public
    procedure Draw(X, Y: integer); override;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData); override;
  end;

  TFill = class(TBufferNotRequired)
  public
    procedure Draw(X, Y: integer); override;
    procedure BeforeDraw(X, Y: integer; Button: TMouseButton); override;
    procedure ExportToSvg(var Output: TextFile; Data: ToolData); override;
  end;

  TToolsDataUtils = class
  public
    constructor Create(var PaintBox_: TPaintbox; var Scene: tbitmap);
    procedure ShowHistory(X_, Y_: integer);
    procedure Add(NofTool_, Width_: integer; PenColor_, BrushColor_: tcolor;
      BrushStyle_: TBrushStyle);
    procedure Delete(Num: integer);
    procedure AddCoords(var Coordinates: Coords);
    procedure Undo(X_, Y_: integer);
    procedure Redo(X_, Y_: integer);
    function GetLength: integer;
    function GetData(i: integer): ToolData;
    function GetPosition: integer;
    function IsFigureExists(X, Y, inaccuracy: integer): integer;
    procedure HighLightFigure(num: integer);
  private
    PrevPenColor: TColor;
    PrevBrushColor: TColor;
    PrevBrushStyle: TBrushStyle;
    FPaintBox: TPaintbox;
    FScene: tbitmap;
    FData: array of ToolData;
    HistoryPos: integer;
    PrevPWidth: integer;
    procedure SaveColors;
    procedure LoadColors;
  end;

var
  ToolsDataUtils: TToolsDataUtils;
  ClassRef: array of ToolName;
  NULL: integer;

implementation

function ColorToHex(color: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x', [byte(color), byte(color shr 8),
    byte(color shr 16)]);
end;

constructor TTools.Create(var SceneToDraw: TBitmap);
begin
  FScene := SceneToDraw;
end;

procedure TTools.ShowHistoryWhenZoomed;
begin
  if Zoom.ZoomValue > 0 then
    ToolsDataUtils.ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
end;

procedure TTools.AddToHistory(ToolTag: integer; Scene: tbitmap);
begin
  with Scene.Canvas do begin
    ToolsDataUtils.Add(ToolTag, pen.Width, pen.color, Brush.Color, brush.Style);
    ToolsDataUtils.AddCoords(ToolCoords);
  end;
end;

procedure TTools.AddCoordsToHistory(Coordinates: Coords);
begin
  ToolsDataUtils.AddCoords(Coordinates);
end;

procedure TTools.BeforeDraw(X, Y: integer; Button: TMouseButton);
var
  Rect_: trect;
begin
  Setlength(ToolCoords, 2);
  ToolCoords[0] := X;
  ToolCoords[1] := Y;
  FTmpScene := tbitmap.Create;
  FTmpScene.Width := FScene.Width;
  FTmpScene.Height := FScene.Height;
  Rect_ := bounds(0, 0, FScene.Width, FScene.Height);
  FTmpScene.Canvas.CopyRect(rect(0, 0, FTmpScene.Width, FTmpScene.Height),
    FScene.canvas, Rect_);
end;

procedure TTools.AfterDraw;
begin
  FTmpScene.Free;
end;

procedure TTools.ToGlobalCoords(var X, Y: integer);
begin
  X := Zoom.GetGlobalX(X);
  Y := Zoom.GetGlobalY(Y);
end;

{ Tools }
procedure TZoom.BeforeDraw(X, Y: integer; Button: TMouseButton);
begin
  Zoom.SetZoom(X, Y);
  if Button = mbLeft then begin
    Zoom.ZoomIn;
    ToolsDataUtils.ShowHistory(Zoom.CurrentX, Zoom.CurrentY);
  end;
  if Button = mbRight then begin
    Zoom.ZoomOut;
    ToolsDataUtils.ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
  end;
end;

procedure TSelect.BeforeDraw(X, Y: integer; Button: TMouseButton);
var
  fig: integer;
begin
  fig := ToolsDataUtils.IsFigureExists(X, Y, 3);
  if fig <> -1 then
    ToolsDataUtils.HighLightFigure(fig)
  else
    ToolsDataUtils.ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
end;

procedure TArm.BeforeDraw(X, Y: integer; Button: TMouseButton);
begin
  PrevX := X;
  PrevY := Y;
end;

procedure TArm.Draw(X, Y: integer);
var
  X_: integer;
  Y_: integer;
begin
  XOffset := (PrevX - X);
  YOffset := (PrevY - Y);
  X_ := Zoom.PreviousX + XOffset;
  Y_ := Zoom.PreviousY + YOffset;
  Zoom.CoordAllignment(X_, NULL, Y_, NULL);
  ToolsDataUtils.ShowHistory(X_, Y_);
end;

procedure TArm.AfterDraw;
begin
  inherited AfterDraw;
  Zoom.PreviousX += XOffset;
  Zoom.PreviousY += YOffset;
  XOffset := 0;
  YOffset := 0;
end;

procedure TRectangle.Draw(X, Y: integer);
var
  Rect_: trect;
  X_, y_: integer;
begin
  Setlength(ToolCoords, 4);
  ToolCoords[2] := X;
  ToolCoords[3] := Y;
  Rect_ := bounds(0, 0, FTmpScene.Width, FTmpScene.Height);
  FScene.Canvas.CopyRect(Rect(0, 0, FScene.Width, FScene.Height),
    FTmpScene.canvas, Rect_);
  X_ := ToolCoords[0];
  y_ := ToolCoords[1];
  Zoom.CoordAllignment(X_, X, y_, Y);
  FScene.Canvas.Rectangle(X_, y_, X, Y);
end;

procedure TRectangle.ExportToSvg(var output: TextFile; Data: ToolData);
var
  X: integer;
  Y: integer;
  Width: integer;
  Height: integer;
begin
  if Data.ToolCoords[0] > Data.ToolCoords[2] then begin
    X := Data.ToolCoords[2];
    Width := Data.ToolCoords[0] - X;
  end
  else begin
    X := Data.ToolCoords[0];
    Width := Data.ToolCoords[2] - X;
  end;

  if Data.ToolCoords[1] > Data.ToolCoords[3] then begin
    Y := Data.ToolCoords[3];
    Height := Data.ToolCoords[1] - Y;
  end
  else begin
    Y := Data.ToolCoords[1];
    Height := Data.ToolCoords[3] - Y;
  end;
  writeln(output, '  <rect x="', X, '" y="', Y, '" width="', Width, '" height="',
    Height, '" fill="', ColorToHex(Data.BrushColor), '" stroke="',
    ColorToHex(Data.PenColor), '" stroke-width="', Data.Width, '"  />');
end;

procedure TEllipse.Draw(X, Y: integer);
var
  Rect_: trect;
  X_: integer;
  Y_: integer;
begin
  Setlength(ToolCoords, 4);
  ToolCoords[2] := X;
  ToolCoords[3] := Y;
  Rect_ := bounds(0, 0, FTmpScene.Width, FTmpScene.Height);
  FScene.Canvas.CopyRect(Rect(0, 0, FScene.Width, FScene.Height),
    FTmpScene.canvas, Rect_);
  X_ := ToolCoords[0];
  Y_ := ToolCoords[1];
  Zoom.CoordAllignment(X_, X, Y_, Y);
  FScene.Canvas.Ellipse(X_, Y_, X, Y);
end;

procedure TEllipse.ExportToSvg(var output: TextFile; Data: ToolData);
var
  CX: integer;
  CY: integer;
  RX: integer;
  RY: integer;
begin
  CX := (Data.ToolCoords[0] + Data.ToolCoords[2]) div 2;
  CY := (Data.ToolCoords[1] + Data.ToolCoords[3]) div 2;
  RX := abs(Data.ToolCoords[2] - CX);
  RY := abs(Data.ToolCoords[3] - CY);
  writeln(output, '  <ellipse cx="', CX, '" cy="', CY, '" rx="', RX, '" ry="', RY,
    '" fill="', ColorToHex(Data.BrushColor), '" stroke="', ColorToHex(
    Data.PenColor), '" stroke-width="', Data.Width, '"  />');
end;

procedure TPen.Draw(X, Y: integer);
var
  PreX: integer;
  PreY: integer;
begin
  PreX := ToolCoords[high(ToolCoords) - 1];
  PreY := ToolCoords[high(ToolCoords)];
  Setlength(ToolCoords, length(ToolCoords) + 2);
  ToolCoords[high(ToolCoords) - 1] := X;
  ToolCoords[high(ToolCoords)] := Y;
  Zoom.CoordAllignment(PreX, X, PreY, Y);
  FScene.canvas.line(PreX, PreY, X, Y);
end;

procedure TPen.ExportToSvg(var output: TextFile; Data: ToolData);
var
  i: integer;
begin
  writeln(output, '  <polyline fill="none" stroke="', ColorToHex(
    Data.PenColor), '" stroke-width="', Data.Width, '"');
  writeln(output, '  points="');
  i := 0;
  while i <= high(Data.ToolCoords) do begin
    Write(output, Data.ToolCoords[i], ',', Data.ToolCoords[i + 1], ' ');
    i += 2;
  end;
  Write(output, '" />');
end;

procedure TFill.BeforeDraw(X, Y: integer; Button: TMouseButton);
var
  PixelColor: TColor;
begin
  inherited BeforeDraw(X, Y, Button);
  PixelColor := FScene.canvas.Pixels[X, Y];
  Zoom.CoordAllignment(NULL, X, NULL, Y);
  FScene.canvas.FloodFill(X, Y, PixelColor, TFillStyle.fsSurface);
end;

procedure TFill.Draw(X, Y: integer);
begin { nothing }
end;

procedure TFill.ExportToSvg(var Output: TextFile; Data: ToolData);
begin { nothing }
end;

procedure TSpecTools.AddToHistory(ToolTag: integer; Canvas_: tbitmap);
begin { nothing }
end;

procedure TSpecTools.AddCoordsToHistory(Coordinates: Coords);
begin { nothing }
end;

procedure TSpecTools.Draw(X, Y: integer);
begin { nothing }
end;

procedure TZoom.ToGlobalCoords(var X, Y: integer);
begin { nothing }
end;

procedure TZoom.ShowHistoryWhenZoomed;
begin { nothing }
end;

procedure TArm.ShowHistoryWhenZoomed;
begin { nothing }
end;

{ ToolsDataUtils }
constructor TToolsDataUtils.Create(var PaintBox_: TPaintbox; var Scene: tbitmap);
begin
  HistoryPos := -1;
  FScene := Scene;
  FPaintBox := PaintBox_;
end;

procedure TToolsDataUtils.ShowHistory(X_, Y_: integer);
var
  i: integer;
  j: integer;
  Tool: TTools;
  NullBtn: TmouseButton;
  Delta_: integer;
begin
  Delta_ := 1 shl Zoom.ZoomValue;
  NullBtn := mbRight;
  SaveColors;
  FScene.Canvas.pen.color := clWhite;
  FScene.Canvas.brush.Color := clWhite;
  FScene.Canvas.brush.style := bsSolid;
  FScene.Canvas.rectangle(0, 0, FScene.Width, FScene.Height);
  for i := 0 to HistoryPos do begin
    Tool := classref[FData[i].Noftool].Tool.Create(FScene);
    FScene.Canvas.pen.color := FData[i].PenColor;
    FScene.Canvas.brush.Color := FData[i].BrushColor;
    FScene.Canvas.brush.Style := FData[i].BrushStyle;
    FScene.Canvas.pen.Width := FData[i].Width;
    Tool.BeforeDraw((FData[i].ToolCoords[0] - X_) * Delta_,
      (FData[i].ToolCoords[1] - Y_) * Delta_, NullBtn);
    j := 2;
    while j <= high(FData[i].ToolCoords) do begin
      Tool.Draw((FData[i].ToolCoords[j] - X_) * Delta_,
        (FData[i].ToolCoords[j + 1] - Y_) * Delta_);
      j += 2;
    end;
    Tool.AfterDraw;
    Tool.Free;
  end;
  LoadColors;
  FPaintBox.Invalidate;
end;

procedure TToolsDataUtils.Add(NofTool_, Width_: integer;
  PenColor_, BrushColor_: tcolor; BrushStyle_: TBrushStyle);
begin
  if HistoryPos < length(FData) - 1 then
    SetLength(FData, HistoryPos + 1 + 1)
  else
    SetLength(FData, length(FData) + 1);
  HistoryPos += 1;
  FData[high(FData)].NofTool := NofTool_;
  FData[high(FData)].PenColor := PenColor_;
  FData[high(FData)].BrushColor := BrushColor_;
  FData[high(FData)].BrushStyle := BrushStyle_;
  FData[high(FData)].Width := Width_;
end;

procedure TToolsDataUtils.Delete(Num: integer);
var
  i: integer;
begin
  if num > -1 then begin
    for i := num to length(FData) - 2 do
      FData[i] := FData[i + 1];
    HistoryPos -= 1;
    SetLength(FData, length(FData) - 1);
    ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
  end;
end;

procedure TToolsDataUtils.AddCoords(var Coordinates: Coords);
begin
  FData[high(FData)].ToolCoords := Coordinates;
end;

procedure TToolsDataUtils.Undo(X_, Y_: integer);
begin
  if HistoryPos > -1 then
    HistoryPos -= 1;
  Zoom.CoordAllignment(X_, NULL, Y_, NULL);
  ShowHistory(X_, Y_);
end;

procedure TToolsDataUtils.Redo(X_, Y_: integer);
begin
  if HistoryPos < high(FData) then
    HistoryPos += 1;
  Zoom.CoordAllignment(X_, NULL, Y_, NULL);
  ShowHistory(X_, Y_);
end;

function TToolsDataUtils.GetLength: integer;
begin
  Result := length(FData);
end;

function TToolsDataUtils.GetData(i: integer): ToolData;
begin
  Result := FData[i];
end;

function TToolsDataUtils.GetPosition: integer;
begin
  Result := HistoryPos;
end;

function TToolsDataUtils.IsFigureExists(X, Y, inaccuracy: integer): integer;
var
  i: integer;
  j: integer;
begin
  for i := 0 to HistoryPos do begin
    j := 0;
    if length(FData[i].ToolCoords) = 4 then begin
      if (X >= FData[i].ToolCoords[0]) and (X <= FData[i].ToolCoords[2]) then
        if (Y >= FData[i].ToolCoords[1]) and (Y <= FData[i].ToolCoords[3]) then begin
          Result := i;
          Exit;
        end;
    end
    else begin
      while j <= high(FData[i].ToolCoords) do begin
        if (abs(X - FData[i].ToolCoords[j]) <= inaccuracy) and
          (abs(Y - FData[i].ToolCoords[j + 1]) <= inaccuracy) then begin
          Result := i;
          Exit;
        end;
        j += 2;
      end;
    end;
  end;
  Result := -1;
end;

procedure TToolsDataUtils.HighLightFigure(num: integer);
var
  i: integer;
  X1: integer;
  X2: integer;
  Y1: integer;
  Y2: integer;
  Delta_: integer;
begin
  if length(FData) > 0 then begin
    Delta_ := 1 shl Zoom.ZoomValue;
    with FData[num] do begin
      X1 := 99999;
      Y1 := 99999;
      X2 := -99999;
      Y2 := -99999;
      i := 2;
      if length(ToolCoords) = 4 then begin
        X1 := ToolCoords[0];
        Y1 := ToolCoords[1];
        X2 := ToolCoords[2];
        Y2 := ToolCoords[3];
      end
      else begin
        while i <= high(ToolCoords) do begin
          if ToolCoords[i] < X1 then
            X1 := ToolCoords[i];
          if ToolCoords[i] > X2 then
            X2 := ToolCoords[i];
          if ToolCoords[i + 1] < Y1 then
            Y1 := ToolCoords[i + 1];
          if ToolCoords[i + 1] > Y2 then
            Y2 := ToolCoords[i + 1];
          i += 2;
        end;
      end;
    end;
    SaveColors;
    FScene.Canvas.Pen.Style := psDashDot;
    FScene.Canvas.Pen.Width := 3;
    FScene.Canvas.pen.Color := clRed;
    X1 := (X1 - Zoom.PreviousX) * Delta_;
    X2 := (X2 - Zoom.PreviousX) * Delta_;
    Y1 := (Y1 - Zoom.PreviousY) * Delta_;
    Y2 := (Y2 - Zoom.PreviousY) * Delta_;
    FScene.Canvas.Rectangle(X1 - 3, Y1 - 3, X2 + 3, Y2 + 3);
    FScene.Canvas.Pen.Style := psSolid;
    LoadColors;
    FPaintBox.Invalidate;
  end;
end;

procedure TToolsDataUtils.SaveColors;
begin
  PrevPWidth := FScene.Canvas.Pen.Width;
  PrevPenColor := FScene.Canvas.Pen.color;
  PrevBrushColor := FScene.Canvas.Brush.Color;
  PrevBrushStyle := FScene.Canvas.Brush.style;
end;

procedure TToolsDataUtils.LoadColors;
begin
  FScene.Canvas.Pen.Width := PrevPWidth;
  FScene.Canvas.Pen.color := PrevPenColor;
  FScene.Canvas.Brush.Color := PrevBrushColor;
  FScene.Canvas.Brush.Style := PrevBrushStyle;
end;

procedure Init(Tool: ClassOfInstrument; NameOfTool: string);
begin
  SetLength(ClassRef, length(ClassRef) + 1);
  ClassRef[high(ClassRef)].Tool := Tool;
  ClassRef[high(ClassRef)].NameOfTool := NameOfTool;
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
