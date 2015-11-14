unit Instruments;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs;

type
  main = class
    PreX, PreY, CurX, CurY: integer;
    holst: tbitmap;
    bufferHolst: tbitmap;
  public
    procedure BeforeDraw(x, y: integer); virtual;
    procedure Draw(x, y: integer); virtual; abstract;
    constructor Create(holstToDraw: TBitmap);
    procedure CtrlZ(); virtual; abstract;
    procedure ZInit(Name: main; Pcolor, Bcolor: tcolor; TWid, X, Y: integer);
      virtual; abstract;
    procedure CoordInc(x, y: integer); virtual; abstract;
    procedure Zshow(i: integer); virtual; abstract;
  end;

  Zdata = record
    NofTool: main;
    PenColor: TColor;
    BrushColor: Tcolor;
    TWidth: integer;
    Coord: array of integer;
  end;

  BufferRequired = class(main)
  public
    procedure Draw(x, y: integer); override; abstract;
    procedure ZInit(Name: main; Pcolor, Bcolor: Tcolor; Twid, X, Y: integer); override;
    procedure CoordInc(x, y: integer); override;
    procedure Zshow(i: integer); override;
  end;


  BufferNotRequired = class(main)
  public
    procedure Draw(x, y: integer); override; abstract;
    procedure ZInit(Name: main; Pcolor, Bcolor: tcolor; TWid, X, Y: integer); override;
    procedure CoordInc(x, y: integer); override;
    procedure Zshow(i: integer); override;
  end;
  //----------------------------------------//
  Rectangle = class(BufferRequired)
  public
    procedure Draw(x, y: integer); override;
  end;

  Ellipse = class(BufferRequired)
  public
    procedure Draw(x, y: integer); override;
  end;

  Pen = class(BufferNotRequired)
  public
    procedure Draw(x, y: integer); override;
  end;

  Fill = class(BufferNotRequired)
  public
    procedure Draw(x, y: integer); override;
    procedure BeforeDraw(x, y: integer); override;
  end;

  ReferenceClass = class of main;

  InstrName = record
    instrument: ReferenceClass;
    nameofinstr: string;
  end;

var
  ClassRef: array of InstrName;     //MAINVAR
  ZArr: array of Zdata;

implementation

procedure BufferNotRequired.ZInit(Name: main; Pcolor, Bcolor: tcolor;
  TWid, X, Y: integer);
begin
  SetLength(Zarr, length(Zarr) + 1);
  with Zarr[high(Zarr)] do
  begin
    NofTool := Name;
    PenColor := Pcolor;
    BrushColor := Bcolor;
    TWidth := Twid;
    setlength(Coord, length(coord) + 2);
    Coord[high(Coord) - 1] := X;
    Coord[high(Coord) - 0] := Y;
  end;
end;

procedure BufferNotRequired.CoordInc(x, y: integer);
begin
  with ZArr[high(Zarr)] do
  begin
    setlength(Coord, length(coord) + 2);
    Coord[high(Coord) - 1] := X;
    Coord[high(Coord) - 0] := Y;
  end;
end;

procedure BufferNotRequired.Zshow(i: integer);
var
  j: integer;
begin
  with ZArr[i] do
  begin
    holst.canvas.pen.color := PenColor;
    holst.canvas.brush.color := BrushColor;
    holst.canvas.pen.Width := TWidth;
    NofTool.PreX := coord[0];
    NofTool.PreY := coord[1];
    j := 2;
    while j <= high(coord) do
    begin
      NofTool.Draw(coord[j], coord[j + 1]);
      j += 2;
    end;
  end;
end;

procedure BufferRequired.ZInit(Name: main; Pcolor, Bcolor: tcolor; TWid, X, Y: integer);
begin
  setlength(Zarr, length(Zarr) + 1);
  with Zarr[high(Zarr)] do
  begin
    NofTool := Name;
    PenColor := Pcolor;
    BrushColor := Bcolor;
    TWidth := Twid;
    setlength(coord, 4);
    Coord[0] := X;
    Coord[1] := Y;
  end;
end;

procedure BufferRequired.CoordInc(x, y: integer);
begin
  with ZArr[high(Zarr)] do
  begin
    Coord[2] := X;
    Coord[3] := Y;
  end;
end;

procedure BufferRequired.Zshow(i: integer);
var
  j: integer;
begin
  with ZArr[i] do
  begin
    holst.canvas.pen.color := PenColor;
    holst.canvas.brush.color := BrushColor;
    holst.canvas.pen.Width := TWidth;
    NofTool.PreX := coord[0];
    NofTool.PreY := coord[1];
    NofTool.BeforeDraw(coord[0], coord[1]);
    NofTool.Draw(coord[2], coord[3]);
  end;
end;

constructor main.Create(holstToDraw: TBitmap);
begin
  holst := holstToDraw;
end;

procedure main.BeforeDraw(x, y: integer);
var
  r: trect;
begin
  PreX := x;
  PreY := y;
  bufferHolst := tbitmap.Create;
  bufferholst.Width := holst.Width;
  bufferholst.Height := holst.Height;
  r := bounds(0, 0, holst.Width, holst.Height);
  bufferHolst.Canvas.CopyRect(rect(0, 0, bufferHolst.Width, bufferHolst.Height),
    holst.canvas, r); {some procedure}
end;

procedure Rectangle.Draw(x, y: integer);
var
  r: trect;
begin
  r := bounds(0, 0, bufferholst.Width, bufferholst.Height);
  holsT.Canvas.CopyRect(rect(0, 0, holsT.Width, holsT.Height), bufferholst.canvas, r);
  holst.Canvas.Rectangle(PreX, PreY, X, Y);
end;

procedure Ellipse.Draw(x, y: integer);
var
  r: trect;
begin
  r := bounds(0, 0, bufferholst.Width, bufferholst.Height);
  holsT.Canvas.CopyRect(rect(0, 0, holsT.Width, holsT.Height), bufferholst.canvas, r);
  holst.Canvas.Ellipse(PreX, PreY, x, y);
end;

procedure pen.Draw(x, y: integer);
begin
  holst.canvas.line(PreX, PreY, x, y);
  PreX := x;
  PreY := y;
end;

procedure fill.BeforeDraw(x, y: integer);
begin
  inherited BeforeDraw(x, y);
  holst.canvas.FloodFill(x, y, holst.canvas.pen.color, TFillStyle.fsBorder);
end;

procedure fill.draw(x, y: integer);
begin
end;

procedure Init(instrument: ReferenceClass; nameofinstr: string);
var
  i: integer;
begin
  setlength(ClassRef, length(ClassRef) + 1);
  classref[high(ClassRef)].instrument := instrument;
  classref[high(ClassRef)].nameofinstr := nameofinstr;
end;

initialization
  Init(pen, 'Pen');
  Init(fill, 'Fill');
  Init(rectangle, 'Rectangle');
  Init(ellipse, 'Ellipse');
end.
