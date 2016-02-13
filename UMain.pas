unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, ExtDlgs, UTools, UZoom, UPalette,
  UObjectMove, UHistory, UToolsPanel, UToolsManager;

type

  { TMainForm }

  BrushStyleCbox = record
    Name: string;
    Style: TBrushStyle;
  end;

  TMainForm = class(TForm)
    ColorDialog: TColorDialog;
    ExportToSvg: TMenuItem;
    ToolsIconList: TImageList;
    MainMenu, EditMenu, FileItem: TMainMenu;
    Undo, Redo, SaveAs, OpenAs, CloseProgram: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    PaintBox: TPaintBox;
    SavePictureDialog: TSavePictureDialog;
    FCanvas: tbitmap;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);

    procedure RedoClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);

    procedure ExportClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure CloseProgramClick(Sender: TObject);
    procedure OpenAsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  isDrawing, isMoving: boolean;

  palette: TPalette;
  PanelMove: TObjectMove;
  ToolsPanel: TToolsPanel;

implementation

{$R *.lfm}
{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanvas := tbitmap.Create;
  Zoom := TZoom.Create(self, FCanvas);
  palette := Tpalette.Create(self);
  PanelMove := TObjectMove.Create(MainForm.Width, MainForm.Height);
  History := THistory.Create(PaintBox, FCanvas);
  ToolsPanel := TToolsPanel.Create(self, ToolsIconList);
  ToolsManager := TToolsManager.Create(FCanvas);

  palette.OnMouseDown := @PanelMove.OnMouseDown;
  palette.OnMouseMove := @PanelMove.OnMouseMove;
  palette.OnMouseUp := @PanelMove.OnMouseUp;
  ToolsPanel.OnMouseDown := @PanelMove.OnMouseDown;
  ToolsPanel.OnMouseMove := @PanelMove.OnMouseMove;
  ToolsPanel.OnMouseUp := @PanelMove.OnMouseUp;

  FCanvas.Width := PaintBox.Width;
  FCanvas.Height := PaintBox.Height;
  FCanvas.canvas.pen.color := clWhite;
  FCanvas.canvas.rectangle(0, 0, FCanvas.Width, FCanvas.Height);
  FCanvas.canvas.brush.Style := bsClear;
  FCanvas.canvas.pen.color := clBlack;
end;
//---------------------------PAINT-BOX------------------------------------------
procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if ToolsManager.isZoomSelected = True then begin
    if button = mbLeft then
      zoom.zoomin(x,y);
    if button = mbRight then
      zoom.zoomout(x,y);
    ToolsManager.isZoomSelected := True;
  end
  else begin
    if Zoom.GetZoomValue > 0 then begin
      x := Zoom.GetGlobalX(x);
      y := Zoom.GetGlobalY(y);
    end;
    ToolsManager.tool.BeforeDraw(x,y);
    with FCanvas.canvas do
      History.Add(ToolsManager.tooltag, pen.Width, pen.color, Brush.Color, brush.Style);
    History.AddCoords(ToolsManager.tool.Coord);
    isDrawing := True;
  end;
  paintbox.invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if isDrawing = True then begin
    if Zoom.GetZoomValue > 0 then begin
      x := Zoom.GetGlobalX(x);
      y := Zoom.GetGlobalY(y);
    end;
    ToolsManager.tool.Draw(x, y);
    History.AddCoords(ToolsManager.tool.Coord);
    PaintBox.Invalidate;
    if Zoom.GetZoomValue > 0 then
      with zoom do
        History.Show(z_px, z_py, z_n);
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if isDrawing = True then begin
    ToolsManager.tool.AfterDraw;
    isDrawing := False;
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  r: trect;
begin
  r := bounds(0, 0, FCanvas.Width, FCanvas.Height);
  PaintBox.Canvas.CopyRect(rect(0, 0, PaintBox.Width, PaintBox.Height),
    FCanvas.canvas, r);
end;
//-----------------------------CTRL-Z-------------------------------------------
procedure TMainForm.UndoClick(Sender: TObject);
begin
  with zoom do
    History.Undo(z_px, z_py, z_n);
  paintbox.invalidate;
end;
//-----------------------------CTRL-Y-------------------------------------------
procedure TMainForm.RedoClick(Sender: TObject);
begin
  with zoom do
    History.Redo(z_px, z_py, z_n);
  paintbox.invalidate;
end;
//-----------------------------DIALOGS----------------------------------------
procedure TMainForm.SaveAsClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    FCanvas.SaveToFile(SavePictureDialog.filename);
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  halt;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    FCanvas.LoadFromFile(OpenPictureDialog.FileName);
  PaintBox.Invalidate;
end;

procedure TMainForm.ExportClick(Sender: TObject);
var
  filename: string;
  output: textfile;
  i: integer;
  ETool: TTools;
  EData: ToolData;
begin
  SavePictureDialog.Filter := 'Vector graphics|*.svg';
  if SavePictureDialog.Execute then begin
    filename := SavePictureDialog.FileName;
    assignfile(output, filename);
    Rewrite(output);
    WriteLn(output, '<?xml version="1.0" standalone="no"?>');
    writeln(output, '<svg width="', FCanvas.Width, 'mm" height="',
      FCanvas.Height, 'mm" viewBox="0 0 ', FCanvas.Width, ' ', FCanvas.Height,
      '" xmlns="http://www.w3.org/2000/svg" version="1.1">');
    for i := 0 to History.GetLength - 1 do begin
      EData := History.GetData(i);
      Etool := classref[EData.Noftool].Tool.Create(FCanvas);
      ETool.ExportToSvg(output, EData);
      ETool.Free;
    end;
    writeln(output, '</svg>');
    closefile(output);
  end;
end;

end.
