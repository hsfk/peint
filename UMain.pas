unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, ExtDlgs, UTools, UZoom, UPalette,
  UObjectMove, UToolsPanel, UToolsManager, UScrollBar,UFigureHistoryManager;

type

  { TMainForm }

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
  ScrollBar : TZoomScrollBar;
  FigureManager : TFigureHistoryManager;
implementation

{$R *.lfm}
{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanvas := tbitmap.Create;
  Zoom := TZoom.Create(self);
  palette := Tpalette.Create(self);             {15 - scroll bar width }
  PanelMove := TObjectMove.Create(MainForm.Width - 15, MainForm.Height - 15);
  ToolsPanel := TToolsPanel.Create(self, ToolsIconList);
  ToolsManager := TToolsManager.Create(FCanvas);
  ToolsDataUtils := TToolsDataUtils.Create(PaintBox, FCanvas);
  ScrollBar := TZoomScrollBar.Create(self);
  FigureManager := TFigureHistoryManager.Create(self);

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

  ToolsManager.FTool := TPen.Create(FCanvas);
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Zoom.GetZoomValue > 0 then begin
    x := Zoom.GetGlobalX(x);
    y := Zoom.GetGlobalY(y);
  end;
  ToolsManager.FTool.BeforeDraw(x, y, Button);
  ToolsManager.FTool.AddToHistory(ToolsManager.FToolTag, FCanvas);
  isDrawing := True;
  ScrollBar.Invalidate;
  PaintBox.invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if isDrawing = True then begin
    if Zoom.GetZoomValue > 0 then begin
      x := Zoom.GetGlobalX(x);
      y := Zoom.GetGlobalY(y);
    end;
    ToolsManager.FTool.Draw(x, y);
    ToolsManager.FTool.AddCoordsToHistory(ToolsManager.FTool.Coord);
    PaintBox.Invalidate;
    if Zoom.GetZoomValue > 0 then
      with zoom do
        ToolsDataUtils.ShowHistory(z_px, z_py, z_n);
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if isDrawing = True then begin
    ToolsManager.FTool.AfterDraw;
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

procedure TMainForm.UndoClick(Sender: TObject);
begin
  with zoom do
    ToolsDataUtils.Undo(z_px, z_py, z_n);
  paintbox.invalidate;
end;

procedure TMainForm.RedoClick(Sender: TObject);
begin
  with zoom do
    ToolsDataUtils.Redo(z_px, z_py, z_n);
  paintbox.invalidate;
end;

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
    for i := 0 to ToolsDataUtils.GetLength - 1 do begin
      EData := ToolsDataUtils.GetData(i);
      Etool := classref[EData.Noftool].Tool.Create(FCanvas);
      ETool.ExportToSvg(output, EData);
      ETool.Free;
    end;
    writeln(output, '</svg>');
    closefile(output);
  end;
end;

end.
