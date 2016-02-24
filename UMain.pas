unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, ExtDlgs, UTools, UZoom, UPalette, USVPFormat,
  UObjectMove, UToolsPanel, UScrollBar, UFigureHistoryManager, UToolsManager;

type

  { TMainForm }

  TMainForm = class(TForm)
    ColorDialog: TColorDialog;
    Export: TMenuItem;
    ExportToSvg: TMenuItem;
    ExportToSVP: TMenuItem;
    ImportFromSVP: TMenuItem;
    ToolsIconList: TImageList;
    MainMenu: TMainMenu;
    EditMenu: TMainMenu;

    Undo: TMenuItem;
    Redo: TMenuItem;
    SaveAs: TMenuItem;
    OpenAs: TMenuItem;
    CloseProgram: TMenuItem;
    PaintBox: TPaintBox;
    FCanvas: TBitmap;
    procedure ExportToSVPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportFromSVPClick(Sender: TObject);
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
  isDrawing: boolean;
  isMoving: boolean;

  palette: TPalette;
  PanelMove: TObjectMove;
  ToolsPanel: TToolsPanel;
  ScrollBar: TZoomScrollBar;
  FigureManager: TFigureHistoryManager;

implementation

{$R *.lfm}
{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanvas := tbitmap.Create;
  Zoom := TZoom.Create(self);
  palette := Tpalette.Create(self);
  PanelMove := TObjectMove.Create(MainForm.Width - 15 - 94, MainForm.Height - 15);
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
  ToolsManager.FTool.ToGlobalCoords(x, y);
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
    ToolsManager.FTool.ToGlobalCoords(x, y);
    ToolsManager.FTool.Draw(x, y);
    ToolsManager.FTool.AddCoordsToHistory(ToolsManager.FTool.ToolCoords);
    ToolsManager.FTool.ShowHistoryWhenZoomed;
    PaintBox.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if isDrawing = True then begin
    ToolsManager.FTool.AfterDraw;
    isDrawing := False;
    FigureManager.LoadHistory;
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Rect_: trect;
begin
  Rect_ := bounds(0, 0, FCanvas.Width, FCanvas.Height);
  PaintBox.Canvas.CopyRect(rect(0, 0, PaintBox.Width, PaintBox.Height),
    FCanvas.canvas, Rect_);
end;

procedure TMainForm.UndoClick(Sender: TObject);
begin
  with Zoom do
    ToolsDataUtils.Undo(PreviousX, PreviousY);
  paintbox.invalidate;
  FigureManager.LoadHistory;
end;

procedure TMainForm.RedoClick(Sender: TObject);
begin
  with Zoom do
    ToolsDataUtils.Redo(PreviousX, PreviousY);
  paintbox.invalidate;
  FigureManager.LoadHistory;
end;

procedure TMainForm.SaveAsClick(Sender: TObject);
var
  SavePictureDialog: TSavePictureDialog;
begin
  SavePictureDialog := TSavePictureDialog.Create(self);
  SavePictureDialog.Filter := 'Bitmap Picture|*.bmp';
  if SavePictureDialog.Execute then
    FCanvas.SaveToFile(SavePictureDialog.filename);
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  Halt;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
var
  OpenPictureDialog: TOpenPictureDialog;
begin
  OpenPictureDialog := TOpenPictureDialog.Create(Self);
  OpenPictureDialog.Filter := 'Bitmap Picture|*.bmp';
  if OpenPictureDialog.Execute then
    FCanvas.LoadFromFile(OpenPictureDialog.FileName);
  PaintBox.Invalidate;
end;

procedure TMainForm.ExportClick(Sender: TObject);
var
  Filename: string;
  Output: textfile;
  i: integer;
  TmpTool: TTools;
  TmpData: ToolData;
  ExportDialog: TSavePictureDialog;
begin
  ExportDialog := TSavePictureDialog.Create(self);
  ExportDialog.Filter := 'Scalable Vector Graphics|*.svg';
  if ExportDialog.Execute then begin
    Filename := ExportDialog.FileName;
    assignfile(Output, Filename);
    Rewrite(Output);
    WriteLn(Output, '<?xml version="1.0" standalone="no"?>');
    writeln(Output, '<svg width="', FCanvas.Width, 'mm" height="',
      FCanvas.Height, 'mm" viewBox="0 0 ', FCanvas.Width, ' ', FCanvas.Height,
      '" xmlns="http://www.w3.org/2000/svg" version="1.1">');
    for i := 0 to ToolsDataUtils.GetLength - 1 do begin
      TmpData := ToolsDataUtils.GetData(i);
      TmpTool := classref[TmpData.Noftool].Tool.Create(FCanvas);
      TmpTool.ExportToSvg(Output, TmpData);
      TmpTool.Free;
    end;
    writeln(Output, '</svg>');
    closefile(Output);
  end;
end;

procedure TMainForm.ImportFromSVPClick(Sender: TObject);
var
  SVPFormat: TSVPFormat;
  ImportFromSVPDialog: TSavePictureDialog;
begin
  ImportFromSVPDialog := TSavePictureDialog.Create(self);
  ImportFromSVPDialog.Filter := 'SOLG Vector Paint|*.svp';
  if ImportFromSVPDialog.Execute then begin
    SVPFormat := TSVPFormat.Create;
    SVPFormat.ImportFromSVP(ImportFromSVPDialog.FileName);
    SVPFormat.Free;
  end;
  FigureManager.LoadHistory;
end;

procedure TMainForm.ExportToSVPClick(Sender: TObject);
var
  SVPFormat: TSVPFormat;
  ExportToSVPDialog: TSavePictureDialog;
begin
  ExportToSVPDialog := TSavePictureDialog.Create(self);
  ExportToSVPDialog.Filter := 'SOLG Vector Paint|*.svp';
  if ExportToSVPDialog.Execute then begin
    SVPFormat := TSVPFormat.Create;
    SVPFormat.ExportToSVP(ExportToSVPDialog.FileName);
  end;
end;

end.
