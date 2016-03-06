unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, ExtDlgs, UTools, UPalette,
  UObjectMove, UToolsPanel, UScrollBar, UFigureHistoryManager,
  UHistory, UZoom, UPointUtils, UMainSceneUtils;

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
    FMainScene: TPaintBox;
    procedure ExportToSVPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportFromSVPClick(Sender: TObject);
    procedure FMainSceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FMainSceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FMainSceneMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FMainScenePaint(Sender: TObject);
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
  IsDrawing: boolean;
  isMoving: boolean;

  Palette: TPalette;
  ToolsPanel: TToolsPanel;
  ScrollBar: TZoomScrollBar;
  FigureManager: TFigureHistoryManager;

implementation

{$R *.lfm}
{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainForm.Constraints.MaxHeight := MainForm.Height;
  MainForm.Constraints.MinHeight := MainForm.Height;
  MainForm.Constraints.MaxWidth := MainForm.Width;
  MainForm.Constraints.MinWidth := MainForm.Width;

  PanelMove := TObjectMove.Create(MainForm.Width - 15, MainForm.Height - 15);
  ScrollBar := TZoomScrollBar.Create(self);
  History := THistory.Create(FMainScene.Canvas);
  Zoom := TZoom.Create(self);
  Tool := TPenTool.Create(FMainScene.Canvas);
  MainSceneUtils := TMainSceneUtils.Create(FMainScene.Canvas);
  Palette := TPalette.Create(FMainScene.Canvas, Self, 0, Self.Width - 155 - 15);
  FigureManager := TFigureHistoryManager.Create(Self, Palette.Height, Palette.Left);
  ToolsPanel := TToolsPanel.Create(FMainScene.Canvas, Self, ToolsIconList);
end;

procedure TMainForm.FMainSceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Tools[CurrentToolIndex].Recreate = True then
    Tool := Tools[CurrentToolIndex].Tool.Create(FMainScene.Canvas);
  Tool.Start(ToPoint(X, Y), Button);
  IsDrawing := True;
  ScrollBar.Invalidate;
  FigureManager.LoadHistory;
  FMainScene.Invalidate;
end;

procedure TMainForm.FMainSceneMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if IsDrawing = True then begin
    Tool.Continue(ToPoint(X, Y), Shift);
    FMainScene.Invalidate;
    ScrollBar.Invalidate;
  end;
end;

procedure TMainForm.FMainSceneMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing = True then begin
    Tool.Stop;
    IsDrawing := False;
  end;
end;

procedure TMainForm.FMainScenePaint(Sender: TObject);
begin
  Palette.LoadToolState;
  MainSceneUtils.SaveToolState;
  History.Show;
  MainSceneUtils.LoadToolState;
end;

procedure TMainForm.UndoClick(Sender: TObject);
begin
  History.Undo;
  FigureManager.LoadHistory;
  FMainScene.Invalidate;
end;

procedure TMainForm.RedoClick(Sender: TObject);
begin
  History.Redo;
  FigureManager.LoadHistory;
  FMainScene.Invalidate;
end;

procedure TMainForm.SaveAsClick(Sender: TObject);
var
  SavePictureDialog: TSavePictureDialog;
begin
  //SavePictureDialog := TSavePictureDialog.Create(self);
  //SavePictureDialog.Filter := 'Bitmap Picture|*.bmp';
  //if SavePictureDialog.Execute then
  //  FCanvas.SaveToFile(SavePictureDialog.filename);
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  Halt;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
var
  OpenPictureDialog: TOpenPictureDialog;
begin
  //OpenPictureDialog := TOpenPictureDialog.Create(Self);
  //OpenPictureDialog.Filter := 'Bitmap Picture|*.bmp';
  //if OpenPictureDialog.Execute then
  //  FCanvas.LoadFromFile(OpenPictureDialog.FileName);
  //FMainScene.Invalidate;
end;

procedure TMainForm.ExportClick(Sender: TObject);
var
  Filename: string;
  Output: textfile;
  i: integer;
  // TmpTool: TTools;
  // TmpData: ToolData;
  ExportDialog: TSavePictureDialog;
begin
  //ExportDialog := TSavePictureDialog.Create(self);
  //ExportDialog.Filter := 'Scalable Vector Graphics|*.svg';
  //if ExportDialog.Execute then begin
  //  Filename := ExportDialog.FileName;
  //  assignfile(Output, Filename);
  //  Rewrite(Output);
  //  WriteLn(Output, '<?xml version="1.0" standalone="no"?>');
  //  writeln(Output, '<svg width="', FCanvas.Width, 'mm" height="',
  //    FCanvas.Height, 'mm" viewBox="0 0 ', FCanvas.Width, ' ', FCanvas.Height,
  //    '" xmlns="http://www.w3.org/2000/svg" version="1.1">');
  //  for i := 0 to ToolsDataUtils.GetLength - 1 do begin
  //    TmpData := ToolsDataUtils.GetData(i);
  //    TmpTool := classref[TmpData.Noftool].Tool.Create(FCanvas);
  //    TmpTool.ExportToSvg(Output, TmpData);
  //    TmpTool.Free;
  //  end;
  //  writeln(Output, '</svg>');
  //  closefile(Output);
  //end;
end;

procedure TMainForm.ImportFromSVPClick(Sender: TObject);
//var
//  SVPFormat: TSVPFormat;
//  ImportFromSVPDialog: TOpenPictureDialog;
begin
  //ImportFromSVPDialog := TOpenPictureDialog.Create(self);
  //ImportFromSVPDialog.Filter := 'SOLG Vector Paint|*.svp';
  //if ImportFromSVPDialog.Execute then begin
  //  SVPFormat := TSVPFormat.Create;
  //  SVPFormat.ImportFromSVP(ImportFromSVPDialog.FileName);
  //  SVPFormat.Free;
  //end;
  //FigureManager.LoadHistory;
end;

procedure TMainForm.ExportToSVPClick(Sender: TObject);
//var
//  SVPFormat: TSVPFormat;
//  ExportToSVPDialog: TSavePictureDialog;
begin
  //ExportToSVPDialog := TSavePictureDialog.Create(self);
  //ExportToSVPDialog.Filter := 'SOLG Vector Paint|*.svp';
  //if ExportToSVPDialog.Execute then begin
  //  SVPFormat := TSVPFormat.Create;
  //  SVPFormat.ExportToSVP(ExportToSVPDialog.FileName);
  //end;
end;

end.
