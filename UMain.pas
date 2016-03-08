unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, ExtDlgs, UPalette,
  UObjectMove, UToolsPanel, UFigureHistoryManager,
  UHistory, UZoom, UPointUtils, USceneUtils, UScrollBar, USVPFormat;

type

  { TMainForm }

  TMainForm = class(TForm)
    Export: TMenuItem;
    ExportToSvg: TMenuItem;
    ToolsIconList: TImageList;
    MainMenu: TMainMenu;
    EditMenu: TMainMenu;
    Undo: TMenuItem;
    Redo: TMenuItem;
    SaveAs: TMenuItem;
    OpenAs: TMenuItem;
    CloseProgram: TMenuItem;
    FMainScene: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    FIsDrawing: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainForm.Constraints.MinHeight := MainForm.Height;
  MainForm.Constraints.MinWidth := MainForm.Width;
  MainForm.Brush.Color := clDkGray;

  ScrollBar := TZoomScrollBar.Create(Self, FMainScene);
  PanelMove := TObjectMove.Create(MainForm.Width - 15, MainForm.Height - 35);
  History := THistory.Create(FMainScene.Canvas);
  Zoom := TZoom.Create(Self, FMainScene);
  SceneUtils := TSceneUtils.Create(FMainScene.Canvas);
  Palette := TPalette.Create(FMainScene.Canvas, Self, 0, Self.Width - 155 - 15);
  FigureManager := TFigureHistoryManager.Create(Self, Palette.Height, Palette.Left);
  ToolsPanel := TToolsPanel.Create(FMainScene.Canvas, Self, ToolsIconList);
end;

procedure TMainForm.FMainSceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FIsDrawing := True;
  ToolsPanel.RecreateCurrentTool;
  ToolsPanel.Tool.Start(ToPoint(X, Y), Button);
  FigureManager.LoadHistory;
  FMainScene.Invalidate;
end;

procedure TMainForm.FMainSceneMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FIsDrawing = True then begin
    ToolsPanel.Tool.Continue(ToPoint(X, Y), Shift);
    FMainScene.Invalidate;
  end;
end;

procedure TMainForm.FMainSceneMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FIsDrawing = True then begin
    ToolsPanel.Tool.Stop;
    FIsDrawing := False;
  end;
end;

procedure TMainForm.FMainScenePaint(Sender: TObject);
begin
  History.Show;
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

procedure TMainForm.FormResize(Sender: TObject);
begin
  PanelMove.UpdateBorders(MainForm.Width - 15, MainForm.Height - 35);
  if Zoom.Value > 0 then
    Zoom.MainSceneFullScreen;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
var
  OpenPictureDialog: TOpenPictureDialog;
  SVPImport: TSVPFormat;
begin
  OpenPictureDialog := TOpenPictureDialog.Create(Self);
  OpenPictureDialog.Filter := 'Solg Vector Picture|*.svp';
  if OpenPictureDialog.Execute then begin
    SVPImport := TSVPFormat.Create(OpenPictureDialog.FileName, FMainScene.Canvas);
    SVPImport.ImportFromSVP;
    SVPImport.Free;
  end;
end;

procedure TMainForm.SaveAsClick(Sender: TObject);
var
  SavePictureDialog: TSavePictureDialog;
  SVPExport: TSVPFormat;
begin
  SavePictureDialog := TSavePictureDialog.Create(Self);
  SavePictureDialog.Filter := 'Solg Vector Picture|*.svp';
  if SavePictureDialog.Execute then begin
    SVPExport := TSVPFormat.Create(SavePictureDialog.FileName, FMainScene.Canvas);
    SVPExport.ExportToSVP;
    SVPExport.Free;
  end;
end;

procedure TMainForm.ExportClick(Sender: TObject);
var
  Output: TFPImageBitmap;
  SavePictureDialog: TSavePictureDialog;
begin
  SavePictureDialog := TSavePictureDialog.Create(self);
  SavePictureDialog.Filter := 'Bitmap Picture|*.bmp';
  if SavePictureDialog.Execute then begin
    Output := TBitmap.Create;
    Output.Width := FMainScene.Width;
    Output.Height := FMainScene.Height;
    SceneUtils.ClearScene(Output.Canvas);
    History.ShowFigures(Output.Canvas);
    Output.SaveToFile(SavePictureDialog.FileName);
  end;
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  Halt;
end;

end.
