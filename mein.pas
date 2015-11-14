unit Mein;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ExtDlgs, Instruments;

type

  { TMainForm }

  BrushStyleCbox = record
    Name: string;
    Style: TBrushStyle;
  end;

  TMainForm = class(TForm)
    BrushStyleCbox: TComboBox;
    CTRLZButton: TButton;
    IncButton: TButton;
    DecButton: TButton;
    ColorDialog: TColorDialog;
    LineWidth: TEdit;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    SaveAs: TMenuItem;
    CloseProgram: TMenuItem;
    OpenAs: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    PaintBox: TPaintBox;
    ToolsPanel: TPanel;
    OptionsPanel: TPanel;
    SavePictureDialog: TSavePictureDialog;
    PenColor: TShape;
    BrushColor: TShape;
    procedure ButtonClick(Sender: TObject);
    procedure BrushStyleCboxSelect(Sender: TObject);
    procedure CTRLZButtonClick(Sender: TObject);
    procedure LineWidthChange(Sender: TObject);
    procedure LineWidthEditingDone(Sender: TObject);
    procedure IncButtonClick(Sender: TObject);
    procedure setstyles();
    procedure SaveAsClick(Sender: TObject);
    procedure CloseProgramClick(Sender: TObject);
    procedure OpenAsClick(Sender: TObject);
    procedure ObjectMove(Obj: twincontrol; px, py, x, y: integer);
    procedure ToolsPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ToolsPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ToolsPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure setbuttons();
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  holst: tbitmap;
  isDrawing, isMoving: boolean;
  px, py, cx, cy, history: integer;
  tool: main;
  BrushStyles: array of BrushStyleCbox;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  holst := tbitmap.Create;
  setbuttons();
  setStyles();
  tool := pen.Create(holst);
  holst.Width := PaintBox.Width;
  holst.Height := PaintBox.Height;
  holst.canvas.brush.color := $FFFFFF;
  holst.canvas.rectangle(0, 0, holst.Width, holst.Height);
  holst.canvas.brush.Style := bsClear;
end;
//---------------------------PAINT-BOX------------------------------------------
procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  tool.BeforeDraw(x, y);
  tool.ZInit(tool, holst.canvas.pen.color, holst.canvas.Brush.Color,
    holst.canvas.pen.Width, X, Y);
  isDrawing := True;
  paintbox.invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if isDrawing = True then
  begin
    tool.Draw(x, y);
    tool.CoordInc(x, y);
    PaintBox.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isDrawing := False;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  r: trect;
begin
  r := bounds(0, 0, holst.Width, holst.Height);
  PaintBox.Canvas.CopyRect(rect(0, 0, PaintBox.Width, PaintBox.Height), holst.canvas, r);
end;
//----------------------PANEL-EVENTS--------------------------------------------
procedure TMainForm.ObjectMove(Obj: Twincontrol; px, py, x, y: integer);
var
  newx, newy: integer;
begin
  newx := obj.Left + x - px;
  newy := obj.top + y - py;
  if (newx) >= (MainForm.Width - obj.Width) then
    obj.Left := MainForm.Width - obj.Width
  else if (newx) <= 0 then
    obj.left := 0
  else
    obj.Left := newx;

  if (newy) >= (MainForm.Height - obj.Height) then
    obj.top := MainForm.Height - obj.Height
  else if (newy) <= 0 then
    obj.top := 0
  else
    obj.top := newy;
end;

procedure TMainForm.ToolsPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isMoving := True;
  px := x;
  py := y;
end;

procedure TMainForm.ToolsPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  newx, newy: integer;
begin
  if isMoving = True then
  begin
    ObjectMove(twincontrol(Sender), px, py, x, y);
  end;
end;

procedure TMainForm.ToolsPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isMoving := False;
end;
//-----------------------------TOOLS-PANEL-ITEM-EVENTS--------------------------
procedure TMainForm.setbuttons();
var
  i: integer;
  button: TButton;
begin
  for i := 0 to high(ClassRef) do
  begin
    button := TButton.Create(self);
    button.tag := i;
    button.Parent := ToolsPanel;
    button.Width := 60;
    button.Height := 20;
    button.top := 15 + 25 * i;
    button.Visible := True;
    button.Caption := ClassRef[i].nameofinstr;
    button.Left := 10;
    button.OnClick := @ButtonClick;
  end;
end;

procedure TMainForm.ButtonClick(Sender: TObject);
begin
  tool.Free;
  tool := ClassRef[TButton(Sender).tag].instrument.Create(holst);
end;
//-----------------------------CTRL-Z-------------------------------------------
procedure TMainForm.CTRLZButtonClick(Sender: TObject);
var
  i: integer;
begin
  holst.canvas.brush.color := $FFFFFF;
  holst.canvas.brush.Style := bsSolid;
  holst.canvas.rectangle(0, 0, holst.Width, holst.Height);
  if high(zarr) - history > 0 then
    history += 1;
  ShowMessage(IntToStr(history));
  for i := 0 to high(ZArr) - history do
    tool.Zshow(i);
  paintbox.invalidate;
end;
//-----------------------------CTRL-Y-------------------------------------------
//-----------------------------OPTIONS-PANEL-ITEM-EVENTS------------------------
procedure TMainForm.BrushStyleCboxSelect(Sender: TObject);
begin
  holst.canvas.brush.Style := brushstyles[BrushStyleCbox.ItemIndex].Style;
end;

procedure TMainForm.LineWidthChange(Sender: TObject);
begin
  if (LineWidth.Text = '') or (StrToInt(LineWidth.Text) < 1) then
    LineWidth.Text := '1';
  if StrToInt(LineWidth.Text) > 99 then
    LineWidth.Text := '99';
end;

procedure TMainForm.LineWidthEditingDone(Sender: TObject);
begin
  holst.Canvas.pen.Width := StrToInt(LineWidth.Text);
end;

procedure TMainForm.IncButtonClick(Sender: TObject);
begin
  LineWidth.Text := IntToStr(StrToInt(LineWidth.Text) + TButton(Sender).Tag);
  LineWidthEditingDone(LineWidth);
end;

procedure TMainForm.SetStyles();
var
  i: integer;
begin
  for i := 0 to high(BrushStyles) do
  begin
    BrushStyleCbox.Items.Add(brushstyles[i].Name);
  end;
end;

procedure InitStyle(NM: string; STL: TBrushStyle);
begin
  setlength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do
  begin
    Name := nm;
    style := stl;
  end;
end;
//-----------------------------DIALOGS----------------------------------------
procedure TMainForm.SaveAsClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    holst.SaveToFile(SavePictureDialog.filename);
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  halt;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    holst.LoadFromFile(OpenPictureDialog1.FileName);
  PaintBox.Invalidate;
end;

procedure TMainForm.PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if ColorDialog.Execute then
    tshape(Sender).Brush.color := ColorDialog.Color;
  holst.canvas.pen.color := PenColor.brush.color;
  holst.canvas.brush.color := BrushColor.brush.color;
end;

initialization
  InitStyle('Horizontal', bsHorizontal);
  InitStyle('Solid', bsSolid);
  InitStyle('Clear', bsClear);
  InitStyle('Vertical', bsVertical);
  InitStyle('FDiagonal', bsFdiagonal);
  InitStyle('BDiagonal', bsBDiagonal);
  InitStyle('Cross', bsCross);
  InitStyle('DiagCross', bsDiagCross);
end.
