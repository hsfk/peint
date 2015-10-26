unit Mein;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ExtDlgs, Instruments;

type

  { TMainForm }

  BrushStyleCbox = record
    Name : string;
    Style : TBrushStyle;
  end;

  TMainForm = class(TForm)
    BrushStyleCbox: TComboBox;
    IncButton: TButton;
    DecButton: TButton;
    ColorDialog: TColorDialog;
    LineWidth: TEdit;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
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
    procedure LineWidthChange(Sender: TObject);
    procedure LineWidthEditingDone(Sender: TObject);
    procedure IncButtonClick(Sender: TObject);
    procedure setstyles();
    procedure SaveAsClick(Sender: TObject);
    procedure CloseProgramClick(Sender: TObject);
    procedure OpenAsClick(Sender: TObject);
    procedure ObjectMove(Obj : twincontrol; px,py,x,y : integer);
    procedure ToolsPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolsPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ToolsPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure setbuttons();
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  holst : tbitmap;
  isDrawing,isMoving : boolean;
  px,py,cx,cy : integer;
  tool : main;
  BrushStyles : array of BrushStyleCbox;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
 holst := tbitmap.Create;
 setbuttons();
 setStyles();
 tool := pen.create(holst);
 holst.Width := PaintBox.width;
 holst.Height := PaintBox.Height;
 holst.canvas.brush.color := $FFFFFF;
 holst.canvas.rectangle(0,0, holst.Width,holst.Height);
 holst.canvas.brush.Style:=bsClear;
end;

//---------------------------PAINT-BOX------------------------------------------
procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 tool.BeforeDraw(x,y);
 isDrawing:=TRUE;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if isDrawing = TRUE then begin
  tool.Draw(x,y);
  PaintBox.Invalidate;
 end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 isDrawing:=FALSE;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var r : trect;
begin
 r := bounds(0,0,holst.Width,holst.Height);
 PaintBox.Canvas.CopyRect(rect(0,0,PaintBox.Width,PaintBox.Height),holst.canvas,r);
end;

//----------------------PANEL-EVENTS--------------------------------------------
procedure TMainForm.ObjectMove(Obj : Twincontrol; px,py,x,y : integer);
var newx, newy : integer;
begin
  newx := obj.Left + x - px;
  newy := obj.top + y - py;
  if (newx) >= (MainForm.Width - obj.width) then
   obj.Left := MainForm.width - obj.width
  else if (newx) <= 0 then
   obj.left := 0
  else
   obj.Left := newx;

  if (newy) >= (MainForm.height - obj.height) then
   obj.top := MainForm.height - obj.height
  else if (newy) <= 0 then
   obj.top := 0
  else
   obj.top := newy;
end;

procedure TMainForm.ToolsPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 isMoving := TRUE;
 px:=x;
 py:=y;
end;

procedure TMainForm.ToolsPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var newx,newy : integer;
begin
 if isMoving = TRUE then begin
  ObjectMove(twincontrol(sender),px,py,x,y);
 end;
end;

procedure TMainForm.ToolsPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isMoving := False;
end;

//-----------------------------TOOLS-PANEL-ITEM-EVENTS--------------------------
procedure TMainForm.setbuttons();
var i : integer;
    button : tbutton;
begin
  for i := 0 to high(ClassRef) do begin
    button := tbutton.Create(self);
    button.tag := i;
    button.Parent := ToolsPanel;
    button.Width:=60;
    button.Height:=20;
    button.top := 15 + 25 * i;
    button.Visible:=TRUE;
    button.Caption:=classref[i].nameofinstr;
    button.Left:=10;
    button.OnClick:=@ButtonClick;
  end;
end;

procedure TMainForm.ButtonClick(Sender: TObject);
begin
tool.free;
 tool := classref[tbutton(sender).tag].instrument.create(holst);
end;
//-----------------------------OPTIONS-PANEL-ITEM-EVENTS------------------------
procedure TMainForm.BrushStyleCboxSelect(Sender: TObject);
begin
 holst.canvas.brush.Style:=brushstyles[BrushStyleCbox.ItemIndex].Style;
end;

procedure TMainForm.LineWidthChange(Sender: TObject);
begin
  if (LineWidth.text = '') OR (strtoint(LineWidth.text) < 1) then
    LineWidth.text := '1';
  if strtoint(LineWidth.text) > 99 then
    LineWidth.text := '99';
end;

procedure TMainForm.LineWidthEditingDone(Sender: TObject);
begin
  holst.Canvas.pen.width := strtoint(LineWidth.text);
end;

procedure TMainForm.IncButtonClick(Sender: TObject);
begin
   LineWidth.text := inttostr(strtoint(LineWidth.text) + tbutton(sender).Tag);
   LineWidthEditingDone(LineWidth);
end;

procedure TMainForm.SetStyles();
var i : integer;
begin
  for i := 0 to high(BrushStyles) do begin
    BrushStyleCbox.Items.Add(brushstyles[i].name);
  end;
end;

procedure InitStyle(NM : string;STL : TBrushStyle);
begin
  setlength(BrushStyles, length(BrushStyles) + 1);
  with BrushStyles[high(BrushStyles)] do begin
    name := nm;
    style := stl;
  end;
end;
//-----------------------------DIALOGS----------------------------------------
procedure TMainForm.SaveAsClick(Sender: TObject);
begin
  if SavePictureDialog.execute then
    holst.SaveToFile(SavePictureDialog.filename);
end;

procedure TMainForm.CloseProgramClick(Sender: TObject);
begin
  halt;
end;

procedure TMainForm.OpenAsClick(Sender: TObject);
begin
  if OpenPictureDialog1.execute then
     holst.LoadFromFile(OpenPictureDialog1.FileName);
  PaintBox.Invalidate;
end;

procedure TMainForm.PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.execute then
    tshape(sender).Brush.color := ColorDialog.Color;
  holst.canvas.pen.color := PenColor.brush.color;
  holst.canvas.brush.color := BrushColor.brush.color;
end;

initialization
   InitStyle('Horizontal', bsHorizontal);
   InitStyle('Solid', bsSolid);
end.

