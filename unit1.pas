unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    button1: TButton;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1EditingDone(Sender: TObject);
    procedure ObjectMove(Obj : twincontrol; px,py,x,y : integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure setbuttons();
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  holst : tbitmap;
  isDrawing,isMoving : boolean;
  px,py,cx,cy : integer;
  tool : main;

implementation

{$R *.lfm}

{ TForm1 }

procedure Tform1.setbuttons();
var i : integer;
    button : tbutton;
begin
  for i := 0 to high(ClassRef) do begin
    button := tbutton.Create(self);
    button.tag := i;
    button.Parent := panel1;
    button.Width:=60;
    button.Height:=20;
    button.top := 15 + 25 * i;
    button.Visible:=TRUE;
    button.Caption:=classref[i].nameofinstr;
    button.Left:=10;
    button.OnClick:=@button1click;
  //  button.Show;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
tool.free;
 tool := classref[tbutton(sender).tag].instrument.create(holst);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if (edit1.text = '') OR (strtoint(edit1.text) < 1) then
    edit1.text := '1';
  if strtoint(edit1.text) > 99 then
    edit1.text := '99';
end;

procedure TForm1.Edit1EditingDone(Sender: TObject);
begin
  holst.Canvas.pen.width := strtoint(edit1.text);
end;


procedure Tform1.ObjectMove(Obj : Twincontrol; px,py,x,y : integer);
var newx, newy : integer;
begin
      newx := obj.Left + x - px;
      newy := obj.top + y - py;
     if (newx) >= (form1.Width - obj.width) then
       obj.Left := form1.width - obj.width
     else if (newx) <= 0 then
       obj.left := 0
     else
       obj.Left := newx;

     if (newy) >= (form1.height - obj.height) then
       obj.top := form1.height - obj.height
     else if (newy) <= 0 then
       obj.top := 0
     else
       obj.top := newy;
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isMoving := TRUE;
  px:=x;
  py:=y;

end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var newx,newy : integer;
begin

   if isMoving = TRUE then begin
    ObjectMove(twincontrol(sender),px,py,x,y);
   end;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isMoving := False;
end;





procedure TForm1.PaintBox1Paint(Sender: TObject);
var r : trect;
begin
   r := bounds(0,0,holst.Width,holst.Height);
   paintbox1.Canvas.CopyRect(rect(0,0,paintbox1.Width,paintbox1.Height),holst.canvas,r);
end;

procedure TForm1.Shape1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog1.execute then
    tshape(sender).Brush.color := ColorDialog1.Color;

  holst.canvas.pen.color := shape1.brush.color;
  holst.canvas.brush.color := shape2.brush.color;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   tool.BeforeDraw(x,y);
   isDrawing:=TRUE;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    holst := tbitmap.Create;
    setbuttons();
    tool := pen.create(holst);
    holst.Width := paintbox1.width;
    holst.Height := paintbox1.Height;
    holst.canvas.brush.color := $FFFFFF;
    holst.canvas.rectangle(0,0, holst.Width,holst.Height);
    holst.canvas.brush.Style:=bsClear;

//    holst.canvas.Clear;
end;


procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if isDrawing = TRUE then begin
    tool.Draw(x,y);
    paintbox1.Invalidate;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   isDrawing:=FALSE;
end;

end.

