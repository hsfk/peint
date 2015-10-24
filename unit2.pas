unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,dialogs;

type
  main = class
    PreX, PreY, CurX, CurY : integer;
    holst : tbitmap;
    bufferHolst : tbitmap;
    public
      procedure BeforeDraw(x,y : integer); virtual;
      procedure Draw(x,y : integer); virtual; abstract;
  //    procedure AfterDraw(); virtual;
      constructor create(holstToDraw : TBitmap);
  end;

   BufferRequired = class(main)
     public
       procedure Draw(x,y : integer);override; abstract;
   end;


   BufferNotRequired = class(main)
     public

       procedure Draw(x,y : integer); override; abstract;
   end;
   //----------------------------------------//

    Rectangle = class(BufferRequired)
      public
        procedure Draw(x,y : integer);override;
    end;

    Ellipse = class(BufferRequired)
      public
        procedure Draw(x,y : integer);override;
    end;



    Pen = class(BufferNotRequired)
      public

      //  procedure BeforeDraw(); override;
        procedure Draw(x,y : integer); override;
      //  procedure AfterDraw(); override;
    end;

    ReferenceClass = class of main;

    InstrName = record
      instrument : ReferenceClass;
      nameofinstr : string;
    end;

    var ClassRef : array of InstrName;     //MAINVAR

implementation

  constructor main.create(holstToDraw : TBitmap);
  begin
    holst := holstToDraw;
  end;

  procedure main.BeforeDraw(x,y : integer);
  var r : trect;
  begin
    PreX := x;
    PreY := y;
    bufferHolst := tbitmap.create;
    bufferholst.Width:= holst.Width;
    bufferholst.Height:= holst.Height;
    r := bounds(0,0,holst.Width,holst.Height);
    bufferHolst.Canvas.CopyRect(rect(0,0,bufferHolst.Width,bufferHolst.Height),holst.canvas,r); {some procedure}
  end;

    procedure Rectangle.Draw(x,y : integer);
    var r : trect;
    begin
      r := bounds(0,0,bufferholst.Width,bufferholst.Height);
      holsT.Canvas.CopyRect(rect(0,0,holsT.Width,holsT.Height),bufferholst.canvas,r);
      holst.Canvas.Rectangle(PreX,PreY,X,Y);
    end;

    procedure Ellipse.Draw(x,y : integer);
    var r : trect;
    begin
      r := bounds(0,0,bufferholst.Width,bufferholst.Height);
      holsT.Canvas.CopyRect(rect(0,0,holsT.Width,holsT.Height),bufferholst.canvas,r);
      holst.Canvas.Ellipse(PreX,PreY,x,y);
    end;

  procedure pen.Draw(x,y : integer);
  begin
    holst.canvas.line(PreX,PreY,x,y);
    PreX := x;
    PreY := y;
  end;

  procedure initialize(instrument : ReferenceClass; nameofinstr : string);
  var i : integer;
  begin
    setlength(ClassRef, length(ClassRef) + 1);
    classref[high(ClassRef)].instrument := instrument;
    classref[high(ClassRef)].nameofinstr := nameofinstr;
  end;
   initialization
     initialize(pen, 'Pen');
     initialize(rectangle, 'Rectangle');
     initialize(ellipse, 'Ellipse');
end.

