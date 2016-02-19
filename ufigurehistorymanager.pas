unit UFigureHistoryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Graphics, StdCtrls, UTools;

type
  TFigureHistoryManager = class(Tpanel)
  public
    constructor Create(parent_: TComponent);
  end;

  TFigureListBox = class(TListBox)
    public
      constructor Create(parent_: TComponent);
  end;

implementation

constructor TFigureHistoryManager.Create(parent_: TComponent);
begin
  inherited Create(parent_);
  with self do begin
    Parent := twincontrol(parent_);
    Width := 60;
    Height := twincontrol(parent_).height - 35 - 1;
    brush.Color := clDkGray;
    BevelInner := bvNone;
    Left := twincontrol(parent_).width - 60 - 15 -1;
    top := 0;
  end;
  TFigureListBox.Create(parent_);
end;

constructor TFigureListBox.Create(parent_: TComponent);
begin
  inherited Create(parent_);
  with self do begin
    Parent := twincontrol(parent_);
    Width := 55;
    Height := twincontrol(parent_).height - 35 - 1 - 5;
    brush.Color := clGray;
    Left := twincontrol(parent_).width - 60 - 15 -1 + 5;
    top := 0 + 5;
  end;
end;

end.

