unit UFigureHistoryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Graphics, StdCtrls, UTools, Menus, UZoom;

type
  TFigureHistoryManager = class(Tpanel)
  public
    constructor Create(Parent_: TComponent);
    procedure LoadHistory;
  private
    FListBox: TListBox;
    procedure OnMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

  TFigureListBox = class(TListBox)
  public
    constructor Create(Parent_: TComponent);
  end;

implementation

constructor TFigureHistoryManager.Create(Parent_: TComponent);
begin
  inherited Create(Parent_);
  with Self do begin
    Parent := twincontrol(Parent_);
    Width := 95;
    Height := twincontrol(Parent_).Height - 35 - 1;
    BevelOuter := bvLowered;
    Left := twincontrol(Parent_).Width - 60 - 15 - 35 - 1;
    Top := 0;
  end;

  FListBox := TFigureListBox.Create(Parent_);
  FListBox.OnMouseDown := @OnMouseDownEvent;
end;

procedure TFigureHistoryManager.LoadHistory;
var
  i: integer;
  Text_: string;
begin
  FListBox.Items.Clear;
  for i := 0 to ToolsDataUtils.GetPosition do begin
    Text_ := IntToStr(i) + '. ' +
      ClassRef[ToolsDataUtils.GetData(i).NofTool].NameOfTool;
    FlistBox.Items.Add(Text_);
  end;
end;

procedure TFigureHistoryManager.OnMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then begin
    ToolsDataUtils.ShowHistory(Zoom.PreviousX, Zoom.PreviousY);
    ToolsDataUtils.HighLightFigure(FListBox.ItemIndex);
  end;
  if Button = mbRight then begin
    ToolsDataUtils.Delete(FListBox.ItemIndex);
    LoadHistory;
  end;
end;

constructor TFigureListBox.Create(Parent_: TComponent);
begin
  inherited Create(Parent_);
  with Self do begin
    Parent := twincontrol(Parent_);
    Width := 85;
    Height := twincontrol(Parent_).Height - 35 - 1 - 5;
    Left := twincontrol(Parent_).Width - 60 - 15 - 35 + 5;
    Top := 0 + 5;
  end;
end;

end.
