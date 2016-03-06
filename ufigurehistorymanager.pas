unit UFigureHistoryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Graphics, StdCtrls, UTools,
  Menus, UHistory, UCustomControls, UObjectMove, UEditPanel;

type

  TFigureHistoryManager = class(TACustomPanel)
  public
    constructor Create(AParent: TComponent; ATop, ALeft: integer);
    procedure LoadHistory;
  private
    FEditPanel: TEditPanel;
    FListBox: TListBox;
    procedure ListBoxMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

implementation

constructor TFigureHistoryManager.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft, 155, 340);
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;
  FEditPanel := TEditPanel.Create(Self);
  FListBox := TListBox.Create(Self);
  with FListBox do begin
    Width := 152;
    Height := 255;
    Left := 1;
    Top := 20;
    Parent := TWinControl(Self);
    OnMouseDown := @ListBoxMouseDownEvent;
  end;
end;

procedure TFigureHistoryManager.LoadHistory;
var
  i: integer;
begin
  FListBox.Items.Clear;
  for i := 0 to History.DataLength - 1 do
    FlistBox.Items.Add(History.GetFigure(i).Name);
end;

procedure TFigureHistoryManager.ListBoxMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if FListBox.ItemIndex <> -1 then begin
    FEditPanel.LoadFigure(History.GetFigure(FListBox.ItemIndex));
    History.GetFigure(FListBox.ItemIndex).HighLight;
  end;
end;

end.
