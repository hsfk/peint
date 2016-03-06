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
    FEditButton: TCustomButton;
    FDelButton: TCustomButton;
    procedure ListBoxMouseDownEvent(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditButtonClickEvent(Sender: TObject);
    procedure DelButtonClickEvent(Sender: TObject);
  end;

implementation

constructor TFigureHistoryManager.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft, 155, 320);
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;

  FEditButton := TACustomButton.Create(Self, 280, 2, 40, 20, 'Edit');
  FEditButton.OnClick := @EditButtonClickEvent;
  FDelButton := TACustomButton.Create(Self, 280, 44, 40, 20, 'Del');

  FListBox := TListBox.Create(Self);
  with FListBox do begin
    Width := 152;
    Height := 255;
    Left := 2;
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
  if FListBox.ItemIndex <> -1 then
    History.GetFigure(FListBox.ItemIndex).HighLight;
end;

procedure TFigureHistoryManager.EditButtonClickEvent(Sender: TObject);
begin
  if FListBox.ItemIndex <> -1 then begin
    FEditPanel.Free;
    FEditPanel := TEditPanel.Create(Self.Parent);
    FEditPanel.LoadFigure(History.GetFigure(FListBox.ItemIndex));
  end;
end;

procedure TFigureHistoryManager.DelButtonClickEvent(Sender: TObject);
begin

end;

end.
