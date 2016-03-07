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
    FEditPanel: TFigureEdit;
    FListBox: TListBox;
    procedure SelectionChangeEvent(Sender: TObject; User: boolean);
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
  FEditPanel := TFigureEdit.Create(Self, 280, 5);
  FListBox := TListBox.Create(Self);
  FListBox.OnSelectionChange := @SelectionChangeEvent;
  FListBox.MultiSelect := True;
  with FListBox do begin
    Width := 152;
    Height := 255;
    Left := 1;
    Top := 20;
    Parent := TWinControl(Self);
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

procedure TFigureHistoryManager.SelectionChangeEvent(Sender: TObject; User: boolean);
var
  i: integer;
begin
  History.Deselect;
  for i := 0 to FListBox.Items.Count - 1 do
    if FListBox.Selected[i] = True then begin
      History.Select(i);
      FEditPanel.LoadFigure(History.GetFigure(i));
    end;
  History.Show;
end;

end.
