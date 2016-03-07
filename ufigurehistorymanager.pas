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
    FIsExtended: boolean;
    FExtButton: TACustomButton;
    FDelButton: TACustomButton;
    FUpButton: TACustomButton;
    FDownButton: TACustomButton;
    FEditPanel: TFigureEdit;
    FListBox: TListBox;
    procedure SelectionChangeEvent(Sender: TObject; User: boolean);
    procedure ExtButtonClickEvent(Sender: TObject);
    procedure FUpButtonClickEvent(Sender: TObject);
    procedure FDownButtonClickEvent(Sender: TObject);
    procedure DelButtonClickEvent(Sender: TObject);
  end;

implementation

constructor TFigureHistoryManager.Create(AParent: TComponent; ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft, 155, 390);
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;

  FIsExtended := True;
  FExtButton := TACustomButton.Create(Self, 280, 120, 30, 15, '▲');
  FextButton.OnClick := @ExtButtonClickEvent;
  FUpButton := TACustomButton.Create(Self, 360, 50, 40, 23, 'Up');
  FUpButton.OnClick := @FUpButtonClickEvent;
  FDownButton := TACustomButton.Create(Self, 360, 95, 40, 23, 'Down');
  FDownButton.OnClick := @FDownButtonClickEvent;
  FDelButton := TACustomButton.Create(Self, 360, 5, 40, 23, 'Delete');
  FDelButton.OnClick := @DelButtonClickEvent;

  FEditPanel := TFigureEdit.Create(Self, 300, 5);
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
  AText: string;
begin
  FListBox.Items.Clear;
  for i := 0 to History.DataLength - 1 do begin
    AText := IntToStr(i) + '. ' + History.GetFigure(i).Name;
    FlistBox.Items.Add(AText);
  end;
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

procedure TFigureHistoryManager.ExtButtonClickEvent(Sender: TObject);
begin
  if FIsExtended = True then begin
    Self.Height := 300;
    TButton(Sender).Caption := '▼';
    FIsExtended := False;
  end
  else if FIsExtended = False then begin
    Self.Height := 390;
    TButton(Sender).Caption := '▲';
    FIsExtended := True;
  end;
end;

procedure TFigureHistoryManager.FUpButtonClickEvent(Sender: TObject);
begin
  if (FListBox.ItemIndex <> -1) and (FListBox.ItemIndex > 0) then begin
    History.MoveUp(FListBox.ItemIndex);
    FListBox.Items.Move(FListBox.ItemIndex, FListBox.ItemIndex - 1);
    FListBox.ItemIndex := FListBox.ItemIndex - 1;
    History.Show;
  end;
end;

procedure TFigureHistoryManager.FDownButtonClickEvent(Sender: TObject);
begin
  if (FListBox.ItemIndex <> -1) and (FListBox.ItemIndex < FListBox.Items.Count - 1) then
  begin
    History.MoveDown(FListBox.ItemIndex);
    FListBox.Items.Move(FListBox.ItemIndex, FListBox.ItemIndex + 1);
    FListBox.ItemIndex := FListBox.ItemIndex + 1;
    History.Show;
  end;
end;

procedure TFigureHistoryManager.DelButtonClickEvent(Sender: TObject);
begin
  History.DeleteSelected;
  LoadHistory;
  History.Show;
end;

end.
