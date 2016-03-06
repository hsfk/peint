unit UToolsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, UTools, Graphics, Buttons;

type
  OnClickEvent = procedure(Sender: TObject) of object;

  TToolsPanel = class(TPanel)
  public
    constructor Create(Parent_: TComponent; List: TImageList; Scene: TCanvas);
  private
    FScene: TCanvas;
    ToolsIcons: TImageList;
    procedure AddButton(Parent_: TComponent; tool_index: integer;
      ClickEvent: OnClickEvent);
    procedure ToolClickEvent(Sender: TObject);
  end;

implementation

constructor TToolsPanel.Create(Parent_: TComponent; List: TImageList; Scene: TCanvas);
var
  i: integer;
begin
  FScene := Scene;
  ToolsIcons := List;
  inherited Create(Parent_);
  with Self do begin
    Parent := TWinControl(Parent_);
    Width := 5 + 40 + 5;
    Height := 20 + 40 * Length(Tools) + 5;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
  end;
  for i := 0 to High(Tools) do
    AddButton(self, i, @ToolClickEvent);
end;

procedure TToolsPanel.AddButton(Parent_: TComponent; tool_index: integer;
  ClickEvent: OnClickEvent);
var
  Button: TSpeedButton;
begin
  Button := TSpeedButton.Create(Parent_);
  with Button do begin
    Parent := TWinControl(Parent_);
    Width := 40;
    Height := 40;
    ToolsIcons.GetBitmap(tool_index, glyph);
    Top := 20 + tool_index * 40;
    Left := 5;
    Tag := tool_index;
    OnClick := ClickEvent;
  end;
end;

procedure TToolsPanel.ToolClickEvent(Sender: TObject);
begin
  CurrentToolIndex := TButton(Sender).tag;
  Tool := Tools[CurrentToolIndex].Tool.Create(FScene);
end;

end.
