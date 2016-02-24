unit UToolsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, UTools, Graphics, Buttons, UToolsManager;

type
  OnClickEvent = procedure(Sender: TObject) of object;

  TToolsPanel = class(TPanel)
  public
    constructor Create(parent_: TComponent; List: TImageList);
  private
    ToolsIcons: TImageList;
    procedure AddButton(parent_: TComponent; tool_index: integer;
      ClickEvent: OnClickEvent);
    function GetTopPosition(i: integer): integer;
    function GetLeftPosition(i: integer): integer;
    procedure ToolClickEvent(Sender: TObject);
  end;

implementation

constructor TToolsPanel.Create(parent_: TComponent; List: TImageList);
var
  i: integer;
begin
  ToolsIcons := List;
  inherited Create(parent_);
  with self do begin
    Parent := twincontrol(parent_);
    Width := 5 + 40 + 5;
    Height := 20 * (high(ClassRef) + 1);
    BevelInner := bvRaised;
    BevelOuter:= bvLowered;
  end;
  for i := 0 to high(ClassRef) do
    AddButton(self, i, @ToolClickEvent);
end;

procedure TToolsPanel.AddButton(parent_: TComponent; tool_index: integer;
  ClickEvent: OnClickEvent);
var
  button: TSpeedButton;
begin
  button := TSpeedButton.Create(parent_);
  with button do begin
    parent := twincontrol(parent_);
    Width := 20;
    Height := 20;
    ToolsIcons.GetBitmap(tool_index, glyph);
    top := GetTopPosition(tool_index);
    left := GetLeftPosition(tool_index);
    tag := tool_index;
    OnClick := ClickEvent;
  end;
end;

function TToolsPanel.GetTopPosition(i: integer): integer;
begin
  Result := 20 + (18 * (i - i mod 2));
  if i > 1 then
    Result -= 18;
  Result -= trunc(i / 4) * 18;
end;

function TToolsPanel.GetLeftPosition(i: integer): integer;
begin
  Result := 5 + (i mod 2) * 18;
end;

procedure TToolsPanel.ToolClickEvent(Sender: TObject);
begin
    ToolsManager.FTool.Free;
    ToolsManager.FToolTag := TButton(Sender).tag;
    ToolsManager.FTool := ClassRef[TButton(Sender).tag].Tool.Create(ToolsManager.FCanvas);
end;

end.
