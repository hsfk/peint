unit UToolsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, UTools, Graphics, UToolsManager, Buttons;

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
    procedure ZoomClickEvent(Sender: TObject);
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
    brush.Color := clDkGray;
    BevelInner := bvNone;
  end;
  for i := 0 to high(ClassRef) do
    AddButton(self, i, @ToolClickEvent);
  AddButton(self, i + 1, @ZoomClickEvent);
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
  if i > 1 then    //чтоб 1-е 2 кнопки не съезжали
    Result -= 18;
  Result -= trunc(i / 4) * 18;  //каждая 4 кнопка улетает еще на 18 вниз
end;

function TToolsPanel.GetLeftPosition(i: integer): integer;
begin
  Result := 5 + (i mod 2) * 18;
end;

procedure TToolsPanel.ToolClickEvent(Sender: TObject);
begin
  with ToolsManager do begin
    isZoomSelected := False;
    tool.Free;
    ToolTag := TButton(Sender).tag;
    tool := ClassRef[TButton(Sender).tag].Tool.Create(FCanvas);
  end;
end;

procedure TToolsPanel.ZoomClickEvent(Sender: TObject);
begin
  ToolsManager.isZoomSelected := True;
end;

end.
