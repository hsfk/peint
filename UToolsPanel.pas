unit UToolsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, UTools, Graphics, Buttons, UCustomControls, UObjectMove, UPalette;

type
  ToolClass = class of TTool;

  ATool = record
    Recreate: boolean;
    Tool: ToolClass;
    Name: string;
  end;

  TToolsPanel = class(TACustomPanel)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent; ImageList: TImageList);
    procedure ForceRecreateCurrentTool;
    procedure RecreateCurrentTool;
  private
    FCurrentTool: TTool;
    FTools: array of ATool;
    FCurrentToolIndex: integer;
    FScene: TCanvas;
    FToolsIcons: TImageList;
    procedure ToolClickEvent(Sender: TObject);
    function GetCurrentTool: TTool;
    procedure InitTool(Tool: ToolClass; IsRecreatingRequired: boolean; AName: string);
  published
    property Tool: TTool read GetCurrentTool;
  end;

var
  ToolsPanel: TToolsPanel;

implementation

constructor TToolsPanel.Create(Scene: TCanvas; AParent: TComponent;
  ImageList: TImageList);
var
  i: integer;
  ToolButton: TACustomSpeedButton;
begin
  FScene := Scene;

  InitTool(TPenTool, True, 'Pen');
  InitTool(TPolyLineTool, False, 'Poly line');
  InitTool(TLineTool, True, 'Line');
  InitTool(TRectangleTool, True, 'Rectangle');
  InitTool(TEllipseTool, True, 'Ellipse');
  InitTool(TZoomTool, True, 'Zoom');
  InitTool(TSelectTool, True, 'Selection tool');
  InitTool(THandTool, True, 'Hand');

  FCurrentToolIndex := 0;
  FToolsIcons := ImageList;

  inherited Create(AParent, 0, 0, 50, 25 + 40 * Length(FTools));
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;

  for i := 0 to High(FTools) do begin
    ToolButton := TACustomSpeedButton.Create(Self, 20 + 40 * i, 5, 40, 40, '');
    FToolsIcons.GetBitmap(i, ToolButton.Glyph);
    ToolButton.Tag := i;
    ToolButton.OnClick := @ToolClickEvent;
  end;

  FCurrentTool := TPenTool.Create(FScene);
end;

procedure TToolsPanel.ForceRecreateCurrentTool;
begin
  FCurrentTool := FTools[FCurrentToolIndex].Tool.Create(FScene);
end;

procedure TToolsPanel.RecreateCurrentTool;
begin
  if FTools[FCurrentToolIndex].Recreate = True then
    ForceRecreateCurrentTool;
end;

procedure TToolsPanel.ToolClickEvent(Sender: TObject);
begin
  FCurrentToolIndex := TButton(Sender).tag;
  FCurrentTool := FTools[FCurrentToolIndex].Tool.Create(FScene);
  Palette.SetCurrentTool(FTools[FCurrentToolIndex].Name);
end;

function TToolsPanel.GetCurrentTool: TTool;
begin
  Result := FCurrentTool;
end;

procedure TToolsPanel.InitTool(Tool: ToolClass; IsRecreatingRequired: boolean;
  AName: string);
begin
  SetLength(FTools, Length(FTools) + 1);
  FTools[High(FTools)].Tool := Tool;
  FTools[High(FTools)].Recreate := IsRecreatingRequired;
  FTools[High(FTools)].Name := AName;
end;

end.
