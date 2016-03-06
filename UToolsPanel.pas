unit UToolsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, UTools, Graphics, Buttons, UCustomControls, UObjectMove,UPalette;

type
  TToolsPanel = class(TACustomPanel)
  public
    constructor Create(Scene: TCanvas; AParent: TComponent; ImageList: TImageList);
  private
    FScene: TCanvas;
    FToolsIcons: TImageList;
    procedure ToolClickEvent(Sender: TObject);
  end;

implementation

constructor TToolsPanel.Create(Scene: TCanvas; AParent: TComponent;
  ImageList: TImageList);
var
  i: integer;
  ToolButton: TACustomSpeedButton;
begin
  FScene := Scene;
  FToolsIcons := ImageList;
  inherited Create(AParent, 0, 0, 50, 25 + 40 * Length(Tools));
  Self.OnMouseDown := @PanelMove.OnMouseDown;
  Self.OnMouseMove := @PanelMove.OnMouseMove;
  Self.OnMouseUp := @PanelMove.OnMouseUp;
  Self.BevelInner := bvRaised;
  Self.BevelOuter := bvLowered;
  for i := 0 to High(Tools) do begin
    ToolButton := TACustomSpeedButton.Create(Self, 20 + 40 * i, 5, 40, 40, '');
    FToolsIcons.GetBitmap(i, ToolButton.Glyph);
    ToolButton.Tag := i;
    ToolButton.OnClick := @ToolClickEvent;
  end;
end;

procedure TToolsPanel.ToolClickEvent(Sender: TObject);
begin
  CurrentToolIndex := TButton(Sender).tag;
  Tool := Tools[CurrentToolIndex].Tool.Create(FScene);
  Palette.CurrentToolInvalidate;
end;

end.
