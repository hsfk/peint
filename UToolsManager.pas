unit UToolsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,UTools;

type
  ClassOfInstrument = class of TTools;
  ToolName = record
    Tool: ClassOFInstrument;
    NameOfTool: string;
  end;

  TToolsManager = class
  public
    FCanvas : tbitmap;
    tool : TTools;
    isZoomSelected : boolean;
    ToolTag : integer;
    constructor Create(FCanvas_ : tbitmap);
  private

  end;

var
  ToolsManager: TToolsManager;
  ClassRef: array of ToolName;

implementation

constructor TToolsManager.Create(FCanvas_ : tbitmap);
begin
  FCanvas := FCanvas_;
  tool := TPen.Create(FCanvas);
  isZoomSelected := false;
end;

procedure Init(Tool: ClassOfInstrument; NameOfTool: string);
begin
  setlength(ClassRef, length(ClassRef) + 1);
  classref[high(ClassRef)].Tool := Tool;
  classref[high(ClassRef)].NameOfTool := NameOfTool;
end;

initialization
  Init(TPen, 'Pen');
  Init(TFill, 'Fill');
  Init(TRectangle, 'Rectangle');
  Init(TEllipse, 'Ellipse');
end.
