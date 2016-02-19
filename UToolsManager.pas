unit UToolsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTools, Graphics;

type
  TToolsManager = class
    public
      FTool : TTools;
      FToolTag : integer;
      FCanvas : tbitmap;
      constructor Create(var Canvas_ : tbitmap);
  end;

  var ToolsManager : TToolsManager;
implementation

constructor TToolsManager.Create(var Canvas_ : tbitmap);
begin
  FCanvas := Canvas_;
end;

end.

