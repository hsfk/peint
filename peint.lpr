program peint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, UPalette, UObjectMove, utoolspanel, UScrollBar,
  UFigureHistoryManager, UFigure, UHistory, UZoom, UPointUtils, UTools,
UMainSceneUtils, UEditPanel;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

