unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigure, Graphics, UMainSceneUtils, UPointUtils;

type
  Nodeptr = ^Node;

  Node = record
    Data: array of TFigure;
    Next: Nodeptr;
    Prev: Nodeptr;
  end;

  THistory = class
  public
    constructor Create(Scene: TCanvas);
    procedure ReplaceLast(Figure: TFigure);
    procedure Insert(Figure: TFigure);
    procedure Show;
    procedure Undo;
    procedure Redo;
    function DataLength: integer;
    function GetFigure(Index: integer): TFigure;
    procedure Select(Index: integer);
    procedure SelectFiguresInRect(AMinPoint, AMaxPoint: TPoint);
    procedure Deselect;
    procedure DeleteLastFigure;
  private
    FHighLightedFigures: array of integer;
    FHistoryPosition: Nodeptr;
    FScene: TCanvas;
    FHead: Nodeptr;
    FTail: Nodeptr;
    procedure CopyLastAction;
    procedure PushBack(Node: Nodeptr);
    procedure Delete(Node: Nodeptr);
    function CopyNode(Node: Nodeptr): Nodeptr;
    function NewNode(Figure: TFigure): Nodeptr;
  end;

var
  History: THistory;

implementation

constructor THistory.Create(Scene: TCanvas);
begin
  FScene := Scene;
  new(FHead);
  new(FTail);
  FHead^.Prev := nil;
  FTail^.Next := nil;
  FHead^.Next := FTail;
  FTail^.Prev := FHead;
  FHistoryPosition := FHead;
end;

procedure THistory.ReplaceLast(Figure: TFigure);
begin
  FHistoryPosition^.Data[High(FHistoryPosition^.Data)] := Figure;
end;

procedure THistory.Insert(Figure: TFigure);
begin
  if FHistoryPosition = FHead then begin
    PushBack(NewNode(Figure));
    FHistoryPosition := FHistoryPosition^.Next;
    exit;
  end;
  while FHistoryPosition^.Next <> FTail do
    Delete(FHistoryPosition^.Next);
  CopyLastAction;
  FHistoryPosition := FHistoryPosition^.Next;
  SetLength(FHistoryPosition^.Data, Length(FHistoryPosition^.Data) + 1);
  ReplaceLast(Figure);
end;

procedure THistory.Show;
var
  i: integer;
begin
  MainSceneUtils.ClearScene;
  if Length(FHighLightedFigures) > 0 then
    for i := 0 to Length(FHighLightedFigures) - 1 do
      FHistoryPosition^.Data[FHighLightedFigures[i]].HighLight;
  if FHistoryPosition <> FHead then
    for i := 0 to Length(FHistoryPosition^.Data) - 1 do
      FHistoryPosition^.Data[i].Draw;
end;

procedure THistory.Undo;
begin
  if FHistoryPosition^.Prev <> FHead then
    FHistoryPosition := FHistoryPosition^.Prev;
  Show;
end;

procedure THistory.Redo;
begin
  if FHistoryPosition^.Next <> FTail then
    FHistoryPosition := FHistoryPosition^.Next;
  Show;
end;

function THistory.DataLength: integer;
begin
  Result := Length(FHistoryPosition^.Data);
end;

function THistory.GetFigure(Index: integer): TFigure;
begin
  Result := FHistoryPosition^.Data[Index];
end;

procedure THistory.Select(Index: integer);
begin
  SetLength(FHighLightedFigures, Length(FHighLightedFigures) + 1);
  FHighLightedFigures[High(FHighLightedFigures)] := Index;
end;

procedure THistory.SelectFiguresInRect(AMinPoint, AMaxPoint: TPoint);
var
  i: integer;
  MaxPoint: TPoint;
  MinPoint: TPoint;
begin
  Deselect;
  if Length(FHistoryPosition^.Data) = 1 then
    Exit;
  for i := Length(FHistoryPosition^.Data) - 2 downto 0 do begin
    MaxPoint := FHistoryPosition^.Data[i].GetMaxPoint;
    MinPoint := FHistoryPosition^.Data[i].GetMinPoint;
    if IsIntersect(AMinPoint, AMaxPoint, MaxPoint, MinPoint) then
      Select(i);
  end;
end;

procedure THistory.Deselect;
begin
  SetLength(FHighLightedFigures, 0);
end;

procedure THistory.DeleteLastFigure;
begin
  SetLength(FHistoryPosition^.Data, Length(FHistoryPosition^.Data) - 1);
end;

procedure THistory.CopyLastAction;
var
  Temp: Nodeptr;
begin
  Temp := FTail^.Prev;
  Temp := CopyNode(Temp);
  PushBack(Temp);
end;

procedure THistory.PushBack(Node: Nodeptr);
var
  Temp: Nodeptr;
begin
  Temp := FTail^.Prev;
  FTail^.Prev := Node;
  Node^.Next := FTail;
  Node^.Prev := Temp;
  Temp^.Next := Node;
end;

procedure THistory.Delete(Node: Nodeptr);
begin
  if (Node = FHead) or (Node = FTail) then
    Exit;
  Node^.Prev^.Next := Node^.Next;
  Node^.Next^.Prev := Node^.Prev;
  SetLength(Node^.Data, 0);
  Dispose(Node);
end;

function THistory.CopyNode(Node: Nodeptr): Nodeptr;
var
  i: integer;
begin
  new(Result);
  SetLength(Result^.Data, Length(Node^.Data));
  for i := 0 to Length(Node^.Data) - 1 do
    Result^.Data[i] := Node^.Data[i].CopyFigure(Node^.Data[i]);
  Result^.Next := nil;
  Result^.Prev := nil;
end;

function THistory.NewNode(Figure: TFigure): Nodeptr;
begin
  new(Result);
  SetLength(Result^.Data, 1);
  Result^.Data[0] := Figure;
  Result^.Prev := nil;
  Result^.Next := nil;
end;

end.
