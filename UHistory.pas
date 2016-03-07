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
    procedure MoveUp(Index: integer);
    procedure MoveDown(Index: integer);
    procedure DeleteSelected;
    procedure DeleteFigure(Index: integer);
    function DataLength: integer;
    function GetFigure(Index: integer): TFigure;
    procedure Select(Index: integer);
    procedure SelectFiguresInRect(AMinPoint, AMaxPoint: TPoint);
    procedure Deselect;
    procedure DeleteLastFigure;
  private
    FSelectedFigures: array of integer;
    FHistoryPosition: Nodeptr;
    FScene: TCanvas;
    FHead: Nodeptr;
    FTail: Nodeptr;
    procedure CopyLastAction;
    procedure PushBack(Node: Nodeptr);
    procedure DeleteNode(Node: Nodeptr);
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
  FHistoryPosition^.Data[DataLength - 1] := Figure;
end;

procedure THistory.Insert(Figure: TFigure);
begin
  if FHistoryPosition = FHead then begin
    PushBack(NewNode(Figure));
    FHistoryPosition := FHistoryPosition^.Next;
    exit;
  end;
  while FHistoryPosition^.Next <> FTail do
    DeleteNode(FHistoryPosition^.Next);
  CopyLastAction;
  FHistoryPosition := FHistoryPosition^.Next;
  SetLength(FHistoryPosition^.Data, DataLength + 1);
  ReplaceLast(Figure);
end;

procedure THistory.Show;
var
  i: integer;
begin
  MainSceneUtils.ClearScene;
  if FHistoryPosition <> FHead then
    for i := 0 to Length(FHistoryPosition^.Data) - 1 do
      FHistoryPosition^.Data[i].Draw;
  if Length(FSelectedFigures) > 0 then
    for i := 0 to Length(FSelectedFigures) - 1 do
      GetFigure(FSelectedFigures[i]).HighLight;
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

procedure THistory.MoveUp(Index: integer);
var
  Temp: TFigure;
begin
  if Index = 0 then
    Exit;
  Temp := GetFigure(Index - 1);
  FHistoryPosition^.Data[Index - 1] := GetFigure(Index);
  FHistoryPosition^.Data[Index] := Temp;
  Show;
end;

procedure THistory.MoveDown(Index: integer);
var
  Temp: TFigure;
begin
  if Index = DataLength - 1 then
    Exit;
  Temp := GetFigure(Index + 1);
  FHistoryPosition^.Data[Index + 1] := GetFigure(Index);
  FHistoryPosition^.Data[Index] := Temp;
  Show;
end;

procedure THistory.DeleteSelected;
var
  i: integer;
begin
  if Length(FSelectedFigures) = 0 then
    Exit;
  for i := 0 to Length(FSelectedFigures) - 1 do
    DeleteFigure(FSelectedFigures[i]);
  Deselect;
  Show;
end;

procedure THistory.DeleteFigure(Index: integer);
var
  i: integer;
begin
  CopyLastAction;
  FHistoryPosition := FHistoryPosition^.Next;
  for i := Index to DataLength - 2 do
    FHistoryPosition^.Data[i] := GetFigure(i + 1);
  SetLength(FHistoryPosition^.Data, DataLength - 1);
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
  SetLength(FSelectedFigures, Length(FSelectedFigures) + 1);
  FSelectedFigures[High(FSelectedFigures)] := Index;
end;

procedure THistory.SelectFiguresInRect(AMinPoint, AMaxPoint: TPoint);
var
  i: integer;
  MaxPoint: TPoint;
  MinPoint: TPoint;
begin
  Deselect;
  if DataLength = 1 then
    Exit;
  for i := DataLength - 2 downto 0 do begin
    MaxPoint := GetFigure(i).GetMaxPoint;
    MinPoint := GetFigure(i).GetMinPoint;
    if IsIntersect(AMinPoint, AMaxPoint, MaxPoint, MinPoint) then
      Select(i);
  end;
end;

procedure THistory.Deselect;
begin
  SetLength(FSelectedFigures, 0);
end;

procedure THistory.DeleteLastFigure;
begin
  SetLength(FHistoryPosition^.Data, DataLength - 1);
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

procedure THistory.DeleteNode(Node: Nodeptr);
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
