unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigure, Graphics, UMainSceneUtils;

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
    function DataLength : Integer;
    function GetFigure(Index : Integer) : TFigure;
  private
    HistoryPosition: Nodeptr;
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
  HistoryPosition := FHead;
end;

procedure THistory.ReplaceLast(Figure: TFigure);
begin
  HistoryPosition^.Data[High(HistoryPosition^.Data)] := Figure;
end;

procedure THistory.Insert(Figure: TFigure);
begin
  if HistoryPosition = FHead then begin
    PushBack(NewNode(Figure));
    HistoryPosition := HistoryPosition^.Next;
    exit;
  end;
  while HistoryPosition^.Next <> FTail do
    Delete(HistoryPosition^.Next);
  CopyLastAction;
  HistoryPosition := HistoryPosition^.Next;
  SetLength(HistoryPosition^.Data, Length(HistoryPosition^.Data) + 1);
  ReplaceLast(Figure);
end;

procedure THistory.Show;
var
  i: integer;
begin
  MainSceneUtils.ClearScene;
  if HistoryPosition <> FHead then
    for i := 0 to Length(HistoryPosition^.Data) - 1 do
      HistoryPosition^.Data[i].Draw;
end;

procedure THistory.Undo;
begin
  if HistoryPosition^.Prev <> FHead then
    HistoryPosition := HistoryPosition^.Prev;
  Show;
end;

procedure THistory.Redo;
begin
  if HistoryPosition^.Next <> FTail then
    HistoryPosition := HistoryPosition^.Next;
  Show;
end;

function THistory.DataLength : Integer;
begin
  Result := Length(HistoryPosition^.Data);
end;

function THistory.GetFigure(Index : Integer) : TFigure;
begin
  Result := HistoryPosition^.Data[Index];
end;

procedure THistory.CopyLastAction;
var
  tmp: Nodeptr;
begin
  tmp := FTail^.Prev;
  tmp := CopyNode(tmp);
  PushBack(tmp);
end;

procedure THistory.PushBack(Node: Nodeptr);
var
  tmp: Nodeptr;
begin
  tmp := FTail^.Prev;
  FTail^.Prev := Node;
  Node^.Next := FTail;
  Node^.Prev := tmp;
  tmp^.Next := Node;
end;

procedure THistory.Delete(Node: Nodeptr);
begin
  if (Node = FHead) or (Node = FTail) then
    exit;
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
