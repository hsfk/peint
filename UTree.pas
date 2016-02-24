unit UTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt, bitops;

type
  nodeptr = ^node;

  node = record
    key: byte;
    weight: longint;
    prev, Next, right, left: nodeptr;
    flag: boolean; { list ili net }
  end;

  FrequencyTable = array [0..255] of longword;
  Cache = array [0..255] of string;

  TTree = class
    Table: FrequencyTable;
  public
    function GetTable(SomeTable: FrequencyTable): cache;
    function MakeTree(SomeTable: FrequencyTable): nodeptr;
    function GetBCache(SomeLCache: LengthCache): BitCache;
    function GetFTable(SomeLCache: LengthCache): FrequencyTable;
  private
    procedure GetRoutes(tmp: nodeptr; s: string);
    function CreateNode(key: byte; weight: longint): nodeptr;
    function merge(node1, node2: nodeptr): nodeptr;
    procedure AddToQueue(newnode: nodeptr);
    function Pop: nodeptr;
  end;

var
  Qhead: nodeptr;
  Routes: cache;
  Fbit: TBitOps;
  BCache: BitCache;
  LCache: lengthCache;

implementation

function TTree.GetTable(SomeTable: FrequencyTable): cache;
var
  root: nodeptr;
begin
  root := MakeTree(SomeTable);
  if (root^.right = nil) and (root^.left = nil) then
    Routes[root^.key] := '1'
  else
    GetRoutes(root, '');
  Result := routes;
end;

function TTree.MakeTree(SomeTable: FrequencyTable): nodeptr;
var
  i: integer;
  root, newnode: nodeptr;
begin
  new(Qhead);
  Qhead^.Next := nil;
  Qhead^.prev := nil;
  for i := 0 to 255 do begin
    if sometable[i] > 0 then begin
      newnode := CreateNode(i, SomeTable[i]);
      AddToQueue(newnode);
    end;
  end;
  if Qhead^.Next^.Next = nil then begin
    Result := pop;
    exit;
  end;
  while Qhead^.Next^.Next <> nil do begin
    root := merge(pop, pop);
    AddToQueue(root);
  end;
  Result := root;
end;

function TTree.GetBCache(SomeLCache: LengthCache): BitCache;
var
  root: nodeptr;
  TestTable: FrequencyTable;
begin
  TestTable := GetFTable(SomeLCache);
  root := MakeTree(TestTable);
  GetRoutes(root, '');
  Result := Fbit.GetBitCache(Routes);
end;

function TTree.GetFTable(SomeLCache: LengthCache): FrequencyTable;
var
  Weights: array [0..24] of longword;
  i: byte;
begin
  Weights[0] := 0;
  Weights[1] := 8388608;
  for i := 2 to 24 do
    Weights[i] := Weights[i - 1] div 2;
  for i := 0 to 255 do
    Result[i] := Weights[SomeLCache[i]];
end;

procedure TTree.GetRoutes(tmp: nodeptr; s: string);
begin
  if tmp^.right <> nil then
    getroutes(tmp^.right, s + '1');
  if tmp^.flag = True then
    Routes[tmp^.key] := s;
  if tmp^.left <> nil then
    getroutes(tmp^.left, s + '0');
end;

function TTree.CreateNode(key: byte; weight: longint): nodeptr;
begin
  new(Result);
  Result^.key := key;
  Result^.weight := weight;
  Result^.prev := nil;
  Result^.Next := nil;
  Result^.right := nil;
  Result^.left := nil;
  Result^.flag := True;
end;

function TTree.merge(node1, node2: nodeptr): nodeptr;
begin
  new(Result);
  Result^.key := 0;
  Result^.prev := nil;
  Result^.Next := nil;
  Result^.right := node2;
  Result^.left := node1;
  Result^.flag := False;
  Result^.weight := node1^.weight + node2^.weight;
end;

procedure TTree.AddToQueue(newnode: nodeptr);
var
  tmp, tmpptr: nodeptr;
begin
  if Qhead^.Next = nil then begin
    Qhead^.Next := newnode;
    newnode^.prev := Qhead;
  end
  else begin
    tmp := Qhead^.Next;
    while (tmp^.Next <> nil) and (tmp^.weight < newnode^.weight) do
      tmp := tmp^.Next;
    if (tmp^.Next = nil) and (newnode^.weight > tmp^.weight) then begin
      tmp^.Next := newnode;
      newnode^.prev := tmp;
      newnode^.Next := nil;
    end
    else begin
      tmpptr := tmp^.prev;
      tmp^.prev := newnode;
      newnode^.Next := tmp;
      newnode^.prev := tmpptr;
      newnode^.prev^.Next := newnode;
    end;
  end;
end;

function TTree.Pop: nodeptr;
begin
  Result := Qhead^.Next;
  if Qhead^.Next <> nil then begin
    Qhead^.Next := Result^.Next;
    if Qhead^.Next <> nil then
      Qhead^.Next^.prev := Qhead;
  end;
end;

end.
