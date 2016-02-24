unit UPackUnpack;

{$mode objfpc}{$H+}
{$ASMMODE intel}
interface

uses
  Classes, SysUtils, Math, UTree, UBitOps, UFileAnalization;

type
  Cache = array [0..255] of string;
  FrequencyTable = array [0..255] of longword;

  THuffman = class
    constructor Create(var MStream: TMemoryStream);
  public
    function Pack(filename: string): boolean;
    function Unpack: TMemoryStream;
  private
    MsInput: TMemoryStream;
  end;

var
  Routes: cache;
  Table: FrequencyTable;
  LastNills: byte;
  Ftree: Ttree;
  FFile: Tfile;
  FBit: TBitOps;

implementation

constructor THuffman.Create(var MStream: TMemoryStream);
begin
  MsInput := MStream;
  LastNills := 0;
  MsInput.Seek(0, soBeginning);
end;

function THuffman.Unpack: TMemoryStream;
var
  SizeOfFile: int64;
  Symbol: byte;
  i: byte;
  Root: nodeptr;
  Tmp: nodeptr;
  Byte_: string;
  LCache: LengthCache;
begin
  Result := TMemoryStream.Create;
  MsInput.Seek(0, soBeginning);

  Byte_ := '';
  for i := 0 to 3 do begin
    Symbol := MsInput.ReadByte;
    Byte_ += chr(Symbol);
  end;
  if Byte_ <> 'SOLG' then
    exit;

  for i := 0 to 255 do begin
    Symbol := MsInput.ReadByte;
    LCache[i] := Symbol;//по таблице длин(она же высота в дереве) будем строить дерево
  end;

  Table := Ftree.GetFTable(LCache);
  Root := Ftree.MakeTree(Table);

  SizeOfFile := MsInput.Size - 256 - 4 - 1 - 1;   //4 solg 256 table 1 - last nil
  Tmp := Root;
  Byte_ := '00000000';

  while SizeOfFile <> 0 do begin
    Symbol := MsInput.ReadByte;
    for i := 0 to 7 do begin
      if ((Symbol and round(power(2, 7 - i))) <> 0) and (Tmp^.right <> nil) then
        Tmp := Tmp^.right//если 7-i бит == 1
      else if Tmp^.left <> nil then
        Tmp := Tmp^.left;
      if Tmp^.flag = True then begin
        Result.WriteByte(Tmp^.key);
        Tmp := Root;
      end;
    end;
    SizeOfFile -= 1;
  end;

  Symbol := MsInput.ReadByte;
  Byte_ := '00000000';
  for i := 0 to 7 do begin
    if (Symbol and round(power(2, 7 - i))) <> 0 then
      Byte_[i + 1] := '1'
    else
      Byte_[i + 1] := '0';
  end;
  LastNills := MsInput.ReadByte;

  if LastNills <> 11 then begin
    for i := 1 to lastNills do
      Byte_ := copy(Byte_, 2, length(Byte_));  //2 - убирает 1й ноль
  end;

  for i := 1 to length(Byte_) do begin
    if (Byte_[i] = '1') and (Tmp^.right <> nil) then
      Tmp := Tmp^.right
    else if Tmp^.left <> nil then
      Tmp := Tmp^.left;
    if Tmp^.flag = True then begin
      Result.WriteByte(Tmp^.key);
      Tmp := Root;
    end;
  end;
  MsInput.Free;
end;

function THuffman.Pack(filename: string): boolean;
var
  Output: Text;
  SizeOfFile: int64;
  Symbol: byte;
  i: byte;
  BCache: BitCache;
  LCache: LengthCache;
  Buffer: longword;
  Temp: longword;
  BufLength: byte;
begin
  Assign(Output, filename);
  rewrite(Output);

  Table := FFile.GetFrequency(MsInput);
  Routes := Ftree.GetTable(Table);
  LCache := Fbit.GetLengthCache(Routes);
  BCache := Ftree.GetBCache(LCache);

  Write(Output, 'SOLG');
  for i := 0 to 255 do
    Write(Output, chr(LCache[i]));

  SizeOfFile := MsInput.Size;
  Buffer := 0;
  BufLength := 0;

  MsInput.Seek(0, soBeginning);
  while SizeOfFile <> 0 do begin
    while (bufLength < 8) and (SizeOfFile <> 0) do begin
      Symbol := MsInput.ReadByte;
      Temp := BCache[Symbol];//BCache-путь к символу по дереву, записанный в битах числа
      BufLength += LCache[Symbol];//LCache - длина пути
      Temp := Temp shl (32 - bufLength);//buffer - очередь битов путей в lworde
      Buffer := Buffer or Temp;          //кладем пути в очередь
      SizeOfFile -= 1;
    end;
    if SizeOfFile = 0 then
      break;

    asm
             MOVSS   XMM0, BUFFER
             PSRLD   XMM0, 24
             MOVSS   Temp, XMM0
             MOVSS   XMM0, BUFFER      //вытаскиваем 8 бит из очереди в Temp
             PSLLD   XMM0, 8
             MOVSS   BUFFER, XMM0
             SUB     BUFLENGTH, 8
    end;
    Write(Output, chr(Temp));
  end;

  while BufLength >= 8 do begin //если в очереди больше 1 байта
    Temp := buffer shr 24;
    buffer := buffer shl 8;
    BufLength -= 8;
    Write(Output, chr(Temp));
  end;

  if BufLength <> 0 then begin
    LastNills := 8 - BufLength; //LastNills - последний байт файла отчечающий за
    buffer := buffer shr lastnills;//остаток, те если остаток < 8 бит то к нему
    buffer := buffer shr 24;//накинутся LastNills нулей
    Write(Output, chr(buffer));
  end;

  if (LastNills < 8) and (LastNills > 0) then
    Write(Output, chr(LastNills))
  else
    Write(Output, chr(11));//если нулей нет

  MsInput.Free;
  Close(Output);
  Result := True;
end;

end.
