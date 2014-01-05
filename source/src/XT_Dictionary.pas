Unit XT_HashTable;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Simply a suboptimal simple dictionary.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  XT_Types, SysUtils;

type
  //----------------------------------\\
  private
  Dictionary<K,V> = record
    Table: Array of V;
    Len: Integer;
    ItemCount: Integer; 
  public
    procedure Create();
    function New(Size:Integer): Dictionary<K,V>;
    function Get(Key:K; var Res:V): Boolean; inline;
    function Add(Key:K; Value:K): Boolean; inline;
    function Del(Value:K): Boolean; inline;
    procedure Free();
  end;
 
//--------------------------------------------------
implementation

uses 
  XT_Math;


(*------------------------------------------------------------------|
 Hash functions depending on type | does not care if it overflows
 Hash-functions are mostly untested, and basic.
|------------------------------------------------------------------*)
function HashFunc(key:Integer; Len:Integer): Integer;
begin
  Result := IModulo(Key, Len);
end;

//--------
function HashFunc(key:Extended; Len:Integer): Integer; overload;
begin
  Result := Ceil(Modulo((Key*1000000), Len));
end;

//--------
function HashFunc(key:Single; Len:Integer): Integer; overload;
begin
  Result := Ceil(Modulo((Key*100000), Len));
end;
 
//--------
function HashFunc(key:Char; Len:Integer): Integer; overload;
begin
  Result := Chr(key) mod Len;
end;

//--------
function HashFunc(key:String; Len:Integer): Integer; overload;
var
  hash,i:Integer;
begin
  hash := 5381;
  for i:=1 to Length(key) do
    hash := ((hash shl 5) + hash) + ord(key[i]);
  Result := Ceil(Modulo(hash, Len));
end;

//--------
function HashFunc(key:TPoint; Len:Integer): Integer; overload;
var
  hash,i:Integer;
begin
  Result := IModulo(Sqr(key.x, key.y), Len);
end;
 

  
(*------------------------------------------------------------------|
 Dinctionary
|------------------------------------------------------------------*)
const MINDICTSIZE = 32768;
const MAXDICTLOAD = 0.8;
const MINDICTLOAD = 1/3;

procedure Dictionary.Create();
begin
  Len := MINDICTSIZE;
  SetLength(Table, Len);
end;


function Dictionary.New(Size:Integer): Dictionary<K,V>;
begin
  Result.Len := Max(MINDICTSIZE, Size);
  SetLength(Result.Table, Result.Len);
end;


procedure Dictionary.Free();
begin
  SetLength(Table, 0);
end;


procedure Dictionary.CheckResize();
var 
  Low,newlen,i:Integer;
  Tmp: Dictionary<K,V>;
begin
  if ItemCount >= (Len * MAXDICTLOAD) then 
  begin
    Len := Len + Len;
    SetLength(Table, Len);
    
    Exit;
  end 
  low := (Len * MINDICTLOAD)
  if (ItemCount < low) and
     (low > MINDICRSIZE) then
  begin
    newlen := Len shr 1;
    Tmp := Dictionary.New(newlen);
    for i:=0 to Len do
      ...
    SetLength(Table, Len);
  end;
end;



function Dictionary.Get(Key:K; var Res:V): Boolean;
var
  Hash,I:Integer;
begin
  Hash := HashFunc(Key,Len);
  for i:=0 to Len-1 do
  begin
    if (Table[Hash].Isset = False) or (Table[Hash].Key = Key) then
      Break;
    Inc(Hash);
    if (Hash = Len) then Hash := 0;
  end;
  if (Table[Hash].Isset = False) then Exit(False);

  Res := Table[Hash].Value;
  Result := True;
end;



//Adds a value to the dictionary with the given key
function Dictionary.Add(Key:K; Value:V): Boolean;
var
  Hash,i:Integer;
begin
  Hash := HashFunc(Key,Len);
  for i:=0 to Len-1 do
  begin
    if (Table[Hash].Key = Key) then
      Break;
    if (Table[Hash].Isset <> True) then
    begin
      Break;
      Inc(ItemCount);
    end;
    Inc(Hash);
    if (Hash = Len) then Hash := 0;
  end;
  
  Self.CheckResize();
  
  Table[Hash].Value := Value;
  if (Table[Hash].Isset=False) then
  begin
    Table[Hash].Key := Key;
    Table[Hash].Isset := True;
  end;
  Result := True;
end;


function Dictionary.Del(Value:V): Boolean;
begin

end;

end.
