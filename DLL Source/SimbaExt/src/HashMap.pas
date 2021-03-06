Unit HashMap;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface
(*
 Some very basic hashing.. You need to write your own hash function if you want
 the Key to be anything else then UInt32/Cardinal/LongWord.
*)
{$mode objfpc}{$H+}
{$macro on}
{$modeswitch advancedrecords}
{$inline on}

uses
  Classes, CoreTypes, SysUtils;

type
  //----------------------------------------\\
  //-----------<UInt32, ColorLAB>-----------\\
  TEntryLAB = record  Key: UInt32; Value: ColorLAB; end;
  TLABHash = class
  private
    FTable: Array of Array of TEntryLAB;
    FLen: Integer;
  public
    constructor Create(Size:Integer);
    function Get(Key: UInt32; out Value: ColorLAB): Boolean;  Inline;
    function Add(Key: UInt32; Value: ColorLAB): Boolean;    Inline;
    Destructor Destroy; override;
  end;
  
  //----------------------------------------\\
  //-------------<UInt32, Int32>------------\\
  TEntryI32 = record Key: UInt32; Value: Int32; end;
  TI32Hash = class
  private
    FTable: Array of Array of TEntryI32;
    FLen: Integer;
  public
    constructor Create(Size:Integer);
    function Get(Key: UInt32; var Value: Int32): Boolean; Inline;
    function Add(Key: UInt32; Value:Int32): Boolean;      Inline;
    Destructor Destroy; override;
  end;
  
  
  //----------------------------------------\\
  //------------<UInt32, Single>------------\\
  TEntryF32 = record Key: UInt32; Value: Single; end;
  TF32Hash = class
  private
    FTable: Array of Array of TEntryF32;
    FLen: Integer;
  public
    constructor Create(Size:Integer);
    function Get(Key: UInt32; var Value: Single): Boolean; Inline;
    function Add(Key: UInt32; Value:Single): Boolean;      Inline;
    Destructor Destroy; override;
  end;

  
  

  //----------------------------------------\\
  //--------------<Key, Value>--------------\\
  generic THashMap<K,V> = record
  private
    FHashFunc: function(const key:K): UInt32;
    FTable: Array of Array of record Key: UInt32; Value: Single; end;
    FLen: Integer;
  public
    procedure Create(Size:Int32; HashFunc:Pointer);
    function Get(Key: K; var Value: V): Boolean; Inline;
    function Add(Key: K; Value:V): Boolean;      Inline;
    procedure Destroy;
  end;
  


//Hash-functions to go with the hashtable.
function Hash(const k: Byte): UInt32; Inline; overload;
function Hash(const k: UInt32): UInt32; Inline; overload;
function Hash(const k: Int32): UInt32; Inline; overload;
function Hash(const k: Int64): UInt32; Inline; overload;
function Hash(const k: TPoint): UInt32; Inline; overload;
function Hash(const k: Single): UInt32; Inline; overload;
function Hash(const k: AnsiString): UInt32; Inline; overload;



//------------------------------------------------------------------------------
implementation

uses CoreMath;



(******************************* Hash Functions *******************************)
//Hash Byte - ofc this will only result in max 255 buckets no matter.
function Hash(const k: Byte): UInt32; Inline; overload;
begin
  Result := k;
end;

//Hash UInt32
function Hash(const k: UInt32): UInt32; Inline; overload;
begin
  Result := k;
end;

//Hash Int32
function Hash(const k: Int32): UInt32; Inline; overload;
begin
  Result := UInt32(k);
end;

//Hash a Int64
function Hash(const k: Int64): UInt32; Inline; overload;
begin
  Result := UInt32(k);
end;

//Hash 4 byte floating point (Single)
function Hash(const k: Single): UInt32; Inline; overload;
begin
  Result := UInt32(k);
end;

//Hash a TPoint (x,y record) | Should work well.
function Hash(const k: TPoint): UInt32; Inline; overload;
begin
  Result := UInt32((k.y * $0f0f1f1f) xor k.x);
end;


//Hash a string.. untested.
function Hash(const k: AnsiString): UInt32; Inline; overload;
var i: Int32;
begin
  Result := 0;
  for i:=1 to Length(k) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result)*8-2))) xor Byte(k[I]);
end;






(******************************** Hash Tables *********************************)
{
  <UInt32, ColorLAB>

  A supersimple hashmap, should be pretty decent performancewise as long as
  enough space is allocated, what ever size you give it it will allocate
  NextPowerOfTwo(size*1.25) mem, this allows us to use bitshifting instead of
  division (mod-operator), the "1.25" is just to overallocate a bit, for performance.

  FTable is like an "array of buckets", where each bucket represents a hash-index.
}
constructor TLABHash.Create(Size:Integer);
begin
  inherited Create;
  FLen := NextPow2m1(Trunc(Size * 1.25));
  SetLength(FTable, FLen+1);
end;

destructor TLABHash.Destroy;
begin
  inherited Destroy;
  SetLength(FTable, 0);
end;

function TLABHash.Add(Key: UInt32; Value: ColorLAB): Boolean; Inline;
var h,l,i: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if(self.FTable[h][i].Key = Key) then
    begin
      Self.FTable[h][i].Value := Value;
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;


function TLABHash.Get(Key: UInt32; out Value: ColorLAB): Boolean; Inline;
var
  h,i,l: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]) - 1;
  for i:=0 to l do
    if(self.FTable[h][i].Key = Key) then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;




{
  <UInt32, Int32>

  A supersimple hashmap, should be pretty decent performancewise as long as 
  enough space is allocated, what ever size you give it it will allocate
  NextPowerOfTwo(size*1.25) mem, this allows us to use bitshifting instead of
  division (mod-operator).

  FTable is like an "array of buckets", where each bucket represents a hash-index.
}
constructor TI32Hash.Create(Size:Integer);
begin
  inherited Create;
  FLen := NextPow2m1(Trunc(Size * 1.25));
  SetLength(FTable, FLen+1);
end;


destructor TI32Hash.Destroy;
begin
  inherited Destroy;
  SetLength(FTable, 0);
end;


function TI32Hash.Add(Key: UInt32; Value:Int32): Boolean; Inline;
var h,l,i: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if(self.FTable[h][i].Key = Key) then
    begin
      Self.FTable[h][i].Value := Value;
      Exit(True);
    end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;


function TI32Hash.Get(Key: UInt32; var Value: Int32): Boolean; Inline;
var
  h,i,l: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]) - 1;
  for i:=0 to l do
    if(self.FTable[h][i].Key = Key) then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;





{
  <UInt32, Single>

  A supersimple hashmap, should be pretty decent performancewise as long as 
  enough space is allocated, what ever size you give it it will allocate
  NextPowerOfTwo(size*1.25) mem, this allows us to use bitshifting instead of
  division (mod-operator).

  FTable is like an "array of buckets", where each bucket represents a hash-index.
}
constructor TF32Hash.Create(Size:Integer);
begin
  inherited Create;
  FLen := NextPow2m1(Trunc(Size * 1.25));
  SetLength(FTable, FLen+1);
end;


destructor TF32Hash.Destroy;
begin
  inherited Destroy;
  SetLength(FTable, 0);
end;


function TF32Hash.Add(Key: UInt32; Value: Single): Boolean; Inline;
var h,l,i: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if(self.FTable[h][i].Key = Key) then
      begin
        Self.FTable[h][i].Value := Value;
        Exit(True);
      end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;


function TF32Hash.Get(Key: UInt32; var Value: Single): Boolean; Inline;
var
  h,i,l: Int32;
begin
  h := Key and FLen;
  l := Length(Self.FTable[h]) - 1;
  for i:=0 to l do
    if(self.FTable[h][i].Key = Key) then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;
















{
  <Key, Value>

  A supersimple hashmap, should be pretty decent performancewise as long as
  enough space is allocated, what ever size you give it it will allocate
  NextPowerOfTwo(size*1.25) mem, this allows us to use bitshifting instead of
  division (mod-operator).

  FTable is like an "array of buckets", where each bucket represents a hash-index.
}
procedure THashMap.Create(Size:Int32; HashFunc:Pointer);
type THashFunc = function(const key:K): UInt32;
begin
  FHashFunc := THashFunc(HashFunc);
  FLen := NextPow2m1(Trunc(Size * 1.25));
  SetLength(FTable, FLen+1);
end;


procedure THashMap.Destroy;
begin
  SetLength(FTable, 0);
end;


function THashMap.Add(Key: K; Value: V): Boolean; Inline;
var h,l,i: Int32;
begin
  h := FHashFunc(Key) and FLen;
  l := Length(Self.FTable[h]);
  for i:=0 to l-1 do
    if(self.FTable[h][i].Key = Key) then
      begin
        Self.FTable[h][i].Value := Value;
        Exit(True);
      end;
  SetLength(Self.FTable[h], l+1);
  Self.FTable[h][l].Key := Key;
  Self.FTable[h][l].Value := Value;
  Result := True;
end;


function THashMap.Get(Key: K; var Value: V): Boolean; Inline;
var
  h,i,l: Int32;
begin
  h := FHashFunc(Key) and FLen;
  l := Length(Self.FTable[h]) - 1;
  for i:=0 to l do
    if(self.FTable[h][i].Key = Key) then
    begin
      Value := Self.FTable[h][i].Value;
      Exit(True);
    end;
  Result := False;
end;

end.
