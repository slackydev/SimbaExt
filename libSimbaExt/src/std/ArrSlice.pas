{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*
  Slicing similar to slice in Python, tho goes from 'start to and including stop'
  Can be used to eg reverse an array, and at the same time allows you to "step" past items.
  You can give it negative start, and stop, then it will wrap around based on length(..).
  
  Examples:
  | TIA := [0,1,2,3,4,5,6,7,8,9];
  | Slice(TIA, 9,0,-1)  = [9,8,7,6,5,4,3,2,1,0]
  | Slice(TIA, 9,0,-2)  = [9,7,5,3,1]
  | Slice(TIA, 3,7,1)   = [3,4,5,6,7]
  | Slice(TIA, 0,-2,1)  = [0,1,2,3,4,5,6,7,8]
  ------------------------------------------------------
*)

//========| 1-dimesional (Simple).. |=========================================|

{$define Slice1DBody :=
  if (Start = High(Int64)) then
    if Step < 0 then Start := -1
    else Start := 0;
  if (Stop = High(Int64)) then
    if Step > 0 then Stop := -1
    else Stop := 0;

  h := Length(Arr);
  if h = 0 then Exit();
  
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);

  if (Start > Stop) and (Step > 0) then
    NewException(exModuloFailure);

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := PtrUInt(@Result[High(Result)]);
  while PtrUInt(R) <= L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
}

function Slice(const Arr:TIntArray; Start,Stop:Int64; Step:Int32=1): TIntArray; overload;
var P,R:PInt32; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TExtArray; Start,Stop:Int64; Step:Int32=1): TExtArray; overload;
var P,R:PExtended; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TFloatArray; Start,Stop:Int64; Step:Int32=1): TFloatArray; overload;
var P,R:PSingle; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TDoubleArray; Start,Stop:Int64; Step:Int32=1): TDoubleArray; overload;
var P,R:PDouble; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TPointArray; Start,Stop:Int64; Step:Int32=1): TPointArray; overload;
var P,R:PPoint; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TByteArray; Start,Stop:Int64; Step:Int32=1): TByteArray; overload;
var P,R:PUInt8; l:Int32; H:PtrUInt;
begin Slice1DBody end;


function Slice(const Arr:TBoxArray; Start,Stop:Int64; Step:Int32=1): TBoxArray; overload;
var P,R:^TBox; l:Int32; H:PtrUInt;
begin Slice1DBody end;


//Special
function Slice(const Arr:String; Start,Stop:Int64; Step:Int32=1): String; overload;
var P,R:PChar; l:Int32; H:PtrUInt;
begin
  if (Start = High(Int64)) then
    if Step < 0 then Start := -1
    else Start := 1;
  if (Stop = High(Int64)) then
    if Step > 0 then Stop := -1
    else Stop := 1;

  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop > h) then Stop := h;
    False: if (Start > h) then Start := h;
  end;
  Start := Modulo(start,h+1);
  Stop  := Modulo(stop,h+1);

  if (Start > Stop) and (Step > 0) then
    NewException(exModuloFailure);

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @arr[start];
  R := @Result[1];
  L := PtrUInt(@Result[Length(Result)]);
  while PtrUInt(R) <= L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;


//========| 2-dimesional |=====================================================|

{$define Slice2DBody :=
  if (Start = High(Int64)) then
    if Step < 0 then Start := -1
    else Start := 0;
  if (Stop = High(Int64)) then
    if Step > 0 then Stop := -1
    else Stop := 0;

  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
}

//Make a TIA of indices. Then sliceTIA, and copy the arr-items the indices refers to..
function Slice(const Arr:T2DIntArray; Start,Stop:Int64; Step:Int32=1): T2DIntArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DExtArray; Start,Stop:Int64; Step:Int32=1): T2DExtArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DFloatArray; Start,Stop:Int64; Step:Int32=1): T2DFloatArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DDoubleArray; Start,Stop:Int64; Step:Int32=1): T2DDoubleArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DPointArray; Start,Stop:Int64; Step:Int32=1): T2DPointArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DByteArray; Start,Stop:Int64; Step:Int32=1): T2DByteArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:T2DBoxArray; Start,Stop:Int64; Step:Int32=1): T2DBoxArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;


function Slice(const Arr:TStringArray; Start,Stop:Int64; Step:Int32=1): TStringArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin Slice2DBody end;
