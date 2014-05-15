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

function Slice(Arr:TIntArray; Start,Stop:Int32; Step:Int32=1): TIntArray; overload;
var P,R:^Int32; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);   

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end; 


function Slice(Arr:TExtArray; Start,Stop:Int32; Step:Int32=1): TExtArray; overload;
var P,R:^Extended; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);   

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end; 


function Slice(Arr:TPointArray; Start,Stop:Int32; Step:Int32=1): TPointArray; overload;
var P,R:^TPoint; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);    

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;


function Slice(Arr:TByteArray; Start,Stop:Int32; Step:Int32=1): TByteArray; overload;
var P,R:^Byte; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);   

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;


function Slice(Arr:TBoxArray; Start,Stop:Int32; Step:Int32=1): TBoxArray; overload;
var P,R:^TBox; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop >= h) then Stop := h-1;
    False: if (Start >= h) then Start := h-1;
  end;
  Start := Modulo(start,h);
  Stop  := Modulo(stop,h);   

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @Arr[start];
  R := @Result[0];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) < L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;


function Slice(Arr:String; Start,Stop:Int32; Step:Int32=1): String; overload;
var P,R:PChar; l,h:uInt32;
begin
  h := Length(Arr);
  case (Step > 0) of
    True:  if (Stop > h) then Stop := h;
    False: if (Start > h) then Start := h;
  end;
  Start := Modulo(start,h+1);
  Stop  := Modulo(stop,h+1);

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @arr[start];
  R := @Result[1];
  L := uInt32(@Result[Length(Result)]);
  while uInt32(R) <= L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;


//========| 2-dimesional |=====================================================|

//Make a TIA of indices. Then sliceTIA, and copy the arr-items the indices refers to..
function Slice(Arr:T2DIntArray; Start,Stop:Int32; Step:Int32=1): T2DIntArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;


function Slice(Arr:T2DExtArray; Start,Stop:Int32; Step:Int32=1): T2DExtArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;


function Slice(Arr:T2DPointArray; Start,Stop:Int32; Step:Int32=1): T2DPointArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;


function Slice(Arr:T2DByteArray; Start,Stop:Int32; Step:Int32=1): T2DByteArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;


function Slice(Arr:T2DBoxArray; Start,Stop:Int32; Step:Int32=1): T2DBoxArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;


function Slice(Arr:TStringArray; Start,Stop:Int32; Step:Int32=1): TStringArray; overload;
var i,h:UInt32; tmp:TIntArray;
begin
  h := Length(Arr);
  SetLength(TMP, h);
  for i:=0 to h-1 do TMP[i] := i;
  TMP := Slice(TMP,Start,Stop,Step);
  SetLength(Result,Length(TMP));
  for i:=0 to High(TMP) do
    Result[i] := Copy(Arr[TMP[i]], 0, Length(Arr[TMP[i]]));
end;
