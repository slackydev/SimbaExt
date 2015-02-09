{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Serches for an item in an array, item can be an array as well.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

procedure TIAAppend(var Arr:TIntArray; val:Int32; var realLen:Int32); inline;
var L:Int32;
begin
  L := Length(Arr);
  if realLen >= L then SetLength(Arr, L+L+1);
  Arr[realLen] := val;
  inc(realLen);
end;

{$define FindAllExBody :=
  len := Length(Arr);
  rLen := 0;
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i < upper) do begin
    if Arr[i] <> Seq[j] then begin
      inc(i);
      continue;
    end;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then
     begin
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
}

function FindAll(const Arr, Seq:TIntArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TExtArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TFloatArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TDoubleArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TPointArray): TIntArray; overload;
var len,Seqlen,upper,i,j, rlen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TByteArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;


function FindAll(const Arr, Seq:TBoxArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rlen: Int32;
begin FindAllExBody end;

function FindAll(const Arr, Seq:TStringArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin FindAllExBody end;

function FindAll(const Arr, Seq:String): TIntArray; overload;
var len,Seqlen,upper,i,j,rlen: Int32;
begin
  len := Length(Arr);
  rlen := 0;
  Seqlen := Length(Seq);
  upper := len-Seqlen;
  i := 1;  j := 1;
  while (i <= upper) do begin
    if Arr[i] <> Seq[j] then begin
      inc(i);
      continue;
    end;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then
     begin
      TIAAppend(Result,i-Seqlen,rlen);
      j := 1;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;



//========| Simplefied version  |==============================================|
{$define FindAllBody :=
  SetLength(Items,1);
  Items[0] := Item;
  Result := FindAll(Arr,Items);
}

function FindAll(const Arr:TByteArray; Item:Byte): TIntArray; overload;
var Items:TByteArray;
begin FindAllBody end;

function FindAll(const Arr:TIntArray; Item:Int32): TIntArray; overload;
var Items:TIntArray;
begin FindAllBody end;

function FindAll(const Arr:TFloatArray; Item:Single): TIntArray; overload;
var Items:TFloatArray;
begin FindAllBody end;

function FindAll(const Arr:TDoubleArray; Item:Double): TIntArray; overload;
var Items:TDoubleArray;
begin FindAllBody end;

function FindAll(const Arr:TExtArray; Item:Extended): TIntArray; overload;
var Items:TExtArray;
begin FindAllBody end;

function FindAll(const Arr:TPointArray; Item:TPoint): TIntArray; overload;
var Items:TPointArray;
begin FindAllBody end;

function FindAll(const Arr:TBoxArray; Item:TBox): TIntArray; overload;
var Items:TBoxArray;
begin FindAllBody end;

function FindAll(const Arr:TStringArray; Item:String): TIntArray; overload;
var Items:TStringArray;
begin FindAllBody end;

function FindAll(const Arr:String; Item:Char): TIntArray; overload;
var Items:String;
begin
  Items := Item;
  Result := FindAll(Arr,Items);
end;

