{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*  
  Serches for an item in an array, item can be an array as well.
*)

//========| 1-dimesional  |===================================================|
{$define FindExBody :=
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i <= upper) do begin
    if (Arr[i] <> Seq[j]) then begin
      inc(i);
      continue;
    end;
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
}


function Find(const Arr, Seq:TByteArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TIntArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TFloatArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TDoubleArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TExtArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TPointArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TBoxArray): Int32; overload;
var  len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:TStringArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin FindExBody end;


function Find(const Arr, Seq:String): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := Length(Seq);
  upper := len-Seqlen+1;
  i := 1;  j := 1;
  while (i <= upper) do begin
    if Arr[i] <> Seq[j] then begin
      inc(i);
      continue;
    end;
    j := 1;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen);
    end;
  end;
  Result := 0;
end;


//========| Simplefied version  |==============================================|
{$define FindBody :=
  SetLength(Items,1);
  Items[0] := Item;
  Result := Find(Arr,Items);
}

function Find(const Arr:TByteArray; Item:Byte): Int32; overload;
var Items:TByteArray;
begin FindBody end;

function Find(const Arr:TIntArray; Item:Int32): Int32; overload;
var Items:TIntArray;
begin FindBody end;

function Find(const Arr:TFloatArray; Item:Single): Int32; overload;
var Items:TFloatArray;
begin FindBody end;

function Find(const Arr:TDoubleArray; Item:Double): Int32; overload;
var Items:TDoubleArray;
begin FindBody end;

function Find(const Arr:TExtArray; Item:Extended): Int32; overload;
var Items:TExtArray;
begin FindBody end;

function Find(const Arr:TPointArray; Item:TPoint): Int32; overload;
var Items:TPointArray;
begin FindBody end;

function Find(const Arr:TBoxArray; Item:TBox): Int32; overload;
var Items:TBoxArray;
begin FindBody end;

function Find(const Arr:TStringArray; Item:String): Int32; overload;
var Items:TStringArray;
begin FindBody end;

function Find(const Arr:String; Item:Char): Int32; overload;
var Items:String;
begin
  Items := Item;
  Result := Find(Arr,Items);
end;

