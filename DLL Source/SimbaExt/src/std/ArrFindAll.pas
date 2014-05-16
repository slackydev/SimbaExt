{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*  
  Serches for an item in an array, item can be an array as well.
*)

procedure TIAAppend(var TIA:TIntArray; Val:Int32); Inline;
var L:Int32;
begin
  L := Length(TIA);
  SetLength(TIA, L+1);
  TIA[L] := Val;
end;

//========| 1-dimesional  |===================================================|

function FindAll(Arr:TIntArray; Seq:TIntArray): TIntArray; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
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
      TIAAppend(Result,i-Seqlen-1);
      j := 0;
      Break;
     end;
    end;
  end;
end;


function FindAll(Arr:TExtArray; Seq:TExtArray): TIntArray; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
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
      TIAAppend(Result,i-Seqlen-1);
      j := 0;
      Break;
     end;
    end;
  end;
end;


function FindAll(Arr:TPointArray; Seq:TPointArray): TIntArray; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i <= upper) do begin
    if (Arr[i].x <> Seq[j].x) or (Arr[i].y <> Seq[j].y) then begin
      inc(i);
      continue;
    end;
    while (Arr[i].x = Seq[j].x) and (Arr[i].y = Seq[j].y) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then 
     begin
      TIAAppend(Result,i-Seqlen-1);
      j := 0;
      Break;
     end;
    end;
  end;
end;


function FindAll(Arr:TByteArray; Seq:TByteArray): TIntArray; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i <= upper) do begin
    if (Arr[i] <> Seq[j]) then begin
      inc(i);
      continue;
    end;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then 
     begin
      TIAAppend(Result,i-Seqlen-1);
      j := 0;
      Break;
     end;
    end;
  end;
end;


function FindAll(Arr:TBoxArray; Seq:TBoxArray): TIntArray; overload;
var 
  len,Seqlen,upper,i,j: Int32;
  function SameBox(B1,B2:TBox):Boolean; Inline;
  begin
    Result := (B1.x1=B2.x1) and (B1.y1=B2.y1) and (B1.x2=B2.x2) and (B1.y2=B2.y2);
  end;

begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i <= upper) do begin
    if not SameBox(Arr[i], Seq[j]) then begin
      inc(i);
      continue;
    end;
    while SameBox(Arr[i], Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then 
     begin
      TIAAppend(Result,i-Seqlen-1);
      j := 0;
      Break;
     end;
    end;
  end;
end;


function FindAll(Arr:String; Seq:String): TIntArray; overload;
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
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then 
     begin
      TIAAppend(Result,i-Seqlen);
      j := 1;
      Break;
     end;
    end;
  end;
end;




//========| 2-dimesional  |===================================================|
(*
 Not supported
*)
