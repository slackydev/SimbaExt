{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*  
  Serches for an item in an array, item can be an array as well.
*)


procedure TIAAppend(var TIA:TIntArray; Val:Int32; var realLen:Int32);
var L:Int32;
begin
  L := Length(TIA);
  if realLen >= L then SetLength(TIA, L+L+1);
  TIA[realLen] := Val;
  inc(realLen);
end;

//========| 1-dimesional  |===================================================|

function FindAll(Arr:TIntArray; Seq:TIntArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TExtArray; Seq:TExtArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TFloatArray; Seq:TFloatArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TDoubleArray; Seq:TDoubleArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TPointArray; Seq:TPointArray): TIntArray; overload;
var len,Seqlen,upper,i,j, rlen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rlen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TByteArray; Seq:TByteArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:TBoxArray; Seq:TBoxArray): TIntArray; overload;
var 
  len,Seqlen,upper,i,j,rlen: Int32;
  function SameBox(B1,B2:TBox):Boolean; Inline;
  begin Result := (B1.x1=B2.x1) and (B1.y1=B2.y1) and (B1.x2=B2.x2) and (B1.y2=B2.y2); end;
begin
  len := Length(Arr);
  rlen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rlen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


function FindAll(Arr:String; Seq:String): TIntArray; overload;
var len,Seqlen,upper,i,j,rlen: Int32;
begin
  len := Length(Arr);
  rlen := 0;
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
      TIAAppend(Result,i-Seqlen,rlen);
      j := 1;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;




//========| 2-dimesional  |===================================================|

function FindAll(Arr:TStringArray; Seq:TStringArray): TIntArray; overload;
var len,Seqlen,upper,i,j,rLen: Int32;
begin
  len := Length(Arr);
  rLen := 0;
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
      TIAAppend(Result,i-Seqlen-1,rLen);
      j := 0;
      Break;
     end;
    end;
  end;
  SetLength(Result, rLen);
end;


(*
 Not supported
*)
