{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(*  
  Serches for an item in an array, item can be an array as well.
*)

//========| 1-dimesional  |===================================================|

function Find(Arr:TIntArray; Seq:TIntArray): Int32; overload;
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
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TExtArray; Seq:TExtArray): Int32; overload;
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
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TFloatArray; Seq:TFloatArray): Int32; overload;
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
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TDoubleArray; Seq:TDoubleArray): Int32; overload;
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
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TPointArray; Seq:TPointArray): Int32; overload;
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
    j := 0;
    while (Arr[i].x = Seq[j].x) and (Arr[i].y = Seq[j].y) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TByteArray; Seq:TByteArray): Int32; overload;
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
    j := 0;
    while (Arr[i] = Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:TBoxArray; Seq:TBoxArray): Int32; overload;
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
    j := 0;
    while SameBox(Arr[i], Seq[j]) do begin
     Inc(i);
     Inc(j);
     if j > Seqlen then Exit(i-Seqlen-1);
    end;
  end;
  Result := -1;
end;


function Find(Arr:String; Seq:String): Int32; overload;
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




//========| 2-dimesional  |===================================================|

function Find(Arr:TStringArray; Seq:TStringArray): Int32; overload;
var len,Seqlen,upper,i,j: Int32;
begin
  len := Length(Arr);
  Seqlen := High(Seq);
  upper := len-Seqlen;
  i := 0;  j := 0;
  while (i <= upper) do begin
    if not(Arr[i] = Seq[j]) then begin
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
end;


(*
 Other types not supported.
*)
