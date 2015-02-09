{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

//------------------------------------------------------------------------------||
//------------------------------------------------------------------------------||
// Miedian of three

//Median of three - Integer.
procedure Median3(var Arr:TIntArray; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Arr[Middle] < Arr[Left])  then Exch(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then Exch(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then Exch(Arr[Middle], Arr[Right]);
end;

//Median of three - BigInteger
procedure Median3(var Arr:TInt64Array; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Arr[Middle] < Arr[Left])  then Exch(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then Exch(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then Exch(Arr[Middle], Arr[Right]);
end;

//Median of three - Extended.
procedure Median3(var Arr:TExtArray; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Arr[Middle] < Arr[Left])  then Exch(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then Exch(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then Exch(Arr[Middle], Arr[Right]);
end;

//Median of three - Double.
procedure Median3(var Arr:TDoubleArray; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Arr[Middle] < Arr[Left])  then Exch(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then Exch(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then Exch(Arr[Middle], Arr[Right]);
end;

//Median of three - Single.
procedure Median3(var Arr:TFloatArray; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Arr[Middle] < Arr[Left])  then Exch(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then Exch(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then Exch(Arr[Middle], Arr[Right]);
end;

//Median of three - TPoint with weight.
procedure Median3(var Arr:TPointArray; var Weight:TIntArray; Left, Middle, Right:Integer); Inline; overload;
begin
  if (Weight[Middle] < Weight[Left]) then begin
    Exch(Arr[Left], Arr[Middle]);
    Exch(Weight[Left], Weight[Middle]);
  end;
  if (Weight[Right] < Weight[Left]) then begin
    Exch(Arr[Left], Arr[Right]);
    Exch(Weight[Left], Weight[Right]);
  end;
  if (Weight[Right] < Weight[Middle]) then begin
    Exch(Arr[Middle], Arr[Right]);
    Exch(Weight[Middle], Weight[Right]);
  end;
end;


//------------------------------------------------------------------------------||
//------------------------------------------------------------------------------||
//Insertion sort

(*
 Fast integer sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTIA(var Arr:TIntArray; Left, Right:Integer); Inline;
var i, j, tmp:Integer;
begin
  for i := Left+1 to Right do begin
    j := i-1;
    Tmp := arr[i];
    while (j >= Left) and (Arr[j] > Tmp) do begin
      Arr[j+1] := Arr[j];
      j:=j-1;
    end;
    Arr[j+1] := Tmp;
  end;
end;


(*
 Fast integer64 sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTBiA(var Arr:TInt64Array; Left, Right:Integer); Inline;
var i, j, tmp:Integer;
begin
  for i := Left+1 to Right do begin
    j := i-1;
    Tmp := arr[i];
    while (j >= Left) and (Arr[j] > Tmp) do begin
      Arr[j+1] := Arr[j];
      j:=j-1;
    end;
    Arr[j+1] := Tmp;
  end;
end;


(*
 Fast extended sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTEA(var Arr:TExtArray; Left, Right:Integer); Inline;
var i, j:Integer; tmp:Extended;
begin
  for i := Left+1 to Right do begin
    j := i-1;
    Tmp := arr[i];
    while (j >= Left) and (Arr[j] > Tmp) do begin
      Arr[j+1] := Arr[j];
      j:=j-1;
    end;
    Arr[j+1] := Tmp;
  end;
end;


(*
 Fast double sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTDA(var Arr:TDoubleArray; Left, Right:Integer); Inline;
var i, j:Integer; tmp:Double;
begin
  for i := Left+1 to Right do begin
    j := i-1;
    Tmp := arr[i];
    while (j >= Left) and (Arr[j] > Tmp) do begin
      Arr[j+1] := Arr[j];
      j:=j-1;
    end;
    Arr[j+1] := Tmp;
  end;
end;


(*
 Fast single sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTFA(var Arr:TFloatArray; Left, Right:Integer); Inline;
var i, j:Integer; tmp:Single;
begin
  for i := Left+1 to Right do begin
    j := i-1;
    Tmp := arr[i];
    while (j >= Left) and (Arr[j] > Tmp) do begin
      Arr[j+1] := Arr[j];
      j:=j-1;
    end;
    Arr[j+1] := Tmp;
  end;
end;


(*
 Fast TPoint sorting for small arrays, or small parts of arrays.
*)
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Integer); Inline;
var i, j:Integer;
begin
  for i := Left to Right do
    for j := i downto Left + 1 do begin
      if not (Weight[j] < Weight[j - 1]) then Break;
      Exch(Arr[j-1], Arr[j]);
      Exch(Weight[j-1], Weight[j]);
    end;
end;

