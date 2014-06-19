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
// Miedian of Five
function MedianOfFive(a,b,c,d,e:uInt8): uInt8;
begin
    ExchMin(a,b);
    ExchMin(c,d);
    if (c < a) then begin Exch(b,d); c := a; end;
    a := e;
    ExchMin(a,b);
    if (a < c) then begin Exch(b,d); a := c; end;
    Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Int8): Int8;
begin
    ExchMin(a,b);
    ExchMin(c,d);
    if (c < a) then begin Exch(b,d); c := a; end;
    a := e;
    ExchMin(a,b);
    if (a < c) then begin Exch(b,d); a := c; end;
    Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Int32): Int32;
begin
    ExchMin(a,b);
    ExchMin(c,d);
    if (c < a) then begin Exch(b,d); c := a; end;
    a := e;
    ExchMin(a,b);
    if (a < c) then begin Exch(b,d); a := c; end;
    Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Single): Single;
begin
    ExchMin(a,b);
    ExchMin(c,d);
    if (c < a) then begin Exch(b,d); c := a; end;
    a := e;
    ExchMin(a,b);
    if (a < c) then begin Exch(b,d); a := c; end;
    Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Double): Double;
begin
    ExchMin(a,b);
    ExchMin(c,d);
    if (c < a) then begin Exch(b,d); c := a; end;
    a := e;
    ExchMin(a,b);
    if (a < c) then begin Exch(b,d); a := c; end;
    Result := Min(d, a);
end;


//------------------------------------------------------------------------------||
//------------------------------------------------------------------------------||
//Insertion sort bellow

(*
 Fast integer sorting from small arrays, or small parts of arrays.
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
 Fast extended sorting from small arrays, or small parts of arrays.
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
 Fast double sorting from small arrays, or small parts of arrays.
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
 Fast single sorting from small arrays, or small parts of arrays.
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
 Fast TPoint sorting from small arrays, or small parts of arrays.
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


//------------------------------------------------------------------------------||
//------------------------------------------------------------------------------||
// ShellSort bellow (only used to ensure O(n^1.5)) in the "main" sorting algorithm.
// Using predifined gaps (Ciura's) only resulted in slowdown.

procedure ShellSortTIA(var Arr: TIntArray);
var
  Gap, i, j, H: Integer;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;
  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        Exch(Arr[j], Arr[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;


procedure ShellSortTEA(var Arr: TExtArray);
var
  Gap, i, j, H: Integer;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;
  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        Exch(Arr[j], Arr[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;


procedure ShellSortTDA(var Arr: TDoubleArray);
var
  Gap, i, j, H: Integer;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;
  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        Exch(Arr[j], Arr[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;


procedure ShellSortTFA(var Arr: TFloatArray);
var
  Gap, i, j, H: Integer;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;
  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        Exch(Arr[j], Arr[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;


procedure ShellSortTPA(var Arr: TPointArray; Weight:TIntArray);
var
  Gap, i, j, H: Integer;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;
  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Weight[j] < Weight[j - Gap]) do
      begin
        Exch(Arr[j], Arr[j - Gap]);
        Exch(Weight[j], Weight[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;
