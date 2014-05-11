{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

//Median of three - Integer.
procedure TIAMedian3(var Arr:TIntArray; Left, Middle, Right:Integer); Inline;
begin
  if (Arr[Middle] < Arr[Left])  then ExchI(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then ExchI(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then ExchI(Arr[Middle], Arr[Right]);
end;

//Median of three - Extended.
procedure TEAMedian3(var Arr:TExtArray; Left, Middle, Right:Integer); Inline;
begin
  if (Arr[Middle] < Arr[Left])  then ExchE(Arr[Left], Arr[Middle]);
  if (Arr[Right] < Arr[Left])   then ExchE(Arr[Left], Arr[Right]);
  if (Arr[Right] < Arr[Middle]) then ExchE(Arr[Middle], Arr[Right]);
end;


//Median of three - TPoint with weight.
procedure TPAMedian3(var Arr:TPointArray; var Weight:TIntArray; Left, Middle, Right:Integer); Inline;
begin
  if (Weight[Middle] < Weight[Left]) then begin
    ExchPt(Arr[Left], Arr[Middle]);
    ExchI(Weight[Left], Weight[Middle]);
  end;
  if (Weight[Right] < Weight[Left]) then begin
    ExchPt(Arr[Left], Arr[Right]);
    ExchI(Weight[Left], Weight[Right]);
  end;
  if (Weight[Right] < Weight[Middle]) then begin
    ExchPt(Arr[Middle], Arr[Right]);
    ExchI(Weight[Middle], Weight[Right]);
  end;
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
 Fast TPoint sorting from small arrays, or small parts of arrays.
*)
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Integer); Inline;
var i, j:Integer;
begin
  for i := Left to Right do
    for j := i downto Left + 1 do begin
      if not (Weight[j] < Weight[j - 1]) then Break;
      ExchPt(Arr[j-1], Arr[j]);
      ExchI(Weight[j-1], Weight[j]);
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
        ExchI(Arr[j], Arr[j - Gap]);
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
        ExchE(Arr[j], Arr[j - Gap]);
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
        ExchPt(Arr[j], Arr[j - Gap]);
        ExchI(Weight[j], Weight[j - Gap]);
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;
