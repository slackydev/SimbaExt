unit Sorting;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$inline on}
(*
 Modified version of Quicksort to get better best-case performance.
 > The closer the array is to beeing already sorted, the faster it gets.
 > Does not matter much if the array is reversed or not.
 > Fallback to ShellSort to avoid a bad worst-case scenario.

 How does it work?
 1. Based on a sligtly modified Quicksort.
 2. Recursively partitions the array at the middle (See step 4)...
 3. If the partition Left to Right is less then a cirtain criteria InsertionSort is used.
 
 > If not insertion:
 4. Pivot is selected as a Median of 5: Left - MidLeft - Mid - MidRight - Right.

 5. If all items from "Left up to pivot" and "Right down to pivot" is (close to) sorted
 then we run InsertionSort on the "partition", and exit. If not then we continue
 doing a regular quicksort. This check is then continued ~6 times in each partioning.
 
 6. If the recursion depth goes bellow a given limit then we fall back to ShellSort to avoid
 worst-case scenario in Quicksort: O(n^2).
 
 -------
 My testes show that it can be ~30x faster then QuickSort..
 >> Much more whenever quicksort goes O(n^2) - Rare/Depending on pivot selection.
*)
interface

uses CoreTypes;

procedure InsSortTIA(var Arr:TIntArray; Left, Right:Integer); Inline;
procedure InsSortTEA(var Arr:TExtArray; Left, Right:Integer); Inline;
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Integer); Inline;
procedure ShellSortTIA(var Arr: TIntArray);
procedure ShellSortTEA(var Arr: TExtArray);
procedure ShellSortTPA(var Arr: TPointArray; Weight:TIntArray);

procedure SortTIA(var Arr: TIntArray); //StdCall;
procedure SortTEA(var Arr: TExtArray); //StdCall;

procedure SortTPA(var Arr: TPointArray); //StdCall;
procedure SortTPAFrom(var Arr: TPointArray; const From:TPoint); //StdCall;
procedure SortTPAbyRow(var Arr: TPointArray); //StdCall;
procedure SortTPAbyColumn(var Arr: TPointArray); //StdCall;
procedure SortTPAByY(var Arr: TPointArray); //StdCall;
procedure SortTPAByX(var Arr: TPointArray); //StdCall;

procedure SortTSA(var Arr: TStringArray); 

procedure SortATPAByLength(var Arr:T2DPointArray);
procedure SortATPAByMean(var Arr:T2DPointArray);
procedure SortATPAByFirst(var Arr:T2DPointArray);
procedure SortATPAByIndex(var Arr:T2DPointArray; index:Int32);



//-----------------------------------------------------------------------||
implementation

uses
  PointTools, CoreMisc, Math;


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
       


//------------------------------------------------------------------------------||
//------------------------------------------------------------------------------||
//The main sorting algorithms are bellow.


(*
 Sorting array of integers!
*)
procedure __SortTIA(var Arr:TIntArray; Left, Right, depth:Integer);
var
  mid,i,j,l,f,pivot:Integer;
begin
  if (Left + 15 <= Right) then
  begin
    Mid := Left + (Right-Left) shr 1;
    TIAMedian3(Arr, Left, Mid, Right);
    TIAMedian3(Arr, (Left+(Mid-Left) shr 1), Mid, (Mid+(Right-Mid) shr 1));
    Pivot := Arr[Mid];
    f := 0;
    i := Left;
    j := Right;
    l := i;
    repeat
      while (Arr[i] < pivot) do Inc(i);
      while (pivot < Arr[j]) do Dec(j);
      if (f <= 5) then
        if (Arr[j] = Arr[i]) then begin
          l := i;
          while (Arr[l] = Arr[j]) and (l<j) do l:=l+1;
          if (l = j) then Break;
        end;
      if (i >= j) then Break;
      ExchI(Arr[i], Arr[j]);
      Inc(f);
      Inc(i);
      Dec(j);
    until False;

    if (l<>j) then begin
      Dec(depth);
      if (depth <= 0) then begin
        ShellSortTIA(Arr);
        Exit;
      end;
      if (Left < j) then __SortTIA(Arr, Left, j, depth);
      if (i < Right) then __SortTIA(Arr, i, Right, depth);
    end else InsSortTIA(Arr, Left, Right);
  end else InsSortTIA(Arr, Left, Right);
end;

procedure SortTIA(var Arr: TIntArray); //StdCall;
var limit,hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  __SortTIA(Arr, Low(Arr), Hi, Limit);
end;






(*
 Sorting Array of Extended!
*)
procedure __SortTEA(var Arr:TExtArray; Left, Right, depth:Integer);
var
  i,j,l,f,mid: Integer;
  pivot: Extended;
begin
  if (Left + 15 <= Right) then
  begin
    Mid := Left + (Right-Left) shr 1;
    TEAMedian3(Arr, Left, Mid, Right);
    TEAMedian3(Arr, (Left+(Mid-Left) shr 1), Mid, (Mid+(Right-Mid) shr 1));
    Pivot := Arr[Mid];
    f := 0;
    i := Left;
    j := Right;
    l := i;
    repeat
      while (Arr[i] < pivot) do Inc(i);
      while (pivot < Arr[j]) do Dec(j);
      if (f <= 5) then
        if (Arr[j] = Arr[i]) then begin
          l := i;
          while (Arr[l] = Arr[j]) and (l<j) do l:=l+1;
          if (l = j) then Break;
        end;
      if (i >= j) then Break;
      ExchE(Arr[i], Arr[j]);
      Inc(f);
      Inc(i);
      Dec(j);
    until False;

    if (l<>j) then begin
      Dec(depth);
      if (depth <= 0) then begin
        ShellSortTEA(Arr);
        Exit;
      end;
      if (Left < j) then __SortTEA(Arr, Left, j, depth);
      if (i < Right) then __SortTEA(Arr, i, Right, depth);
    end else InsSortTEA(Arr, Left, Right);
  end else InsSortTEA(Arr, Left, Right);
end;


procedure SortTEA(var Arr: TExtArray); //StdCall;
var limit,hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  __SortTEA(Arr, Low(Arr), Hi, Limit);
end;






(*
 Sorting Array of TPoint using an array for weight!
*)
procedure __SortTPA(var Arr:TPointArray; var Weight:TIntArray; Left, Right, Depth:Integer);
var
  i,j,l,f,mid: Integer;
  pivot: Extended;
begin
  if (Left + 15 <= Right) then
  begin
    Mid := Left + (Right-Left) shr 1;
    TPAMedian3(Arr, Weight, Left, Mid, Right);
    TPAMedian3(Arr, Weight, (Left+(Mid-Left) shr 1), Mid, (Mid+(Right-Mid) shr 1));
    Pivot := Weight[Mid];
    f := 0;
    i := Left;
    j := Right;
    l := i;
    repeat
      while (Weight[i] < pivot) do Inc(i);
      while (pivot < Weight[j]) do Dec(j);
      if (f <= 5) then
        if (Weight[j] = Weight[i]) then begin
          l := i;
          while (Weight[l] = Weight[j]) and (l<j) do l:=l+1;
          if (l = j) then Break;
        end;
      if (i >= j) then Break;
      ExchPt(Arr[i], Arr[j]);
      ExchI(Weight[i], Weight[j]);
      Inc(f);
      Inc(i);
      Dec(j);
    until False;

    if (l<>j) then begin
      Dec(depth);
      if (depth <= 0) then begin
        ShellSortTPA(Arr, Weight);
        Exit;
      end;
      if (Left < j) then __SortTPA(Arr, Weight, Left, j, depth);
      if (i < Right) then __SortTPA(Arr, Weight, i, Right, depth);
    end else InsSortTPA(Arr, Weight, Left, Right);
  end else InsSortTPA(Arr, Weight, Left, Right);
end;



//Sort TPA by Distance from 0,0;
procedure SortTPA(var Arr: TPointArray); //StdCall;
var
  i,Hi: Integer;
  Weight:TIntArray;
  limit: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi+1) / ln(2));
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Sqr(Arr[i].x) + Sqr(Arr[i].y);
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;


//Sort TPA by Distance from a TPoint `From`.
procedure SortTPAFrom(var Arr: TPointArray; const From:TPoint); //StdCall;
var
  i,Hi: Integer;
  Weight:TIntArray;
  limit: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Sqr(From.x-Arr[i].x) + Sqr(From.y-Arr[i].y);
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;


//Sort TPA by Row.
procedure SortTPAbyRow(var Arr: TPointArray); //StdCall;
var
  i,Hi,W: Integer;
  Weight:TIntArray;
  Area : TBox;
  limit: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5*ln(Hi + 1) / ln(2));
  Area := TPABounds(Arr);
  W := Area.X2-Area.X1+1;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Arr[i].y * W + Arr[i].x;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;


//Sort TPA by Column.
procedure SortTPAbyColumn(var Arr: TPointArray); //StdCall;
var
  i,Hi,H: Integer;
  Weight:TIntArray;
  Area : TBox;
  limit: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  Area := TPABounds(Arr);
  H := Area.Y2-Area.Y1+1;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Arr[i].x * H + Arr[i].y;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;


//Sort TPA by Y
procedure SortTPAByY(var Arr: TPointArray); //StdCall;
var
  i,Hi: Integer;
  Weight:TIntArray;
  limit: Integer;
begin
  Hi := Length(Arr);
  if Hi <= 0 then Exit;
  Limit := Round(2.5 * ln(Hi) / ln(2));
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Arr[i].y;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;



//Sort TPA by X
procedure SortTPAByX(var Arr: TPointArray); //StdCall;
var
  i,Hi: Integer;
  Weight:TIntArray;
  limit: Integer;
begin
  Hi := Length(Arr);
  if Hi <= 0 then Exit;
  Limit := Round(2.5 * ln(Hi) / ln(2));
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Arr[i].x;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr), Limit);
  SetLength(Weight, 0);
end;







(*
 Sorting Array of strings lexicographically.
*)
procedure __SortTSALex(var Arr:TStringArray; Left, Right:Integer);
var
  i,j,n,key: Integer;
  tmp,pivot:String;
begin
  i:=Left;
  j:=Right;
  pivot := arr[(left+right) shr 1];
  repeat
    while pivot > Arr[i] do i:=i+1;
    while pivot < Arr[j] do j:=j-1;
    if i<=j then begin
      tmp:= Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortTSALex(Arr, Left,j);
  if (i < Right) then __SortTSALex(Arr, i,Right);
end; 


//Sort TSA lexicographically.
procedure SortTSA(var Arr: TStringArray);
begin
  if (High(Arr) <= 0) then Exit;
  __SortTSALex(Arr,Low(Arr),High(Arr));
end;






(*
 Sorting Array of strings using an array for weight!
*)
procedure __SortTSA(var Arr:TStringArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,n,key,pivot: Integer;
  tmp:String;
begin
  i:=Left;
  j:=Right;
  pivot := Weight[(left+right) shr 1];
  repeat
    while pivot > Weight[i] do i:=i+1;
    while pivot < Weight[j] do j:=j-1;
    if i<=j then begin
      tmp := Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      ExchI(Weight[i], Weight[j]);
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortTSA(Arr, Weight, Left,j);
  if (i < Right) then __SortTSA(Arr, Weight, i,Right);
end; 
















(*
 Sorting Array of TPA using an array for weight!
*)
procedure __SortATPA(var Arr:T2DPointArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,n,pivot: Integer;
  tmp:TPointArray;
begin
  i:=Left;
  j:=Right;
  pivot := Weight[(left+right) shr 1];
  repeat
    while pivot > Weight[i] do i:=i+1;
    while pivot < Weight[j] do j:=j-1;
    if i<=j then begin
      tmp:= Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      ExchI(Weight[i], Weight[j]);
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortATPA(Arr, Weight, Left,j);
  if (i < Right) then __SortATPA(Arr, Weight, i,Right);
end; 


procedure SortATPAByLength(var Arr:T2DPointArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Length(Arr[i]);
  __SortATPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATPAByMean(var Arr:T2DPointArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
  M:TPoint;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
  begin
    M := TPACenter(Arr[i], CM_Mean, False);
    Weight[i] := Sqr(M.x) + Sqr(M.y);
  end;
  __SortATPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATPAByFirst(var Arr:T2DPointArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Sqr(Arr[i][0].x) + Sqr(Arr[i][0].y);
  __SortATPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATPAByIndex(var Arr:T2DPointArray; index:Int32);
var
  i,Hi,M: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
  begin
    if index <= -1 then
      M := Max(0,High(Arr[i]) + index)
    else
      M := Min(High(Arr[i]), index);
    
    Weight[i] := Sqr(Arr[i][M].x) + Sqr(Arr[i][M].y);
  end;
  __SortATPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;

end.
