{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


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
      Exch(Arr[i], Arr[j]);
      Exch(Weight[i], Weight[j]);
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
