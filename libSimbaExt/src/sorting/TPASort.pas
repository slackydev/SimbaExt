{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

(*
 Sorting Array of TPoint using an array for weight!
*)
procedure __SortTPA(var Arr:TPointArray; var Weight:TIntArray; Left, Right:Int32);
var
  i,j,l,f,mid: Int32;
  pivot: Int32;
begin
  if (Left + 15 <= Right) then
  begin
    mid := Left + (Right-Left) shr 1;
    median3(Arr,weight,left,mid,right);
    Pivot := Weight[mid];
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
      if (Left < j) then __SortTPA(Arr, Weight, Left, j);
      if (i < Right) then __SortTPA(Arr, Weight, i, Right);
    end else InsSortTPA(Arr, Weight, Left, Right);
  end else InsSortTPA(Arr, Weight, Left, Right);
end;



//Sort TPA by Distance from 0,0;
procedure SortTPA(var Arr: TPointArray); 
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Sqr(Arr[i].x) + Sqr(Arr[i].y);
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;


//Sort TPA by Distance from a TPoint `From`.
procedure SortTPAFrom(var Arr: TPointArray; const From:TPoint); 
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Sqr(From.x-Arr[i].x) + Sqr(From.y-Arr[i].y);
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;


//Sort TPA by Row.
procedure SortTPAbyRow(var Arr: TPointArray);
var
  i,Hi,W: Integer;
  Weight:TIntArray;
  Area : TBox;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Area := TPABounds(Arr);
  W := Area.X2-Area.X1+1;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Arr[i].y * W + Arr[i].x;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;


//Sort TPA by Column.
procedure SortTPAbyColumn(var Arr: TPointArray);
var
  i,Hi,H: Integer;
  Weight:TIntArray;
  Area : TBox;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Area := TPABounds(Arr);
  H := Area.Y2-Area.Y1+1;
  SetLength(Weight, Hi+1);
  for i := 0 to Hi do
    Weight[i] := Arr[i].x * H + Arr[i].y;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;


//Sort TPA by Y
procedure SortTPAByY(var Arr: TPointArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 0 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Arr[i].y;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



//Sort TPA by X
procedure SortTPAByX(var Arr: TPointArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 0 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Arr[i].x;
  __SortTPA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;
