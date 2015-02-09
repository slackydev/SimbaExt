{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

(*
 Sorting Array of TIA using an array for weight!
*)
procedure __SortATIA(var Arr:T2DIntArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,pivot: Integer;
  tmp:TIntArray;
begin
  __WeightedSortBody
  if (Left < j) then __SortATIA(Arr, Weight, Left,j);
  if (i < Right) then __SortATIA(Arr, Weight, i,Right);
end; 


procedure SortATIAByLength(var Arr:T2DIntArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Length(Arr[i]);
  __SortATIA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATIAByMean(var Arr:T2DIntArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := Round(MeanPtr(Pointer(Arr[i]),4,True));

  __SortATIA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATIAByFirst(var Arr:T2DIntArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := Arr[i][0];
  __SortATIA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATIAByIndex(var Arr:T2DIntArray; index:Int32);
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
    Weight[i] := Arr[i][M];
  end;
  __SortATIA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;
