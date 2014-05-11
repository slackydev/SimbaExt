{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of TEA using an array for weight!
*)
procedure __SortATEA(var Arr:T2DExtArray; Weight:TExtArray; Left, Right:Integer);
var
  i,j: Integer;
  pivot:Extended;
  tmp:TExtArray;
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
      ExchE(Weight[i], Weight[j]);
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortATEA(Arr, Weight, Left,j);
  if (i < Right) then __SortATEA(Arr, Weight, i,Right);
end; 


procedure SortATEAByLength(var Arr:T2DExtArray);
var
  i,Hi: Integer;
  Weight:TExtArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Length(Arr[i]);
  __SortATEA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATEAByMean(var Arr:T2DExtArray);
var
  i,Hi: Integer;
  Weight:TExtArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := SumTEA(Arr[i]) / Length(Arr[i]);

  __SortATEA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATEAByFirst(var Arr:T2DExtArray);
var
  i,Hi: Integer;
  Weight:TExtArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := Arr[i][0];
    
  __SortATEA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATEAByIndex(var Arr:T2DExtArray; index:Int32);
var
  i,Hi,M: Integer;
  Weight:TExtArray;
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
  __SortATEA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;
