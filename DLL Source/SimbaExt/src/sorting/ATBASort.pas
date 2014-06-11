{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of TBA using an array for weight!
*)
procedure __SortATBA(var Arr:T2DByteArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,pivot: Integer;
  tmp:CoreTypes.TByteArray;
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
      Exch(Weight[i], Weight[j]);
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortATBA(Arr, Weight, Left,j);
  if (i < Right) then __SortATBA(Arr, Weight, i,Right);
end; 


procedure SortATBAByLength(var Arr:T2DByteArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do Weight[i] := Length(Arr[i]);
  __SortATBA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATBAByMean(var Arr:T2DByteArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := Round(SumTBA(Arr[i]) / Length(Arr[i]));

  __SortATBA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATBAByFirst(var Arr:T2DByteArray);
var
  i,Hi: Integer;
  Weight:TIntArray;
begin
  Hi := Length(Arr);
  if Hi <= 1 then Exit;
  SetLength(Weight, Hi);
  for i := 0 to Hi-1 do 
    Weight[i] := Arr[i][0];
  __SortATBA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;



procedure SortATBAByIndex(var Arr:T2DByteArray; index:Int32);
var
  i,Hi,M: Integer;
  Weight: TIntArray;
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
  __SortATBA(Arr, Weight, Low(Arr), High(Arr));
  SetLength(Weight, 0);
end;
