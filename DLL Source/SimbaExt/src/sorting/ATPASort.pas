{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of TPA using an array for weight!
*)
procedure __SortATPA(var Arr:T2DPointArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,pivot: Integer;
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
      Exch(Weight[i], Weight[j]);
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
    M := TPACenter(Arr[i], CA_MEAN, False);
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
