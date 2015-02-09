{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting array of int64
*)
procedure __SortTBiA(var Arr:TInt64Array; Left, Right:Integer);
var
  i,j,l,f,mid: Int32;
  pivot: Int64;
begin
  if (Left + 15 <= Right) then
  begin
    __NumericSortBody

    if (l<>j) then begin
      if (Left < j) then __SortTBiA(Arr, Left, j);
      if (i < Right) then __SortTBiA(Arr, i, Right);
    end else InsSortTBiA(Arr, Left, Right);
  end else InsSortTBiA(Arr, Left, Right);
end;


procedure SortTBiA(var Arr: TInt64Array); 
var hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  __SortTBiA(Arr, Low(Arr), Hi);
end;
