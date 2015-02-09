{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of Extended!
*)
procedure __SortTEA(var Arr:TExtArray; Left, Right:Integer);
var
  i,j,l,f,mid: Int32;
  pivot: Extended;
begin
  if (Left + 15 <= Right) then
  begin
    __NumericSortBody

    if (l<>j) then begin
      if (Left < j) then __SortTEA(Arr, Left, j);
      if (i < Right) then __SortTEA(Arr, i, Right);
    end else InsSortTEA(Arr, Left, Right);
  end else InsSortTEA(Arr, Left, Right);
end;


procedure SortTEA(var Arr: TExtArray); 
var hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  __SortTEA(Arr, Low(Arr), Hi);
end;
