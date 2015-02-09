{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of Double!
*)
procedure __SortTDA(var Arr:TDoubleArray; Left, Right:Integer);
var
  i,j,l,f,mid: Int32;
  pivot: Double;
begin
  if (Left + 15 <= Right) then
  begin
    __NumericSortBody

    if (l<>j) then begin
      if (Left < j) then __SortTDA(Arr, Left, j);
      if (i < Right) then __SortTDA(Arr, i, Right);
    end else InsSortTDA(Arr, Left, Right);
  end else InsSortTDA(Arr, Left, Right);
end;


procedure SortTDA(var Arr: TDoubleArray); 
var hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  __SortTDA(Arr, Low(Arr), Hi);
end;
