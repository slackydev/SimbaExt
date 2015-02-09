{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting array of integers!
*)
procedure __SortTIA(var Arr:TIntArray; Left, Right:Integer);
var
  i,j,l,f,mid: Int32;
  pivot: Int32;
begin
  if (Left + 15 <= Right) then
  begin
    __NumericSortBody

    if (l<>j) then begin
      if (Left < j) then __SortTIA(Arr, Left, j);
      if (i < Right) then __SortTIA(Arr, i, Right);
    end else InsSortTIA(Arr, Left, Right);
  end else InsSortTIA(Arr, Left, Right);
end;


procedure SortTIA(var Arr: TIntArray); //StdCall;
var hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  __SortTIA(Arr, Low(Arr), Hi);
end;
