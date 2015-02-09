{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of Single!
*)
procedure __SortTFA(var Arr:TFloatArray; Left, Right:Integer);
var
  i,j,l,f,mid: Int32;
  pivot: Single;
begin
  if (Left + 15 <= Right) then
  begin
    __NumericSortBody

    if (l<>j) then begin
      if (Left < j) then __SortTFA(Arr, Left, j);
      if (i < Right) then __SortTFA(Arr, i, Right);
    end else InsSortTFA(Arr, Left, Right);
  end else InsSortTFA(Arr, Left, Right);
end;


procedure SortTFA(var Arr: TFloatArray); 
var hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  __SortTFA(Arr, Low(Arr), Hi);
end;
