{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure FastSortTBA(var Arr:CoreTypes.TByteArray);
var
  i,j,c:Int32;
  Lookup:TIntArray;
begin
  SetLength(lookup,256);
  for i:=0 to High(Arr) do
    Inc(Lookup[Arr[i]]);
  c := 0;
  for i:=0 to 255 do
    for j:=0 to Lookup[i]-1 do
    begin
      Arr[c] := i;
      inc(c);
    end;
end;


procedure SortTBA(var Arr: CoreTypes.TByteArray);
begin
  FastSortTBA(Arr);
end;
