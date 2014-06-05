{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting Array of Double!
*)
procedure __SortTDA(var Arr:TDoubleArray; Left, Right, depth:Integer);
var
  i,j,l,f,mid: Integer;
  pivot: Double;
begin
  if (Left + 15 <= Right) then
  begin
    Mid := Left + (Right-Left) shr 1;
    TDAMedian3(Arr, Left, Mid, Right);
    TDAMedian3(Arr, (Left+(Mid-Left) shr 1), Mid, (Mid+(Right-Mid) shr 1));
    Pivot := Arr[Mid];
    f := 0;
    i := Left;
    j := Right;
    l := i;
    repeat
      while (Arr[i] < pivot) do Inc(i);
      while (pivot < Arr[j]) do Dec(j);
      if (f <= 5) then
        if (Arr[j] = Arr[i]) then begin
          l := i;
          while (Arr[l] = Arr[j]) and (l<j) do l:=l+1;
          if (l = j) then Break;
        end;
      if (i >= j) then Break;
      ExchD(Arr[i], Arr[j]);
      Inc(f);
      Inc(i);
      Dec(j);
    until False;

    if (l<>j) then begin
      Dec(depth);
      if (depth <= 0) then begin
        ShellSortTDA(Arr);
        Exit;
      end;
      if (Left < j) then __SortTDA(Arr, Left, j, depth);
      if (i < Right) then __SortTDA(Arr, i, Right, depth);
    end else InsSortTDA(Arr, Left, Right);
  end else InsSortTDA(Arr, Left, Right);
end;


procedure SortTDA(var Arr: TDoubleArray); //StdCall;
var limit,hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  __SortTDA(Arr, Low(Arr), Hi, Limit);
end;
