{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Sorting array of integers!
*)
procedure __SortTIA(var Arr:TIntArray; Left, Right, depth:Integer);
var
  mid,i,j,l,f,pivot:Integer;
begin
  if (Left + 15 <= Right) then
  begin
    Mid := Left + (Right-Left) shr 1;
    TIAMedian3(Arr, Left, Mid, Right);
    TIAMedian3(Arr, (Left+(Mid-Left) shr 1), Mid, (Mid+(Right-Mid) shr 1));
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
      ExchI(Arr[i], Arr[j]);
      Inc(f);
      Inc(i);
      Dec(j);
    until False;

    if (l<>j) then begin
      Dec(depth);
      if (depth <= 0) then begin
        ShellSortTIA(Arr);
        Exit;
      end;
      if (Left < j) then __SortTIA(Arr, Left, j, depth);
      if (i < Right) then __SortTIA(Arr, i, Right, depth);
    end else InsSortTIA(Arr, Left, Right);
  end else InsSortTIA(Arr, Left, Right);
end;

procedure SortTIA(var Arr: TIntArray); //StdCall;
var limit,hi: Integer;
begin
  Hi := High(Arr);
  if Hi < 0 then Exit;
  Limit := Round(2.5 * ln(Hi + 1) / ln(2));
  __SortTIA(Arr, Low(Arr), Hi, Limit);
end;
