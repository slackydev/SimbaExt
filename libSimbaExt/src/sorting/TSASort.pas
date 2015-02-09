{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


(*
 Dirty comparison method to compare two strings in a more natural manner.
*)
function CompareNatural(Str1, Str2: String): Int8;
type
  TNaturalString = record
     s: String; i:Int32; IsString:Boolean;
  end;
  TNaturalArray = array of TNaturalString;

  //Splits strings and numbers in to a TNaturalArray, outputing something like: ['asd',123,'cat'].
  function GetNaturalString(Str: String): TNaturalArray;
  var
    i,l,j: Int32;
    IsStr,NextAlso: Boolean;
    tmp: String;
  begin
    tmp := '';
    L := Length(Str);
    j := 0;
    for i:=1 to L do
    begin
      NextAlso := False;
      if (Str[i] in ['0'..'9']) then
      begin
        IsStr := False;
        tmp := tmp + Str[i];
        if i+1 <= L then
          NextAlso := (Str[i+1]  in ['0'..'9']);
      end else
      begin
        IsStr := True;
        tmp := tmp + Str[i];
        if i+1 <= L then
          NextAlso := not(Str[i+1] in ['0'..'9']);
      end;

      if not(NextAlso) then
      begin
        SetLength(Result, j+1);
        Result[j].IsString := IsStr;
        case IsStr of
          True: Result[j].s := tmp;
          False:Result[j].i := StrToInt(tmp);
        end;
        Inc(j);
        tmp := '';
      end;
    end;
  end;

(*
 Using the above splitting we can compare each part induvidually and by their "actual" value
 not just by using ASCII table, but the actual values of the numbers in the string.
 
 It's a bit messy, and not optimized at all...
*)
var
  hi,i: Int32;
  list1, list2: TNaturalArray;
begin
  list1 := GetNaturalString(LowerCase(Str1));
  list2 := GetNaturalString(LowerCase(Str2));
  hi := Min(High(list1), High(list2));
  Result := 0;

  for i:=0 to hi do
  begin
    if not(list1[i].isString) and not(list2[i].IsString) then
    begin
      if list1[i].i < list2[i].i then begin
        Result := -1;
      end else if list1[i].i > list2[i].i then
        Result := 1
      else
        Result := 0;
    end else if (list1[i].IsString) and (list2[i].IsString) then
    begin
      if list1[i].s < list2[i].s then begin
        Result := -1;
      end else if list1[i].s > list2[i].s then
        Result := 1
      else
        Result := 0;
    end else if not(list1[i].IsString) and (list2[i].IsString) then
    begin
      Result := -1;
    end else if (list1[i].IsString) and not(list2[i].IsString) then
      Result := 1
    else
      Result := 0;

    if result <> 0 then
      Exit(result);
  end;
  if result = 0 then
    Result := Sign(length(list1) - length(list2));
end;



(*
 Sorting Array of strings naturally.
*)
procedure __SortTSANatural(var Arr:TStringArray; Left, Right:Integer);
var
  lo,hi: Integer;
  tmp,pivot:String;
begin
  lo:=Left;
  hi:=Right;
  pivot := Arr[(left+right) div 2];
  repeat
    while CompareNatural(Arr[lo], Pivot) < 0 do Inc(lo);
    while CompareNatural(Arr[hi], Pivot) > 0 do Dec(hi);
    if lo<=hi then
    begin
      tmp := Arr[lo];
      Arr[lo] := Arr[hi];
      Arr[hi] := tmp;
      Dec(hi);
      Inc(lo);
    end;
  until lo > hi;
  if (Left < hi) then __SortTSANatural(Arr, Left,hi);
  if (lo < Right) then __SortTSANatural(Arr, lo,Right);
end;


//Sort TSA naturally (alpha-numeric sorting).
procedure SortTSANatural(var Arr: TStringArray);
begin
  if (High(Arr) <= 0) then Exit;
  __SortTSANatural(Arr,Low(Arr),High(Arr));
end;








(*
 Sorting Array of strings lexicographically but comparison is case-insesitive.
*)
procedure __SortTSALexUp(var Arr:TStringArray; Left, Right:Integer);
var
  i,j: Integer;
  tmp,pivot:String;
begin
  i:=Left;
  j:=Right;
  pivot := Uppercase(Arr[(left+right) shr 1]);
  repeat
    while pivot > Uppercase(Arr[i]) do i:=i+1;
    while pivot < Uppercase(Arr[j]) do j:=j-1;
    if i<=j then begin
      tmp:= Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortTSALexUp(Arr, Left,j);
  if (i < Right) then __SortTSALexUp(Arr, i,Right);
end;


(*
 Sorting Array of strings lexicographically.
*)
procedure __SortTSALex(var Arr:TStringArray; Left, Right:Integer);
var
  i,j: Integer;
  tmp,pivot:String;
begin
  i:=Left;
  j:=Right;
  pivot := arr[(left+right) shr 1];
  repeat
    while pivot > Arr[i] do i:=i+1;
    while pivot < Arr[j] do j:=j-1;
    if i<=j then begin
      tmp:= Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortTSALex(Arr, Left,j);
  if (i < Right) then __SortTSALex(Arr, i,Right);
end; 


//Sort TSA lexicographically.
procedure SortTSA(var Arr: TStringArray; CaseInsesitive:Boolean=False);
begin
  if (High(Arr) <= 0) then Exit;
  if CaseInsesitive then
    __SortTSALexUp(Arr,Low(Arr),High(Arr))
  else
    __SortTSALex(Arr,Low(Arr),High(Arr));
end;







(*
 Sorting Array of strings using an array for weight!
*)
procedure __SortTSA(var Arr:TStringArray; Weight:TIntArray; Left, Right:Integer);
var
  i,j,pivot: Integer;
  tmp:String;
begin
  i:=Left;
  j:=Right;
  pivot := Weight[(left+right) shr 1];
  repeat
    while pivot > Weight[i] do i:=i+1;
    while pivot < Weight[j] do j:=j-1;
    if i<=j then begin
      tmp := Arr[i];
      Arr[i] := Arr[j];
      Arr[j] := tmp;
      Exch(Weight[i], Weight[j]);
      j:=j-1;
      i:=i+1;
    end;
  until (i>j);
  if (Left < j) then __SortTSA(Arr, Weight, Left,j);
  if (i < Right) then __SortTSA(Arr, Weight, i,Right);
end; 
