Unit XT_Numeric;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}

interface
uses
  XT_Types, Math, SysUtils;

function SumTIA(const Arr: TIntArray): Integer; Inline; 
function SumTEA(const Arr: TExtArray): Extended; Inline; 
function TIACombinations(const Arr: TIntArray; Seq:Integer): T2DIntArray; 
function TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray; 
procedure MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer); Inline; 
procedure MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended); Inline; 
function TIAMatches(const Arr1, Arr2:TIntArray; InPercent, Inversed:Boolean): Integer; 
function LogscaleTIA(const Freq:TIntArray; Scale: Integer): TIntArray; 

//--------------------------------------------------
implementation


{*
  Sum of a TIA.
*}
function SumTIA(const Arr: TIntArray): Integer; Inline; 
var i:Integer;
begin
  Result := 0;
  for i:=Low(Arr) to High(Arr) do
    Result := Result + Arr[i];
end;


{*
  Sum of a TEA.
*}
function SumTEA(const Arr: TExtArray): Extended; Inline; 
var i:Integer;
begin
  Result := 0.0;
  for i:=Low(Arr) to High(Arr) do
    Result := Result + Arr[i];
end;


{*
  Combinations of size `seq` from given TIA `arr`.
*}
function TIACombinations(const Arr:TIntArray; Seq:Integer): T2DIntArray; 
var
  n,h,i,j: Integer;
  indices: TIntArray;
  breakout: Boolean;
begin
  n := Length(arr);
  if seq > n then Exit;
  SetLength(indices, seq);
  for i:=0 to (seq-1) do indices[i] := i;
  SetLength(Result, 1, Seq);
  for i:=0 to (seq-1) do Result[0][i] := arr[i];
  while True do
  begin
    breakout := True;
    for i:=(Seq-1) downto 0 do
      if (indices[i] <> (i + n - Seq)) then begin
        breakout := False;
        Break;
      end;
    if breakout then Exit;
    Indices[i] := Indices[i]+1;
    for j:=i+1 to Seq-1 do
      Indices[j] := (Indices[j-1] + 1);
    h := Length(Result);
    SetLength(Result, h+1);
    SetLength(Result[h], Seq);
    for i:=0 to Seq-1 do
      Result[h][i] := Arr[Indices[i]];
  end;
  SetLength(Indices, 0);
end;


{*
  Combinations of size `seq` from given TEA `arr`.
*}
function TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray; 
var
  n,h,i,j: Integer;
  indices: TIntArray;
  breakout: Boolean;
begin
  n := Length(arr);
  if seq > n then Exit;
  SetLength(Indices, seq);
  for i:=0 to (seq-1) do Indices[i] := i;
  SetLength(Result, 1, Seq);
  for i:=0 to (seq-1) do Result[0][i] := Arr[i];
  while True do
  begin
    breakout := True;
    for i:=(Seq-1) downto 0 do
      if (Indices[i] <> (i + n - Seq)) then begin
        Breakout := False;
        Break;
      end;
    if Breakout then Exit;
    Indices[i] := Indices[i]+1;
    for j:=i+1 to Seq-1 do
      Indices[j] := (Indices[j-1] + 1);
    h := Length(Result);
    SetLength(Result, h+1);
    SetLength(Result[h], Seq);
    for i:=0 to Seq-1 do
      Result[h][i] := Arr[Indices[i]];
  end;
  SetLength(Indices, 0);
end;

{*
  Finds the minimum and maximum of a TIA.
*}
procedure MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer); Inline; 
var i:Integer;
begin
  Min := Arr[0];
  Max := Arr[0];
  for i:=Low(Arr) to High(Arr) do
  begin
    if Arr[i] < Min then
      Min := Arr[i]
    else if Arr[i] > Max then
      Max := Arr[i];
  end;
end;


{*
  Finds the minimum and maximum of a TEA.
*}
procedure MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended); Inline; 
var i:Integer;
begin
  Min := Arr[0];
  Max := Arr[0];
  for i:=Low(Arr) to High(Arr) do
  begin
    if Arr[i] < Min then
      Min := Arr[i]
    else if Arr[i] > Max then
      Max := Arr[i];
  end;
end;


{*
  Finds the amount of different indices, by comparing each index in "Arr1" to each index in "Arr2".
*}
function TIAMatches(const Arr1, Arr2:TIntArray; InPercent, Inversed:Boolean): Integer; 
var H,i:integer;
begin
  H := Min(High(Arr1), High(Arr2));
  Result := Abs(High(Arr1) - High(Arr2));
  for I:=0 to H do
    if (Arr1[I] <> Arr2[I]) then
      Inc(Result);
      
  if InPercent then begin
    H := Max(High(Arr1), High(Arr2));
    Result := Trunc((Result / H) * 100);
  end;
  
  if Inversed then begin
    case InPercent of 
      True : Result := (100-Result);
      False: Result := (Max(High(Arr1), High(Arr2)) - Result);
    end;
  end;
end;


{*
 Given a TIA with a range of numbers it scales each element in a logarithmic pattern:
 EG:
 > Freq := [0,1,2,3,4]
 > Scale := 2;
 > Result := [0, 1,1, 2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4];
*}
function LogscaleTIA(const Freq:TIntArray; Scale: Integer): TIntArray; 
var
  Size,i,j,H: Integer;
  step,L:Integer;
begin
  H := High(Freq);
  if H = -1 then Exit;
  if (Scale <= 1) or (H = 0) then
  begin
    Result := Copy(Freq);
    Exit;
  end;
  Step := 1;
  SetLength(Result, Step);
  Size := 1;
  L := 0;
  i := 0;
  for i:=0 to H do
  begin 
    for j:=0 to Size-1 do
    begin 
      if L >= Step then begin
        step := step+step;
        SetLength(Result, step);
      end;
      Result[L] := Freq[i];
      Inc(L);
    end;
    Size := Size * Scale; 
  end;
  SetLength(Result, L);
end;


end.






