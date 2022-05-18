unit Statistics;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Most methods here are pointer-based, so that one can pass it an array
 of any numeric basetype.
 
 Methods for:
 > Sum, Min, Max, Mean, Varaince (sample), Standard deviation (sample), Mode
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}

interface
uses
  CoreTypes, Math, SysUtils;

  
function SumFPtr(Ptr:Pointer; Size:UInt8): Extended;
function SumPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Int64;

procedure MinMaxFPtr(Ptr:Pointer; Size:UInt8; var Min,Max:Extended);
procedure MinMaxPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean; var Min,Max:Int64);

function MeanFPtr(Ptr:Pointer; Size:UInt8): Extended;
function MeanPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;

function VarianceFPtr(Ptr:Pointer; Size:UInt8): Extended;
function VariancePtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;

function StdevFPtr(Ptr:Pointer; Size:UInt8): Extended;
function StdevPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;

function ModeFPtr(Ptr:Pointer; Size:UInt8): Extended;
function ModePtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Int64;

function MedianFPtr(Ptr:Pointer; Size:UInt8): Extended;
function MedianPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;

//old
function TIACombinations(const Arr: TIntArray; Seq:Integer): T2DIntArray; 
function TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray; 
function TIAMatches(const Arr1, Arr2:TIntArray; InPercent, Inversed:Boolean): Int32;


//--------------------------------------------------
implementation
uses
  CoreMisc, Sorting;


// Returns the sum of any float-type array.
function SumFPtr(Ptr:Pointer; Size:UInt8): Extended;
var l:PtrUInt; len:SizeInt;
begin
  if not(Size in [4,8,10]) then Exit(0);
  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  l := PtrUInt(ptr)+(len*size);

  Result := 0;
  while (PtrUInt(ptr) < l) do begin
    case size of
      4 : Result += PFloat32(Ptr)^;
      8 : Result += PFloat64(Ptr)^;
      10: Result += PFloat80(Ptr)^;
    end;
    Inc(ptr,size);
  end;
end;


// Returns the sum of any int-type array.
function SumPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Int64;
var
  l:PtrUInt; len:SizeInt;
begin
  if not(Size in [1,2,4,8]) then Exit(0);
  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  l := PtrUInt(ptr)+(len*size);

  Result := 0;
  case Signed of
  False:
    while (PtrUInt(ptr) < l) do begin
      case size of
        1: Result += PUInt8(Ptr)^;
        2: Result += PUInt16(Ptr)^;
        4: Result += PUInt32(Ptr)^;
        8: Result += PUInt64(Ptr)^;
      end;
      Inc(ptr,size);
    end;
  True:
    while (PtrUInt(ptr) < l) do begin
      case size of
        1: Result += PInt8(Ptr)^;
        2: Result += PInt16(Ptr)^;
        4: Result += PInt32(Ptr)^;
        8: Result += PInt64(Ptr)^;
      end;
      Inc(ptr,size);
    end;
  end;
end;


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Finds the minimum and maximum of any float-type array.
procedure MinMaxFPtr(Ptr:Pointer; Size:UInt8; var Min,Max:Extended);
var l:PtrUInt; len:SizeInt;
begin
  if not(Size in [4,8,10]) then Exit();
  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  l := PtrUInt(ptr)+(len*size);

  case size of
    4 :begin Min := PFloat32(Ptr)^; Max := Min; end;
    8 :begin Min := PFloat64(Ptr)^; Max := Min; end;
    10:begin Min := PFloat80(Ptr)^; Max := Min; end;
  end;
  while (PtrUInt(ptr) < l) do begin
    case size of
      4 : if PFloat32(Ptr)^ < Min then        Min := PFloat32(Ptr)^
          else if PFloat32(Ptr)^ > Max then   Max := PFloat32(Ptr)^;
      8 : if PFloat64(Ptr)^ < Min then        Min := PFloat64(Ptr)^
          else if PFloat64(Ptr)^ > Max then   Max := PFloat64(Ptr)^;
      10: if PFloat80(Ptr)^ < Min then        Min := PFloat80(Ptr)^
          else if PFloat80(Ptr)^ > Max then   Max := PFloat80(Ptr)^;
    end;
    Inc(ptr,size);
  end;
end;


// Finds the minimum and maximum of any int-type array.
procedure MinMaxPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean; var Min,Max:Int64);
var l:PtrUInt; tmp,len:Int64;
begin
  if not(Size in [1,2,4,8]) then Exit();
  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  l := PtrUInt(ptr)+(len*size);

  Max := Low(Int64);
  Min := High(Int64); 

  case signed of
  True:
    while (PtrUInt(ptr) < l) do begin
      case size of
        1: tmp := PUInt8(Ptr)^;
        2: tmp := PUInt16(Ptr)^;
        4: tmp := PUInt32(Ptr)^;
        8: tmp := PUInt64(Ptr)^;
      end;
      if (tmp < Min) then Min := tmp;
      if (tmp > Max) then Max := tmp;
      Inc(ptr,size);
    end;
  False:
    while (PtrUInt(ptr) < l) do begin
      case size of
        1: tmp := PUInt8(Ptr)^;
        2: tmp := PUInt16(Ptr)^;
        4: tmp := PUInt32(Ptr)^;
        8: tmp := PUInt64(Ptr)^;
      end;
      if (tmp < Min) then Min := tmp;
      if (tmp > Max) then Max := tmp;
      Inc(ptr,size);
    end;
  end;
end;


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Returns the mean of any float-type array
function MeanFPtr(Ptr:Pointer; Size:UInt8): Extended;
var len:SizeInt;
begin
   if not(Size in [4,8,10]) then Exit(0);

  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  Result := SumFPtr(Ptr, Size) / Len;
end;


// Returns the mean of any int-type array
function MeanPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;
var len:SizeInt;
begin
  if not(Size in [1,2,4,8]) then Exit(0);

  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  Result := SumPtr(Ptr, Size, Signed) / Len;
end;



{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Returns the variance of any float-type array
function VarianceFPtr(Ptr:Pointer; Size:UInt8): Extended;
var
  len:SizeInt;
  upper: PtrUInt;
  avg, square:Extended;
begin
  if not(Size in [4,8,10]) then Exit(0);
  len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  upper := PtrUInt(ptr)+(len*size);

  Avg := SumFPtr(Ptr, Size) / Len;
  Square := 0;
  while (PtrUInt(ptr) < upper) do begin
    case size of
      4 : Square += Sqr(PFloat32(Ptr)^ - avg);
      8 : Square += Sqr(PFloat64(Ptr)^ - avg);
      10: Square += Sqr(PFloat80(Ptr)^ - avg);
    end;
    Inc(ptr,size);
  end;
  Result := (Square / Len);
end;


// Returns the variance of any int-type array
function VariancePtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;
var
  len:SizeInt;
  upper: PtrUInt;
  avg, square:Extended;
begin
  if not(Size in [1,2,4,8]) then Exit(0);
  Len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  upper := PtrUInt(ptr)+(len*size);

  Avg := SumPtr(Ptr, Size, Signed) / Len;
  Square := 0;
  case Signed of
  False:
    while (PtrUInt(ptr) < upper) do begin
      case size of
        1: Square += Sqr(PUInt8(Ptr)^ - avg);
        2: Square += Sqr(PUInt16(Ptr)^ - avg);
        4: Square += Sqr(PUInt32(Ptr)^ - avg);
        8: Square += Sqr(PUInt64(Ptr)^ - avg);
      end;
      Inc(ptr,size);
    end;
  True:
    while (PtrUInt(ptr) < upper) do begin
      case size of
        1: Square += Sqr(PInt8(Ptr)^ - avg);
        2: Square += Sqr(PInt16(Ptr)^ - avg);
        4: Square += Sqr(PInt32(Ptr)^ - avg);
        8: Square += Sqr(PInt64(Ptr)^ - avg);
      end;
      Inc(ptr,size);
    end;
  end;
  Result := (Square / Len);
end;


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Returns the standard deviation of any float-type array
function StdevFPtr(Ptr:Pointer; Size:UInt8): Extended;
begin
  Result := Sqrt( VarianceFPtr(Ptr, Size) );
end;


// Returns the standard deviation of any int-type array
function StdevPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;
begin
  Result := Sqrt( VariancePtr(Ptr, Size, Signed) );
end;



{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Returns the mode of a numeric array
{$define ComputeMode :=
  cur := arr[0];
  hits := 1;
  best := 0;
  for i:=1 to High(Arr) do
  begin
    if (cur <> arr[i]) then
    begin
      if (hits > best) then
      begin
        best := hits;
        Result := cur;
      end;
      hits := 0;
      cur := Arr[I];
    end;
    Inc(hits);
  end;
  if (hits > best) then
    Result := cur;
}

function ModeFPtr(Ptr:Pointer; Size:UInt8): Extended;
var
  len,i,hits,best:Int32;
  Arr:TExtArray;
  dType:TDataType;
  cur:Extended;
begin
  if not(Size in [4,8,10]) then Exit(0);
  Len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  dType.vSize := Size;
  dType.vType := dtFloat;

  SetLength(Arr, Len);
  for i:=0 to Len-1 do
  begin
    Arr[i] := dType.VarCastF(ptr);
    Inc(ptr, dType.vSize);
  end;

  SortTEA(Arr);
  ComputeMode
end;


function ModePtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Int64;
var
  len,i,hits,best:Int32;
  Arr:TInt64Array;
  dType:TDataType;
  cur:Int64;
begin
  if not(Size in [1,2,4,8]) then Exit(0);
  Len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  dType.vSize := Size;
  if Signed then dType.vType := dtSigned
  else dType.vType := dtUnsigned;

  SetLength(Arr, Len);
  for i:=0 to Len-1 do
  begin
    Arr[i] := dType.VarCast(ptr);
    Inc(ptr, dType.vSize);
  end;

  SortTBiA(Arr);
  ComputeMode
end;


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Returns the median of any float-type array
function MedianFPtr(Ptr:Pointer; Size:UInt8): Extended;
var
  len,i,mid:Int32;
  Arr:TExtArray;
  dType:TDataType;
begin
  if not(Size in [4,8,10]) then Exit(0);
  Len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  dType.vSize := Size;
  dType.vType := dtFloat;

  SetLength(Arr, Len);
  for i:=0 to Len-1 do
  begin
    Arr[i] := dType.VarCastF(ptr);
    Inc(ptr, dType.vSize);
  end;

  SortTEA(Arr);
  mid := len div 2;
  if len mod 2 = 1 then
    Result := Arr[mid]
  else
    Result := (Arr[mid-1] + Arr[mid]) / 2;
end;


// Returns the median of any int-type array
function MedianPtr(Ptr:Pointer; Size:UInt8; Signed:Boolean): Extended;
var
  len,i,mid:Int32;
  Arr:TInt64Array;
  dType:TDataType;
begin
  if not(Size in [1,2,4,8]) then Exit(0);
  Len := PSizeInt(Ptr-SizeOf(SizeInt))^{$ifdef FPC}+1{$endif};
  dType.vSize := Size;
  if Signed then dType.vType := dtSigned
  else dType.vType := dtUnsigned;

  SetLength(Arr, Len);
  for i:=0 to Len-1 do
  begin
    Arr[i] := dType.VarCast(ptr);
    Inc(ptr, dType.vSize);
  end;

  SortTBiA(Arr);
  mid := len div 2;
  if len mod 2 = 1 then
    Result := Arr[mid]
  else
    Result := (Arr[mid-1] + Arr[mid]) / 2;
end;



{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~]
[--|  OLDER STUFF  |-----------------------------------------------------------]
[~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}


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
  Finds the amount of different indices, by comparing each index in "Arr1" to each index in "Arr2".
*}
function TIAMatches(const Arr1, Arr2:TIntArray; InPercent, Inversed:Boolean): Int32; 
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


end.





