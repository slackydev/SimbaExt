unit Sorting;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$inline on}
{$macro on}
interface

uses 
  CoreTypes, CoreMisc, SysUtils;

procedure InsSortTIA(var Arr:TIntArray; Left, Right:Integer); Inline;
procedure InsSortTBiA(var Arr:TInt64Array; Left, Right:Integer); Inline;
procedure InsSortTEA(var Arr:TExtArray; Left, Right:Integer); Inline;
procedure InsSortTDA(var Arr:TDoubleArray; Left, Right:Integer); Inline;
procedure InsSortTFA(var Arr:TFloatArray; Left, Right:Integer); Inline;
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Integer); Inline;

procedure SortTBA(var Arr: CoreTypes.TByteArray);
procedure SortTIA(var Arr: TIntArray);
procedure SortTBiA(var Arr: TInt64Array);
procedure SortTFA(var Arr: TFloatArray);
procedure SortTDA(var Arr: TDoubleArray);
procedure SortTEA(var Arr: TExtArray);

procedure SortTPA(var Arr: TPointArray);
procedure SortTPAFrom(var Arr: TPointArray; const From:TPoint);
procedure SortTPAbyRow(var Arr: TPointArray);
procedure SortTPAbyColumn(var Arr: TPointArray);
procedure SortTPAByY(var Arr: TPointArray);
procedure SortTPAByX(var Arr: TPointArray);

procedure SortTSA(var Arr: TStringArray; CaseInsesitive:Boolean=False); 
procedure SortTSANatural(var Arr: TStringArray);

procedure SortATPAFrom(var Arr:T2DPointArray; point:TPoint);
procedure SortATPAByLength(var Arr:T2DPointArray);
procedure SortATPAByMean(var Arr:T2DPointArray);
procedure SortATPAByFirst(var Arr:T2DPointArray);
procedure SortATPAByIndex(var Arr:T2DPointArray; index:Int32);

procedure SortATBAByLength(var Arr:T2DByteArray);
procedure SortATBAByMean(var Arr:T2DByteArray);
procedure SortATBAByFirst(var Arr:T2DByteArray);
procedure SortATBAByIndex(var Arr:T2DByteArray; index:Int32);

procedure SortATIAByLength(var Arr:T2DIntArray);
procedure SortATIAByMean(var Arr:T2DIntArray);
procedure SortATIAByFirst(var Arr:T2DIntArray);
procedure SortATIAByIndex(var Arr:T2DIntArray; index:Int32);

procedure SortATEAByLength(var Arr:T2DExtArray);
procedure SortATEAByMean(var Arr:T2DExtArray);
procedure SortATEAByFirst(var Arr:T2DExtArray);
procedure SortATEAByIndex(var Arr:T2DExtArray; index:Int32);

//-----------------------------------------------------------------------||
implementation
uses 
  PointTools, Statistics, Math;

{$define __NumericSortBody := 
  mid := Left + (Right - Left) shr 1;
  median3(Arr, left,mid,right);
  pivot := Arr[mid];
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
    Exch(Arr[i], Arr[j]);
    Inc(f);
    Inc(i);
    Dec(j);
  until False;
}


{$define __WeightedSortBody := 
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
}

  
{$I Sorting/SortingBase.pas}
{$I Sorting/TPASort.pas}  //special [TPointArray]
{$I Sorting/TBASort.pas}  //special [TByteArray]
{$I Sorting/TSASort.pas}  //special [TStringArray]
{$I Sorting/TIASort.pas}  //numsort_body
{$I Sorting/TBiASort.pas} //numsort_body
{$I Sorting/TFASort.pas}  //numsort_body
{$I Sorting/TDASort.pas}  //numsort_body
{$I Sorting/TEASort.pas}  //numsort_body
{$I Sorting/ATPASort.pas} //weightsort_body
{$I Sorting/ATBASort.pas} //weightsort_body
{$I Sorting/ATIASort.pas} //weightsort_body
{$I Sorting/ATEASort.pas} //weightsort_body

end.
