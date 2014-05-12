unit Sorting;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$inline on}
(*
 Modified version of Quicksort to get better best-case performance.
 > The closer the array is to beeing already sorted, the faster it gets.
 > Does not matter much if the array is reversed or not.
 > Fallback to ShellSort to avoid a bad worst-case scenario.

 How does it work?
 1. Based on a sligtly modified Quicksort.
 2. Recursively partitions the array at the middle (See step 4)...
 3. If the partition Left to Right is less then a cirtain criteria InsertionSort is used.
 
 > If not insertion:
 4. Pivot is selected as a Median of 5: Left - MidLeft - Mid - MidRight - Right.

 5. If all items from "Left up to pivot" and "Right down to pivot" is (close to) sorted
 then we run InsertionSort on the "partition", and exit. If not then we continue
 doing a regular quicksort. This check is then continued ~6 times in each partioning.
 
 6. If the recursion depth goes bellow a given limit then we fall back to ShellSort to avoid
 worst-case scenario in Quicksort: O(n^2).
 
 -------
 My testes show that it can be ~30x faster then QuickSort..
 >> Much more whenever quicksort goes O(n^2) - Rare/Depending on pivot selection.
*)
interface

uses 
  CoreTypes, SysUtils;

procedure InsSortTIA(var Arr:TIntArray; Left, Right:Integer); Inline;
procedure InsSortTEA(var Arr:TExtArray; Left, Right:Integer); Inline;
procedure InsSortTPA(var Arr:TPointArray; Weight:TIntArray; Left, Right:Integer); Inline;
procedure ShellSortTIA(var Arr: TIntArray);
procedure ShellSortTEA(var Arr: TExtArray);
procedure ShellSortTPA(var Arr: TPointArray; Weight:TIntArray);

procedure SortTBA(var Arr: CoreTypes.TByteArray);
procedure SortTIA(var Arr: TIntArray);
procedure SortTEA(var Arr: TExtArray);

procedure SortTPA(var Arr: TPointArray);
procedure SortTPAFrom(var Arr: TPointArray; const From:TPoint);
procedure SortTPAbyRow(var Arr: TPointArray);
procedure SortTPAbyColumn(var Arr: TPointArray);
procedure SortTPAByY(var Arr: TPointArray);
procedure SortTPAByX(var Arr: TPointArray);

procedure SortTSA(var Arr: TStringArray; CaseInsesitive:Boolean=False); 
procedure SortTSANatural(var Arr: TStringArray);

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
  PointTools, Numeric, CoreMisc, Math;

{$I Sorting/SortingBase.pas}
{$I Sorting/TPASort.pas}
{$I Sorting/TBASort.pas}
{$I Sorting/TIASort.pas}
{$I Sorting/TEASort.pas}
{$I Sorting/TSASort.pas}
{$I Sorting/ATPASort.pas}
{$I Sorting/ATBASort.pas}
{$I Sorting/ATIASort.pas}
{$I Sorting/ATEASort.pas}

end.
