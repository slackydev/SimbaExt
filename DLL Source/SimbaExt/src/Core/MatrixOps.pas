unit MatrixOps;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses CoreTypes, Sorting, SysUtils, SimpleHeap, MatrixMath;

function ArgMax(Mat: T2DByteArray): TPoint; overload;
function ArgMax(Mat: T2DIntArray): TPoint; overload;
function ArgMax(Mat: T2DExtArray): TPoint; overload;
function ArgMax(Mat: T2DDoubleArray): TPoint; overload;
function ArgMax(Mat: T2DFloatArray): TPoint; overload;
function ArgMin(Mat: T2DByteArray): TPoint; overload;
function ArgMin(Mat: T2DIntArray): TPoint; overload;
function ArgMin(Mat: T2DExtArray): TPoint; overload;
function ArgMin(Mat: T2DDoubleArray): TPoint; overload;
function ArgMin(Mat: T2DFloatArray): TPoint; overload;

function ArgMax(Mat: T2DByteArray; B: TBox): TPoint; overload;
function ArgMax(Mat: T2DIntArray; B: TBox): TPoint; overload;
function ArgMax(Mat: T2DExtArray; B: TBox): TPoint; overload;
function ArgMax(Mat: T2DDoubleArray; B: TBox): TPoint; overload;
function ArgMax(Mat: T2DFloatArray; B: TBox): TPoint; overload;
function ArgMin(Mat: T2DByteArray; B: TBox): TPoint; overload;
function ArgMin(Mat: T2DIntArray; B: TBox): TPoint; overload;
function ArgMin(Mat: T2DExtArray; B: TBox): TPoint; overload;
function ArgMin(Mat: T2DDoubleArray; B: TBox): TPoint; overload;
function ArgMin(Mat: T2DFloatArray; B: TBox): TPoint; overload;


function Indices(const Mat: T2DByteArray; B: TBox; Value: byte;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DIntArray; B: TBox; Value: integer;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DExtArray; B: TBox; Value: extended;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DDoubleArray; B: TBox; Value: double;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DFloatArray; B: TBox; Value: single;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DByteArray; Value: byte;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DIntArray; Value: integer;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DExtArray; Value: extended;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DDoubleArray; Value: double;
  const Comparator: TComparator): TPointArray; overload;
function Indices(const Mat: T2DFloatArray; Value: single;
  const Comparator: TComparator): TPointArray; overload;

procedure MinMax(Mat: T2DByteArray; var Min, Max: byte); overload;
procedure MinMax(Mat: T2DIntArray; var Min, Max: integer); overload;
procedure MinMax(Mat: T2DExtArray; var Min, Max: extended); overload;
procedure MinMax(Mat: T2DDoubleArray; var Min, Max: double); overload;
procedure MinMax(Mat: T2DFloatArray; var Min, Max: single); overload;

function VarMulti(Mat: T2DByteArray; Count: Int32; HiLo: boolean): CoreTypes.TByteArray; overload;
function VarMulti(Mat: T2DIntArray; Count: Int32; HiLo: boolean): TIntArray; overload;
function VarMulti(Mat: T2DExtArray; Count: Int32; HiLo: boolean): TExtArray; overload;
function VarMulti(Mat: T2DDoubleArray; Count: Int32; HiLo: boolean): TDoubleArray; overload;
function VarMulti(Mat: T2DFloatArray; Count: Int32; HiLo: boolean): TFloatArray; overload;

function ArgMulti(Mat:T2DByteArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
function ArgMulti(Mat:T2DIntArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
function ArgMulti(Mat:T2DExtArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
function ArgMulti(Mat:T2DDoubleArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
function ArgMulti(Mat:T2DFloatArray; Count:Int32; HiLo:Boolean): TPointArray; overload;


function CombineMatrix(Mat1:T2DByteArray; Mat2:T2DByteArray; OP:Char): T2DByteArray; overload;
function CombineMatrix(Mat1:T2DIntArray; Mat2:T2DIntArray; OP:Char): T2DIntArray; overload;
function CombineMatrix(Mat1:T2DFloatArray; Mat2:T2DFloatArray; OP:Char): T2DFloatArray; overload;
function CombineMatrix(Mat1:T2DDoubleArray; Mat2:T2DDoubleArray; OP:Char): T2DDoubleArray; overload;
function CombineMatrix(Mat1:T2DExtArray; Mat2:T2DExtArray; OP:Char): T2DExtArray; overload;



//-----------------------------------------------------------------------
implementation

uses BoxTools, CoreMisc; //WrapAround from Box, and Inc/Dec from CoreMisc

{$I Src/Core/Matrix/_ArgMinMax.pas}
{$I Src/Core/Matrix/_MinMax.pas}
{$I Src/Core/Matrix/_Indices.pas}
{$I Src/Core/Matrix/_VarMulti.pas}
{$I Src/Core/Matrix/_ArgMulti.pas}
{$I Src/Core/Matrix/_Combine.pas}

end.
