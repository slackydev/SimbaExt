unit MatrixOps;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses CoreTypes, SysUtils;

function ArgMax(Mat:T2DByteArray): TPoint; overload;
function ArgMax(Mat:T2DIntArray): TPoint; overload;
function ArgMax(Mat:T2DExtArray): TPoint; overload;
function ArgMax(Mat:T2DDoubleArray): TPoint; overload;
function ArgMax(Mat:T2DFloatArray): TPoint; overload;
function ArgMin(Mat:T2DByteArray): TPoint; overload;
function ArgMin(Mat:T2DIntArray): TPoint; overload;
function ArgMin(Mat:T2DExtArray): TPoint; overload;
function ArgMin(Mat:T2DDoubleArray): TPoint; overload;
function ArgMin(Mat:T2DFloatArray): TPoint; overload;

function ArgMax(Mat:T2DByteArray; B:TBox): TPoint; overload;
function ArgMax(Mat:T2DIntArray; B:TBox): TPoint; overload;
function ArgMax(Mat:T2DExtArray; B:TBox): TPoint; overload;
function ArgMax(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
function ArgMax(Mat:T2DFloatArray; B:TBox): TPoint; overload;
function ArgMin(Mat:T2DByteArray; B:TBox): TPoint; overload;
function ArgMin(Mat:T2DIntArray; B:TBox): TPoint; overload;
function ArgMin(Mat:T2DExtArray; B:TBox): TPoint; overload;
function ArgMin(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
function ArgMin(Mat:T2DFloatArray; B:TBox): TPoint; overload;

function Indices(const Mat:T2DByteArray; B:TBox; Value: Byte; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator): TPointArray; overload;
function Indices(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator): TPointArray; overload;

procedure MinMax(Mat:T2DByteArray; var Min, Max:Byte); overload;
procedure MinMax(Mat:T2DIntArray; var Min, Max:Integer); overload;
procedure MinMax(Mat:T2DExtArray; var Min, Max:Extended); overload;
procedure MinMax(Mat:T2DDoubleArray; var Min, Max:Double); overload;
procedure MinMax(Mat:T2DFloatArray; var Min, Max:Single); overload;


//-----------------------------------------------------------------------
implementation

uses BoxTools, CoreMisc; //WrapAround from Box, and Inc/Dec from CoreMisc

{$I Src/Core/Matrix/_ArgMinMax.pas}
{$I Src/Core/Matrix/_MinMax.pas}
{$I Src/Core/Matrix/_Indices.pas}



end.
