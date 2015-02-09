unit MatrixOps;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Manly just some 1D array math procedures
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses CoreTypes, SysUtils;

operator + (left,right:TByteArray): TByteArray;
operator + (left,right:TIntArray): TIntArray;
operator + (left,right:TInt64Array): TInt64Array;
operator + (left,right:TFloatArray): TFloatArray;
operator + (left,right:TDoubleArray): TDoubleArray;
operator + (left,right:TExtArray): TExtArray;

operator - (left,right:TByteArray): TByteArray;
operator - (left,right:TIntArray): TIntArray;
operator - (left,right:TInt64Array): TInt64Array;
operator - (left,right:TFloatArray): TFloatArray;
operator - (left,right:TDoubleArray): TDoubleArray;
operator - (left,right:TExtArray): TExtArray;

operator * (left,right:TByteArray): TByteArray;
operator * (left,right:TIntArray): TIntArray;
operator * (left,right:TInt64Array): TInt64Array;
operator * (left,right:TFloatArray): TFloatArray;
operator * (left,right:TDoubleArray): TDoubleArray;
operator * (left,right:TExtArray): TExtArray;

operator / (left,right:TByteArray): TByteArray;
operator / (left,right:TIntArray): TIntArray;
operator / (left,right:TInt64Array): TInt64Array;
operator / (left,right:TFloatArray): TFloatArray;
operator / (left,right:TDoubleArray): TDoubleArray;
operator / (left,right:TExtArray): TExtArray;


implementation

uses math;

{$define LoopBodyXY :=
  Len := High(Left);
  if High(Left) = -1 then Exit();
  if High(Right) <> High(Left) then Exit();
  
  SetLength(Result, len+1);
  for i:=0 to L do
}


{$define LoopBody :=
  Len := High(Left);
  if Len = -1 then Exit();
  
  SetLength(Result, len+1);
  for i:=0 to L do
}


//------------------------------------------------------------------------
{$define AdditionBody :=
  Len := High(Left);
  if High(Left) = -1 then Exit();
  if High(Right) <> High(Left) then Exit();
  
  SetLength(Result, len+1);
  for i:=0 to L do
    Result[i] := Left[i] + Right[i];
}
operator + (left,right:TByteArray): TByteArray;
var Len,i:Int32; begin AdditionBody end;

operator + (left,right:TIntArray): TIntArray;
var Len,i:Int32; begin AdditionBody end;

operator + (left,right:TInt64Array): TInt64Array;
var Len,i:Int32; begin AdditionBody end;

operator + (left,right:TFloatArray): TFloatArray;
var Len,i:Int32; begin AdditionBody end;

operator + (left,right:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin AdditionBody end;

operator + (left,right:TExtArray): TExtArray;
var Len,i:Int32; begin AdditionBody end;


//------------------------------------------------------------------------
{$define SubtractionBody :=
  Len := High(Left);
  if High(Left) = -1 then Exit();
  if High(Right) <> High(Left) then Exit();
  
  SetLength(Result, len+1);
  for i:=0 to L do
    Result[i] := Left[i] - Right[i];
}
operator - (left,right:TByteArray): TByteArray;
var Len,i:Int32; begin SubtractionBody end;

operator - (left,right:TIntArray): TIntArray;
var Len,i:Int32; begin SubtractionBody end;

operator - (left,right:TInt64Array): TInt64Array;
var Len,i:Int32; begin SubtractionBody end;

operator - (left,right:TFloatArray): TFloatArray;
var Len,i:Int32; begin SubtractionBody end;

operator - (left,right:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin SubtractionBody end;

operator - (left,right:TExtArray): TExtArray;
var Len,i:Int32; begin SubtractionBody end;


//------------------------------------------------------------------------
{$define MultiplicationBody :=
  Len := High(Left);
  if High(Left) = -1 then Exit();
  if High(Right) <> High(Left) then Exit();
  
  SetLength(Result, len+1);
  for i:=0 to L do
    Result[i] := Left[i] * Right[i];
}
operator - (left,right:TByteArray): TByteArray;
var Len,i:Int32; begin MultiplicationBody end;

operator - (left,right:TIntArray): TIntArray;
var Len,i:Int32; begin MultiplicationBody end;

operator - (left,right:TInt64Array): TInt64Array;
var Len,i:Int32; begin MultiplicationBody end;

operator - (left,right:TFloatArray): TFloatArray;
var Len,i:Int32; begin MultiplicationBody end;

operator - (left,right:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin MultiplicationBody end;

operator - (left,right:TExtArray): TExtArray;
var Len,i:Int32; begin MultiplicationBody end;



//------------------------------------------------------------------------
operator - (left,right:TByteArray): TByteArray;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] div Right[i]; end;

operator - (left,right:TIntArray): TIntArray;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] div Right[i]; end;

operator - (left,right:TInt64Array): TInt64Array;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] div Right[i]; end;

operator - (left,right:TFloatArray): TFloatArray;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] / Right[i]; end;

operator - (left,right:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] / Right[i]; end;

operator - (left,right:TExtArray): TExtArray;
var Len,i:Int32; begin LoopBodyXY Result[i] := Left[i] / Right[i]; end;


//------------------------------------------------------------------------
function Sqrt(left:TByteArray): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;

function Sqrt(left:TIntArray): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;

function Sqrt(left:TInt64Array): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;

function Sqrt(left:TFloatArray): TFloatArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;

function Sqrt(left:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;

function Sqrt(left:TExtArray): TExtArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqrt(Left[i]); end;


//------------------------------------------------------------------------
function Sqr(left:TByteArray): TByteArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;

function Sqr(left:TIntArray): TIntArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;

function Sqr(left:TInt64Array): TInt64Array;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;

function Sqr(left:TFloatArray): TFloatArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;

function Sqr(left:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;

function Sqr(left:TExtArray): TExtArray;
var Len,i:Int32; begin LoopBody Result[i] := Sqr(Left[i]); end;



//------------------------------------------------------------------------
function Abs(left:TByteArray): TByteArray;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;

function Abs(left:TIntArray): TIntArray;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;

function Abs(left:TInt64Array): TInt64Array;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;

function Abs(left:TFloatArray): TFloatArray;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;

function Abs(left:TDoubleArray): TDoubleArray;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;

function Abs(left:TExtArray): TExtArray;
var Len,i:Int32; begin LoopBody Result[i] := Abs(Left[i]); end;



end.