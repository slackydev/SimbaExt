{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Computes the cumulative sum of a matrix
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define CumSumBody_1 :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);
}
{$define CumSumBody_2 :=
  SetLength(Result,H+1);
  for i:=0 to H do Result[i] := Copy(Mat[i]);
}
{$define CumSumBody_3 :=
  if axis = 0 then begin
    for i:=1 to h do
      for j:=0 to w do
        Result[i,j] += Result[i-1,j];
  end else
    for i:=0 to h do
      for j:=1 to w do
        Result[i,j] += Result[i,j-1];
}

function CumSum(constref Mat:T2DByteArray; axis:Int8): T2DIntArray;
var W,H,i,j:Int32;
begin
  CumSumBody_1

  SetLength(Result,H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j];

  CumSumBody_3
end;


function CumSum(constref Mat:T2DIntArray; axis:Int8): T2DIntArray;
var W,H,i,j:Int32;
begin
  CumSumBody_1
  CumSumBody_2
  CumSumBody_3
end;


function CumSum(constref Mat:T2DFloatArray; axis:Int8): T2DFloatArray;
var W,H,i,j:Int32;
begin
  CumSumBody_1
  CumSumBody_2
  CumSumBody_3
end;


function CumSum(constref Mat:T2DDoubleArray; axis:Int8): T2DDoubleArray;
var W,H,i,j:Int32;
begin
  CumSumBody_1
  CumSumBody_2
  CumSumBody_3
end;


function CumSum(constref Mat:T2DExtArray; axis:Int8): T2DExtArray;
var W,H,i,j:Int32;
begin
  CumSumBody_1
  CumSumBody_2
  CumSumBody_3
end;
