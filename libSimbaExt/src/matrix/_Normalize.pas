{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Normalizes the matrix in the range `Alpha` -> `Beta`
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define NormalizeBody :=
  H := High(Mat);
  if (length(mat) = 0) then
    NewException(exEmptyMatrix);

  W := High(Mat[0]);
  SetLength(Result, H+1,W+1);

  MinMax(Mat, Lo,Hi);
  oldRange := Hi-Lo;
  newRange := Beta-Alpha;
  
  for Y:=0 to H do
    for X:=0 to W do
      Result[Y,X] := (Mat[Y,X] - lo) / oldRange * newRange + Alpha;
}


function Normalize(const Mat:T2DByteArray; Alpha,Beta:Single): T2DFloatArray; overload;
var
  Lo,Hi:Byte;
  oldRange,newRange:Single;
  X,Y,W,H: Int32;
begin
  NormalizeBody
end;

function Normalize(const Mat:T2DIntArray; Alpha,Beta:Double): T2DDoubleArray; overload;
var
  Lo,Hi:Int32;
  oldRange,newRange:Double;
  X,Y,W,H: Int32;
begin
  NormalizeBody
end;


function Normalize(const Mat:T2DExtArray; Alpha,Beta:Extended): T2DExtArray; overload;
var
  Lo,Hi,oldRange,newRange:Extended;
  X,Y,W,H: Int32;
begin
  NormalizeBody
end;


function Normalize(const Mat:T2DDoubleArray; Alpha,Beta:Double): T2DDoubleArray; overload;
var
  Lo,Hi,oldRange,newRange:Double;
  X,Y,W,H: Int32;
begin
  NormalizeBody
end;


function Normalize(const Mat:T2DFloatArray; Alpha,Beta:Single): T2DFloatArray; overload;
var
  Lo,Hi,oldRange,newRange:Single;
  X,Y,W,H: Int32;
begin
  NormalizeBody
end;


