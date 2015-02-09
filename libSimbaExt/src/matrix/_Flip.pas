{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Flips the matrices order.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{$define FlipMatBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  SetLength(Result, W+1,H+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[x,y] := Mat[y,x];
}

function FlipMat(const Mat:T2DByteArray): T2DByteArray; overload;
var x,y,H,W:Integer;
begin FlipMatBody end;

function FlipMat(const Mat:T2DIntArray): T2DIntArray; overload;
var x,y,H,W:Integer;
begin FlipMatBody end;


function FlipMat(const Mat:T2DFloatArray): T2DFloatArray; overload;
var x,y,H,W:Integer;
begin FlipMatBody end;


function FlipMat(const Mat:T2DDoubleArray): T2DDoubleArray; overload;
var x,y,H,W:Integer;
begin FlipMatBody end;


function FlipMat(const Mat:T2DExtArray): T2DExtArray; overload;
var x,y,H,W:Integer;
begin FlipMatBody end;
