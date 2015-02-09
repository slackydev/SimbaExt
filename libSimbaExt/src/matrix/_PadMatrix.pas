{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define PadMatrixBody :=
  OldH := Length(Mat);
  if (OldH = 0) then NewException(exEmptyMatrix);
  OldW := Length(Mat[0]);
  H := Pre.y+OldH+post.y;
  W := Pre.x+OldW+post.x;
  SetLength(Result, H, W);

  for y:=0 to OldH-1 do
    Move(Mat[y][0], Result[y+pre.y][pre.x], OldW*SizeOf(Mat[0,0]));
}

{*
  Pads the matrix with "empty" from all sides.
*}
function Pad(constref Mat:T2DByteArray; Pre, Post:TPoint): T2DByteArray;
var y,oldw,oldh,w,h:Int32;
begin PadMatrixBody end;


function Pad(constref Mat:T2DIntArray; Pre, Post:TPoint): T2DIntArray;
var y,oldw,oldh,w,h:Int32;
begin PadMatrixBody end;


function Pad(constref Mat:T2DFloatArray; Pre, Post:TPoint): T2DFloatArray;
var y,oldw,oldh,w,h:Int32;
begin PadMatrixBody end;


function Pad(constref Mat:T2DDoubleArray; Pre, Post:TPoint): T2DDoubleArray;
var y,oldw,oldh,w,h:Int32;
begin PadMatrixBody end;


function Pad(constref Mat:T2DExtArray; Pre, Post:TPoint): T2DExtArray;
var y,oldw,oldh,w,h:Int32;
begin PadMatrixBody end;
