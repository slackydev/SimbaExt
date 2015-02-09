{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Returns a cropped area of the matrix.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define GetAreaBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  x2 := Min(x2, W);
  y2 := Min(y2, H);
  SetLength(Result, y2-y1+1, x2-x1+1);
  for Y:=y1 to y2 do
    Move(Mat[y][x1], Result[y-y1][0], (x2-x1+1)*SizeOf(Mat[0,0]));
}

function GetArea(const Mat:T2DByteArray; x1,y1,x2,y2:Int32): T2DByteArray; overload;
var Y,W,H:Int32;
begin GetAreaBody end;


function GetArea(const Mat:T2DIntArray; x1,y1,x2,y2:Int32): T2DIntArray; overload;
var Y,W,H:Int32;
begin GetAreaBody end;


function GetArea(const Mat:T2DFloatArray; x1,y1,x2,y2:Int32): T2DFloatArray; overload;
var Y,W,H:Int32;
begin GetAreaBody end;


function GetArea(const Mat:T2DDoubleArray; x1,y1,x2,y2:Int32): T2DDoubleArray; overload;
var Y,W,H:Int32;
begin GetAreaBody end;


function GetArea(const Mat:T2DExtArray; x1,y1,x2,y2:Int32): T2DExtArray; overload;
var Y,W,H:Int32;
begin GetAreaBody end;
