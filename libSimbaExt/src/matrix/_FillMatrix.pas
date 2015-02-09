{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Fills the matrix given area of the matrix with the value "Fill"
 Upper part of the area wraps around so given [0,0,-1,-1] will be equal to [0,0,w-1,h-1]
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{$define FillMatrixExBody :=
  H := Length(Mat);
  if (H = 0) then NewException(exEmptyMatrix);
  W := Length(Mat[0]);

  Area.x1 := Max(0,Area.x1);
  Area.y1 := Max(0,Area.y1);
  Area.x2 := Modulo(Area.x2,W);
  Area.y2 := Modulo(Area.y2,H);

  for y:=Area.y1 to Area.y2 do
    for x:=Area.x1 to Area.x2 do
      Mat[y,x] := Fill;
}

procedure FillMatrix(var Mat:T2DByteArray; Area:TBox; Fill:UInt8);
var x,y,H,W:Int32;
begin FillMatrixExBody end;


procedure FillMatrix(var Mat:T2DIntArray; Area:TBox; Fill:Int32);
var x,y,H,W:Int32;
begin FillMatrixExBody end;


procedure FillMatrix(var Mat:T2DFloatArray; Area:TBox; Fill:Single);
var x,y,H,W:Int32;
begin FillMatrixExBody end;


procedure FillMatrix(var Mat:T2DDoubleArray; Area:TBox; Fill:Double);
var x,y,H,W:Int32;
begin FillMatrixExBody end;


procedure FillMatrix(var Mat:T2DExtArray; Area:TBox; Fill:Extended);
var x,y,H,W:Int32;
begin FillMatrixExBody end;


//------------------------------------------------------------------------
{$define FillMatrixBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  for y:=0 to H do
    for x:=0 to W do
      Mat[y,x] := Fill;
}

procedure FillMatrix(var Mat:T2DByteArray; Fill:UInt8);
var x,y,H,W:Int32;
begin FillMatrixBody end;


procedure FillMatrix(var Mat:T2DIntArray; Fill:Int32);
var x,y,H,W:Int32;
begin FillMatrixBody end;


procedure FillMatrix(var Mat:T2DFloatArray; Fill:Single);
var x,y,H,W:Int32;
begin FillMatrixBody end;


procedure FillMatrix(var Mat:T2DDoubleArray; Fill:Double);
var x,y,H,W:Int32;
begin FillMatrixBody end;


procedure FillMatrix(var Mat:T2DExtArray; Fill:Extended);
var x,y,H,W:Int32;
begin FillMatrixBody end;
