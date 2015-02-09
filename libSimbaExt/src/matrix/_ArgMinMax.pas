{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define ArgMaxBody :=
  Result := Point(0,0);
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
}

{$define ArgMinBody :=
  Result := Point(0,0);
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
}

{*
 Find the maximum value in a matrix, returning that position.
*}
function ArgMax(Mat:T2DByteArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxBody end;

function ArgMax(Mat:T2DIntArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxBody end;


function ArgMax(Mat:T2DExtArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxBody end;


function ArgMax(Mat:T2DDoubleArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxBody end;


function ArgMax(Mat:T2DFloatArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxBody end;


{*
 Find the minimum value in a matrix, returning that position.
*}
function ArgMin(Mat:T2DByteArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinBody end;


function ArgMin(Mat:T2DIntArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinBody end;


function ArgMin(Mat:T2DExtArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinBody end;


function ArgMin(Mat:T2DDoubleArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinBody end;


function ArgMin(Mat:T2DFloatArray): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinBody end;







{***********************************************************]
 Extended version [search within a specified box]
[***********************************************************}
{$define ArgMaxExBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  WrapAroundBox(B, W+1,H+1);
  Result := Point(B.x1,B.y1);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
}

{$define ArgMinExBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  WrapAroundBox(B, W+1,H+1);
  Result := Point(B.x1,B.y1);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
}


{*
 Find the maximum value in a matrix, returning that position.
*}
function ArgMax(Mat:T2DByteArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxExBody end;


function ArgMax(Mat:T2DIntArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxExBody end;


function ArgMax(Mat:T2DExtArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxExBody end;


function ArgMax(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxExBody end;


function ArgMax(Mat:T2DFloatArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMaxExBody end;



{*
 Find the minimum value in a matrix, returning that position.
*}
function ArgMin(Mat:T2DByteArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinExBody end;


function ArgMin(Mat:T2DIntArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinExBody end;


function ArgMin(Mat:T2DExtArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinExBody end;


function ArgMin(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinExBody end;


function ArgMin(Mat:T2DFloatArray; B:TBox): TPoint; overload;
var X,Y,W,H:Int32;
begin ArgMinExBody end;




