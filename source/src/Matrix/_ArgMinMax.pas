{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{*
 Find the maximum value in a matrix, returning that position.
*}
function ArgMax(Mat:T2DIntArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DExtArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DDoubleArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DFloatArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;



{*
 Find the minimum value in a matrix, returning that position.
*}
function ArgMin(Mat:T2DIntArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DExtArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DDoubleArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DFloatArray): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;







{***********************************************************]
 Extended version
[***********************************************************}
//procedure WrapAroundBox(var B: TBox; W,H: Integer); Inline; || defined in Indices

{*
 Find the maximum value in a matrix, returning that position.
*}
function ArgMax(Mat:T2DIntArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DExtArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMax(Mat:T2DFloatArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] > Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;



{*
 Find the minimum value in a matrix, returning that position.
*}
function ArgMin(Mat:T2DIntArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  //WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DExtArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DDoubleArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;


function ArgMin(Mat:T2DFloatArray; B:TBox): TPoint; overload;
var 
  X,Y,W,H:Integer;
begin
  Result := Point(0,0);
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W,H);
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      if Mat[Y][X] < Mat[Result.y][Result.x] then
      begin 
        Result.x := x;
        Result.y := y;
      end;
end;




