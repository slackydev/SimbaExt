{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


{*
 Extract all the points that fitt the bill
*}
function Indices(const Mat:T2DByteArray; B:TBox; Value: Byte; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;


function Indices(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;



function Indices(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;


function Indices(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;



function Indices(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;




{*
 Extract all the points that fitt the bill
*}
function Indices(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  SetLength(Result, (W+1)*(H+1));
  i := 0;
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;

function Indices(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  SetLength(Result, (W+1)*(H+1));
  i := 0;
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;



function Indices(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  SetLength(Result, (W+1)*(H+1));
  i := 0;
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;


function Indices(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  SetLength(Result, (W+1)*(H+1));
  i := 0;
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;


function Indices(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator): TPointArray; overload;
var 
  X,Y,W,H,i: Integer;
begin
  H := High(Mat);
  W := High(Mat[0]);
  SetLength(Result, (W+1)*(H+1));
  i := 0;
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Inc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Inc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Inc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Inc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Inc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Inc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
end;
