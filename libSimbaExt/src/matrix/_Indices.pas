{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define IndicesExBody :=
  H := High(Mat);
  if (length(mat) = 0) then
    NewException(exEmptyMatrix);

  W := High(Mat[0]);
  WrapAroundBox(B, W+1,H+1);
  SetLength(Result, (B.x2-B.x1+1)*(B.y2-B.y1+1));
  i := 0;
  for Y:=B.y1 to B.y2 do
    for X:=B.x1 to B.x2 do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  Result[Asc(i)] := Point(x,y);
        __GT__: if Mat[y][x] > Value then  Result[Asc(i)] := Point(x,y);
        __EQ__: if Mat[y][x] = Value then  Result[Asc(i)] := Point(x,y);
        __LE__: if Mat[y][x] <= Value then Result[Asc(i)] := Point(x,y);
        __GE__: if Mat[y][x] >= Value then Result[Asc(i)] := Point(x,y);
        __NE__: if Mat[y][x] <> Value then Result[Asc(i)] := Point(x,y);
      end;
  SetLength(Result, i);
}

(*
 Extract all the points that fitt the bill within a area defined by B:TBox
*)
function Indices(const Mat:T2DByteArray; B:TBox; Value: Byte; const Comparator:EComparator): TPointArray; overload;
var  X,Y,W,H,i: Int32;
begin IndicesExBody end;


function Indices(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:EComparator): TPointArray; overload;
var  X,Y,W,H,i: Int32;
begin IndicesExBody end;


function Indices(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:EComparator): TPointArray; overload;
var  X,Y,W,H,i: Int32;
begin IndicesExBody end;


function Indices(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:EComparator): TPointArray; overload;
var  X,Y,W,H,i: Int32;
begin IndicesExBody end;



function Indices(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:EComparator): TPointArray; overload;
var  X,Y,W,H,i: Int32;
begin IndicesExBody end;


{$define IndicesBody :=
  H := High(mat);
  if (length(mat) = 0) then
    NewException(exEmptyMatrix);

  W := High(Mat[0]);
  ResList.Init();
  for Y:=0 to H do
    for X:=0 to W do
      case Comparator of
        __LT__: if Mat[y][x] < Value then  ResList.Append(x,y);
        __GT__: if Mat[y][x] > Value then  ResList.Append(x,y);
        __EQ__: if Mat[y][x] = Value then  ResList.Append(x,y);
        __LE__: if Mat[y][x] <= Value then ResList.Append(x,y);
        __GE__: if Mat[y][x] >= Value then ResList.Append(x,y);
        __NE__: if Mat[y][x] <> Value then ResList.Append(x,y);
      end;
  Result := ResList.Finalize();
}

(*
 Extract all the points that fitt the bill
*)
function Indices(const Mat:T2DByteArray; Value: Byte; const Comparator:EComparator): TPointArray; overload;
var W,H,x,y: Int32; ResList:TPointList;
begin IndicesBody end;


function Indices(const Mat:T2DIntArray; Value: Integer; const Comparator:EComparator): TPointArray; overload;
var W,H,x,y: Int32; ResList:TPointList;
begin IndicesBody end;


function Indices(const Mat:T2DExtArray; Value: Extended; const Comparator:EComparator): TPointArray; overload;
var W,H,x,y: Int32; ResList:TPointList;
begin IndicesBody end;


function Indices(const Mat:T2DDoubleArray; Value: Double; const Comparator:EComparator): TPointArray; overload;
var W,H,x,y: Int32; ResList:TPointList;
begin IndicesBody end;


function Indices(const Mat:T2DFloatArray; Value: Single; const Comparator:EComparator): TPointArray; overload;
var W,H,x,y: Int32; ResList:TPointList;
begin IndicesBody end;
