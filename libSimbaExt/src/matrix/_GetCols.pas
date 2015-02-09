{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Returns a matrix containing a copy of all the values in the given cols.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{$define GetColsBody :=
  if Length(Mat) = 0 then NewException(exEmptyMatrix);
  ToCol := Min(ToCol, High(Mat[0]));
  H := High(Mat);
  NewW := (ToCol - FromCol)+1;
  SetLength(Result, H+1, NewW);
  case (NewW = 1) and True of
    False:
      for y:=0 to H do
        Move(Mat[y][FromCol], Result[y][0], NewW*SizeOf(Mat[0,0]));
    True:
      for y:=0 to H do
        Result[y][0] := Mat[y][FromCol];
  end;
}

function GetCols(const Mat:T2DByteArray; FromCol, ToCol:Int32): T2DByteArray; overload;
var y,H,NewW:Integer;
begin GetColsBody end;

function GetCols(const Mat:T2DIntArray; FromCol, ToCol:Int32): T2DIntArray; overload;
var y,H,NewW:Integer;
begin GetColsBody end;

function GetCols(const Mat:T2DFloatArray; FromCol, ToCol:Int32): T2DFloatArray; overload;
var y,H,NewW:Integer;
begin GetColsBody end;

function GetCols(const Mat:T2DDoubleArray; FromCol, ToCol:Int32): T2DDoubleArray; overload;
var y,H,NewW:Integer;
begin GetColsBody end;

function GetCols(const Mat:T2DExtArray; FromCol, ToCol:Int32): T2DExtArray; overload;
var y,H,NewW:Integer;
begin GetColsBody end;
