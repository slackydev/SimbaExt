{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap matrixops methods (matrixops.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{-------| Fill (matrix) |-------}
procedure exp_FillMatExB(var SE:Pointer; var Mat:T2DByteArray; Area:TBox; Fill:UInt8); cdecl;
begin FillMatrix(Mat,Area,Fill); end;

procedure exp_FillMatExI(var SE:Pointer; var Mat:T2DIntArray; Area:TBox; Fill:Int32); cdecl;
begin FillMatrix(Mat,Area,Fill); end;

procedure exp_FillMatExF(var SE:Pointer; var Mat:T2DFloatArray; Area:TBox; Fill:Single); cdecl;
begin FillMatrix(Mat,Area,Fill); end;

procedure exp_FillMatExD(var SE:Pointer; var Mat:T2DDoubleArray; Area:TBox; Fill:Double); cdecl;
begin FillMatrix(Mat,Area,Fill); end;

procedure exp_FillMatExE(var SE:Pointer; var Mat:T2DExtArray; Area:TBox; Fill:Extended); cdecl;
begin FillMatrix(Mat,Area,Fill); end;


procedure exp_FillMatB(var SE:Pointer; var Mat:T2DByteArray; Area:TBox; Fill:UInt8); cdecl;
begin FillMatrix(Mat,Fill); end;

procedure exp_FillMatI(var SE:Pointer; var Mat:T2DIntArray; Area:TBox; Fill:Int32); cdecl;
begin FillMatrix(Mat,Fill); end;

procedure exp_FillMatF(var SE:Pointer; var Mat:T2DFloatArray; Area:TBox; Fill:Single); cdecl;
begin FillMatrix(Mat,Fill); end;

procedure exp_FillMatD(var SE:Pointer; var Mat:T2DDoubleArray; Area:TBox; Fill:Double); cdecl;
begin FillMatrix(Mat,Fill); end;

procedure exp_FillMatE(var SE:Pointer; var Mat:T2DExtArray; Area:TBox; Fill:Extended); cdecl;
begin FillMatrix(Mat,Fill); end;



{-------| se.indices |-------}
function exp_indicesB(var SE:Pointer; const Mat:T2DByteArray; Value: Byte; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, Value, Comparator);
end;

function exp_indicesI(var SE:Pointer; const Mat:T2DIntArray; Value: Integer; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, Value, Comparator);
end;

function exp_indicesE(var SE:Pointer; const Mat:T2DExtArray; Value: Extended; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, Value, Comparator);
end;

function exp_indicesD(var SE:Pointer; const Mat:T2DDoubleArray; Value: Double; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, Value, Comparator);
end;

function exp_indicesF(var SE:Pointer; const Mat:T2DFloatArray; Value: Single; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, Value, Comparator);
end;


{-------| Extended version of se.indices |-------}
function exp_indicesExB(var SE:Pointer; const Mat:T2DByteArray; B:TBox; Value: Byte; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, B, Value, Comparator);
end;

function exp_indicesExI(var SE:Pointer; const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, B, Value, Comparator);
end;

function exp_indicesExE(var SE:Pointer; const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, B, Value, Comparator);
end;

function exp_indicesExD(var SE:Pointer; const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, B, Value, Comparator);
end;

function exp_indicesExF(var SE:Pointer; const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:EComparator): TPointArray; cdecl;
begin
  Result := indices(Mat, B, Value, Comparator);
end;


{-------| se.MinMax |-------}
procedure exp_MinMaxB(var SE:Pointer; Mat:T2DByteArray; var Min, Max:Byte); cdecl;
begin MinMax(Mat, Min, Max); end;

procedure exp_MinMaxI(var SE:Pointer; Mat:T2DIntArray; var Min, Max:Int32); cdecl;
begin MinMax(Mat, Min, Max); end;

procedure exp_MinMaxF(var SE:Pointer; Mat:T2DFloatArray; var Min, Max:Single); cdecl;
begin MinMax(Mat, Min, Max); end;

procedure exp_MinMaxD(var SE:Pointer; Mat:T2DDoubleArray; var Min, Max:Double); cdecl;
begin MinMax(Mat, Min, Max); end;

procedure exp_MinMaxE(var SE:Pointer; Mat:T2DExtArray; var Min, Max:Extended); cdecl;
begin MinMax(Mat, Min, Max); end;


{-------| se.Min |-------}
function exp_MinB(var SE:Pointer; Mat:T2DByteArray): Byte; cdecl;
var _:Byte;     begin MinMax(Mat, Result, _); end;

function exp_MinI(var SE:Pointer; Mat:T2DIntArray): Int32; cdecl;
var _:Int32;    begin MinMax(Mat, Result, _); end;

function exp_MinF(var SE:Pointer; Mat:T2DFloatArray): Single; cdecl;
var _:Single;   begin MinMax(Mat, Result, _); end;

function exp_MinD(var SE:Pointer; Mat:T2DDoubleArray): Double; cdecl;
var _:Double;   begin MinMax(Mat, Result, _); end;

function exp_MinE(var SE:Pointer; Mat:T2DExtArray): Extended; cdecl;
var _:Extended; begin MinMax(Mat, Result, _); end;


{-------| se.Max |-------}
function exp_MaxB(var SE:Pointer; Mat:T2DByteArray): Byte; cdecl;
var _:Byte;     begin MinMax(Mat, _, Result); end;

function exp_MaxI(var SE:Pointer; Mat:T2DIntArray): Int32; cdecl;
var _:Int32;    begin MinMax(Mat, _, Result); end;

function exp_MaxF(var SE:Pointer; Mat:T2DFloatArray): Single; cdecl;
var _:Single;   begin MinMax(Mat, _, Result); end;

function exp_MaxD(var SE:Pointer; Mat:T2DDoubleArray): Double; cdecl;
var _:Double;   begin MinMax(Mat, _, Result); end;

function exp_MaxE(var SE:Pointer; Mat:T2DExtArray): Extended; cdecl;
var _:Extended; begin MinMax(Mat, _, Result); end;



{-------| se.ArgMax |-------}
function exp_ArgMaxB(var SE:Pointer; Mat:T2DByteArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxI(var SE:Pointer; Mat:T2DIntArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxE(var SE:Pointer; Mat:T2DExtArray): TPoint; Cdecl; 
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxD(var SE:Pointer; Mat:T2DDoubleArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxF(var SE:Pointer; Mat:T2DFloatArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

{-------| se.ArgMin |-------}
function exp_ArgMinB(var SE:Pointer; Mat:T2DByteArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinI(var SE:Pointer; Mat:T2DIntArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinE(var SE:Pointer; Mat:T2DExtArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinD(var SE:Pointer; Mat:T2DDoubleArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinF(var SE:Pointer; Mat:T2DFloatArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;


{-------| Extended version of se.ArgMin/se.ArgMax |-------}
//argmax
function exp_ArgMaxExB(var SE:Pointer; Mat:T2DByteArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExI(var SE:Pointer; Mat:T2DIntArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExE(var SE:Pointer; Mat:T2DExtArray; B:TBox): TPoint; Cdecl; 
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExD(var SE:Pointer; Mat:T2DDoubleArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExF(var SE:Pointer; Mat:T2DFloatArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

//se.argmin
function exp_ArgMinExB(var SE:Pointer; Mat:T2DByteArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExI(var SE:Pointer; Mat:T2DIntArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExE(var SE:Pointer; Mat:T2DExtArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExD(var SE:Pointer; Mat:T2DDoubleArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExF(var SE:Pointer; Mat:T2DFloatArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;


{-------| se.VarMulti |-------}
function exp_VarMultiB(var SE:Pointer; const Mat:T2DByteArray; Count: Int32; HiLo:LongBool): CoreTypes.TByteArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiI(var SE:Pointer; const Mat:T2DIntArray; Count: Int32; HiLo:LongBool): TIntArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiE(var SE:Pointer; const Mat:T2DExtArray; Count: Int32; HiLo:LongBool): TExtArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiD(var SE:Pointer; const Mat:T2DDoubleArray; Count: Int32; HiLo:LongBool): TDoubleArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiF(var SE:Pointer; const Mat:T2DFloatArray; Count: Int32; HiLo:LongBool): TFloatArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;


{-------| se.ArgMulti |-------}
function exp_ArgMultiB(var SE:Pointer; const Mat:T2DByteArray; Count: Int32; HiLo:LongBool): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiI(var SE:Pointer; const Mat:T2DIntArray; Count: Int32; HiLo:LongBool): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiE(var SE:Pointer; const Mat:T2DExtArray; Count: Int32; HiLo:LongBool): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiD(var SE:Pointer; const Mat:T2DDoubleArray; Count: Int32; HiLo:LongBool): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiF(var SE:Pointer; const Mat:T2DFloatArray; Count: Int32; HiLo:LongBool): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;


{-------| CombineMatrix |-------}
function exp_CombineMatB(var SE:Pointer; const Mat1, Mat2:T2DByteArray; Op:Char): T2DByteArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatI(var SE:Pointer; const Mat1, Mat2:T2DIntArray; Op:Char): T2DIntArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatF(var SE:Pointer; const Mat1, Mat2:T2DFloatArray; Op:Char): T2DFloatArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatD(var SE:Pointer; const Mat1, Mat2:T2DDoubleArray; Op:Char): T2DDoubleArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatE(var SE:Pointer; const Mat1, Mat2:T2DExtArray; Op:Char): T2DExtArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;


{-------| Get- & PutValues |-------}
function exp_GetValuesB(var SE:Pointer; const Mat:T2DByteArray; const indices:TPointArray): TByteArray; cdecl;
begin Result := GetValues(Mat, indices); end;
 
function exp_GetValuesI(var SE:Pointer; const Mat:T2DIntArray; const indices:TPointArray): TIntArray; cdecl;
begin Result := GetValues(Mat, indices); end;
 
function exp_GetValuesF(var SE:Pointer; const Mat:T2DFloatArray; const indices:TPointArray): TFloatArray; cdecl;
begin Result := GetValues(Mat, indices); end;
 
function exp_GetValuesD(var SE:Pointer; const Mat:T2DDoubleArray; const indices:TPointArray): TDoubleArray; cdecl;
begin Result := GetValues(Mat, indices); end;
 
function exp_GetValuesE(var SE:Pointer; const Mat:T2DExtArray; const indices:TPointArray): TExtArray; cdecl;
begin Result := GetValues(Mat, indices); end;
 
//Put
procedure exp_PutValuesB(var SE:Pointer; var Matrix:T2DByteArray; const indices:TPointArray; const values:CoreTypes.TByteArray); cdecl;
begin PutValues(Matrix, indices, values); end;

procedure exp_PutValuesI(var SE:Pointer; var Matrix:T2DIntArray; const indices:TPointArray; const values:TIntArray); cdecl;
begin PutValues(Matrix, indices, values); end;

procedure exp_PutValuesF(var SE:Pointer; var Matrix:T2DFloatArray; const indices:TPointArray; const values:TFloatArray); cdecl;
begin PutValues(Matrix, indices, values); end;

procedure exp_PutValuesD(var SE:Pointer; var Matrix:T2DDoubleArray; const indices:TPointArray; const values:TDoubleArray); cdecl;
begin PutValues(Matrix, indices, values); end;

procedure exp_PutValuesE(var SE:Pointer; var Matrix:T2DExtArray; const indices:TPointArray; const values:TExtArray); cdecl;
begin PutValues(Matrix, indices, values); end;


{-------| Normalize (matrix) |-------}
function exp_NormalizeMatB(var SE:Pointer; const Mat:T2DByteArray; Alpha, Beta: Single): T2DFloatArray; cdecl;
begin Result := Normalize(Mat, Alpha, Beta); end;

function exp_NormalizeMatI(var SE:Pointer; const Mat:T2DIntArray; Alpha, Beta: Double): T2DDoubleArray; cdecl;
begin Result := Normalize(Mat, Alpha, Beta); end;

function exp_NormalizeMatF(var SE:Pointer; const Mat:T2DFloatArray; Alpha, Beta: Single): T2DFloatArray; cdecl;
begin Result := Normalize(Mat, Alpha, Beta); end;

function exp_NormalizeMatD(var SE:Pointer; const Mat:T2DDoubleArray; Alpha, Beta: Double): T2DDoubleArray; cdecl;
begin Result := Normalize(Mat, Alpha, Beta); end;

function exp_NormalizeMatE(var SE:Pointer; const Mat:T2DExtArray; Alpha, Beta: Extended): T2DExtArray; cdecl;
begin Result := Normalize(Mat, Alpha, Beta); end;


{-------| GetArea (matrix) |-------}
function exp_GetAreaB(var SE:Pointer; const Mat:T2DByteArray; X1,Y1,X2,Y2:Integer): T2DByteArray; cdecl;
begin Result := GetArea(Mat, X1,Y1,X2,Y2); end;

function exp_GetAreaI(var SE:Pointer; const Mat:T2DIntArray; X1,Y1,X2,Y2:Integer): T2DIntArray; cdecl;
begin Result := GetArea(Mat, X1,Y1,X2,Y2); end;

function exp_GetAreaF(var SE:Pointer; const Mat:T2DFloatArray; X1,Y1,X2,Y2:Integer): T2DFloatArray; cdecl;
begin Result := GetArea(Mat, X1,Y1,X2,Y2); end;

function exp_GetAreaD(var SE:Pointer; const Mat:T2DDoubleArray; X1,Y1,X2,Y2:Integer): T2DDoubleArray; cdecl;
begin Result := GetArea(Mat, X1,Y1,X2,Y2); end;

function exp_GetAreaE(var SE:Pointer; const Mat:T2DExtArray; X1,Y1,X2,Y2:Integer): T2DExtArray; cdecl;
begin Result := GetArea(Mat, X1,Y1,X2,Y2); end;


{-------| GetCols (matrix) |-------}
function exp_GetColsB(var SE:Pointer; const Mat:T2DByteArray; FromCol, ToCol:Int32): T2DByteArray; cdecl;
begin Result := GetCols(Mat, FromCol,ToCol); end;

function exp_GetColsI(var SE:Pointer; const Mat:T2DIntArray; FromCol, ToCol:Int32): T2DIntArray; cdecl;
begin Result := GetCols(Mat, FromCol,ToCol); end;

function exp_GetColsF(var SE:Pointer; const Mat:T2DFloatArray; FromCol, ToCol:Int32): T2DFloatArray; cdecl;
begin Result := GetCols(Mat, FromCol,ToCol); end;

function exp_GetColsD(var SE:Pointer; const Mat:T2DDoubleArray; FromCol, ToCol:Int32): T2DDoubleArray; cdecl;
begin Result := GetCols(Mat, FromCol,ToCol); end;

function exp_GetColsE(var SE:Pointer; const Mat:T2DExtArray; FromCol, ToCol:Int32): T2DExtArray; cdecl;
begin Result := GetCols(Mat, FromCol,ToCol); end;


{-------| GetRows (matrix) |-------}
function exp_GetRowsB(var SE:Pointer; const Mat:T2DByteArray; FromRow, ToRow:Int32): T2DByteArray; cdecl;
begin Result := GetRows(Mat, FromRow,ToRow); end;

function exp_GetRowsI(var SE:Pointer; const Mat:T2DIntArray; FromRow, ToRow:Int32): T2DIntArray; cdecl;
begin Result := GetRows(Mat, FromRow,ToRow); end;

function exp_GetRowsF(var SE:Pointer; const Mat:T2DFloatArray; FromRow, ToRow:Int32): T2DFloatArray; cdecl;
begin Result := GetRows(Mat, FromRow,ToRow); end;

function exp_GetRowsD(var SE:Pointer; const Mat:T2DDoubleArray; FromRow, ToRow:Int32): T2DDoubleArray; cdecl;
begin Result := GetRows(Mat, FromRow,ToRow); end;

function exp_GetRowsE(var SE:Pointer; const Mat:T2DExtArray; FromRow, ToRow:Int32): T2DExtArray; cdecl;
begin Result := GetRows(Mat, FromRow,ToRow); end;


{-------| FlipMat (matrix) |-------}
function exp_FlipMatB(var SE:Pointer; const Mat:T2DByteArray): T2DByteArray; cdecl;
begin Result := FlipMat(Mat); end;

function exp_FlipMatI(var SE:Pointer; const Mat:T2DIntArray): T2DIntArray; cdecl;
begin Result := FlipMat(Mat); end;

function exp_FlipMatF(var SE:Pointer; const Mat:T2DFloatArray): T2DFloatArray; cdecl;
begin Result := FlipMat(Mat); end;

function exp_FlipMatD(var SE:Pointer; const Mat:T2DDoubleArray): T2DDoubleArray; cdecl;
begin Result := FlipMat(Mat); end;

function exp_FlipMatE(var SE:Pointer; const Mat:T2DExtArray): T2DExtArray; cdecl;
begin Result := FlipMat(Mat); end;


{-------| CumSum (matrix) |-------}
function exp_CumSumB(var SE:Pointer; const Mat:T2DByteArray; axis:Int8): T2DIntArray; cdecl;
begin Result := CumSum(Mat, axis); end;

function exp_CumSumI(var SE:Pointer; const Mat:T2DIntArray; axis:Int8): T2DIntArray; cdecl;
begin Result := CumSum(Mat, axis); end;

function exp_CumSumF(var SE:Pointer; const Mat:T2DFloatArray; axis:Int8): T2DFloatArray; cdecl;
begin Result := CumSum(Mat, axis); end;

function exp_CumSumD(var SE:Pointer; const Mat:T2DDoubleArray; axis:Int8): T2DDoubleArray; cdecl;
begin Result := CumSum(Mat, axis); end;

function exp_CumSumE(var SE:Pointer; const Mat:T2DExtArray; axis:Int8): T2DExtArray; cdecl;
begin Result := CumSum(Mat, axis); end;


{-------| Pad (matrix) |-------}
function exp_PadMatB(var SE:Pointer; const Mat:T2DByteArray; pre, post:TPoint): T2DByteArray; cdecl;
begin Result := Pad(Mat, pre, post); end;

function exp_PadMatI(var SE:Pointer; const Mat:T2DIntArray; pre, post:TPoint): T2DIntArray; cdecl;
begin Result := Pad(Mat, pre, post); end;

function exp_PadMatF(var SE:Pointer; const Mat:T2DFloatArray; pre, post:TPoint): T2DFloatArray; cdecl;
begin Result := Pad(Mat, pre, post); end;

function exp_PadMatD(var SE:Pointer; const Mat:T2DDoubleArray; pre, post:TPoint): T2DDoubleArray; cdecl;
begin Result := Pad(Mat, pre, post); end;

function exp_PadMatE(var SE:Pointer; const Mat:T2DExtArray; pre, post:TPoint): T2DExtArray; cdecl;
begin Result := Pad(Mat, pre, post); end;
