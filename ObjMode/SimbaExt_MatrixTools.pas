{!DOCTOPIC}{ 
  Matrix functions
} 

{!DOCREF} {
  @method: function se.NewMatrixEx(W,H, Init:Integer): TIntMatrix;  
  @desc: ...
}
function SimbaExt.NewMatrixEx(W,H, Init:Integer): TIntMatrix;  
begin
  exp_NewMatrixEx(W,H, Init, Result);
end;

{!DOCREF} {
  @method: function se.NewMatrix(W,H:Integer): TIntMatrix;
  @desc: ...
}
function SimbaExt.NewMatrix(W,H:Integer): TIntMatrix;  
begin
  exp_NewMatrix(W,H, Result);
end;


{!DOCREF} {
  @method: procedure TIntMatrix.SetSize(Height,Width:Int32);
  @desc:
    Sets the size (width and height) of the matrix.
    Same as SetLength(Matrix, H,W);
}
procedure TIntMatrix.SetSize(Height,Width:Int32);
begin
  SetLength(Self, Height,Width);
end;


{!DOCREF} {
  @method: procedure TFloatMatrix.SetSize(Height,Width:Int32);
  @desc:
    Sets the size (width and height) of the matrix.
    Same as SetLength(Matrix, H,W);
}
procedure TFloatMatrix.SetSize(Height,Width:Int32);
begin
  SetLength(Self, Height,Width);
end;


{!DOCREF} {
  @method: procedure TDoubleMatrix.SetSize(Height,Width:Int32);
  @desc:
    Sets the size (width and height) of the matrix.
    Same as SetLength(Matrix, H,W);
}
procedure TDoubleMatrix.SetSize(Height,Width:Int32);
begin
  SetLength(Self, Height,Width);
end;


{!DOCREF} {
  @method: procedure TExtMatrix.SetSize(Height,Width:Int32);
  @desc:
    Sets the size (width and height) of the matrix.
    Same as SetLength(Matrix, H,W);
}
procedure TExtMatrix.SetSize(Height,Width:Int32);
begin
  SetLength(Self, Height,Width);
end;


{!DOCREF} {
  @method: procedure TIntMatrix.InsertTPA(const TPA:TPointArray; Value:Integer);  
  @desc: ...
}
procedure TIntMatrix.InsertTPA(const TPA:TPointArray; Value:Integer);  
begin
  exp_MatInsertTPA(Self, TPA, Value);
end;

{!DOCREF} {
  @method: function TIntMatrix.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;
  @desc: ...
}
function SimbaExt.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;  
begin
  exp_TPAToMatrixEx(TPA,Init,Value,Align, Result);
end;

{!DOCREF} {
  @method: function se.MatrixFromTPA(const TPA:TPointArray; Value:Integer; Align:Boolean): TIntMatrix;
  @desc: ...
}
function SimbaExt.MatrixFromTPA(const TPA:TPointArray; Value:Integer; Align:Boolean): TIntMatrix;  
begin
  exp_TPAToMatrix(TPA, Value, Align, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.Normalize(Alpha, Beta:Integer): TIntMatrix;  
  @desc: ...
}
function TIntMatrix.Normalize(Alpha, Beta:Integer): TIntMatrix;  
begin
  exp_NormalizeMat(Self, Alpha, Beta, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetValues(const Indices:TPointArray): TIntArray; 
  @desc: ...
}
function TIntMatrix.GetValues(const Indices:TPointArray): TIntArray;  
begin
  exp_MatGetValues(Self, Indices, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetCol(Column:Integer): TIntArray;  
  @desc: ...
}
function TIntMatrix.GetCol(Column:Integer): TIntArray;  
begin
  exp_MatGetCol(Self, Column, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetRow(Row:Integer): TIntArray;
  @desc: ...
}
function TIntMatrix.GetRow(Row:Integer): TIntArray;  
begin
  exp_MatGetRow(Self, Row, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
  @desc: ...
}
function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
begin
  exp_MatGetCols(Self, FromCol, ToCol, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
  @desc: ...
}
function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
begin
  exp_MatGetRows(Self, FromRow, ToRow, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
  @desc: ...
}
function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
begin
  exp_MatGetArea(Self, X1,Y1,X2,Y2, Result);
end;

{!DOCREF} {
  @method: function se.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix; 
  @desc: ...
}
function SimbaExt.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix;  
begin
  exp_MatFromTIA(Arr, Width, Height, Result);
end;

{!DOCREF} {
  @method: procedure TIntMatrix.Padding(HPad,WPad:Integer);
  @desc: ...
}
procedure TIntMatrix.Padding(HPad,WPad:Integer);  
begin
  exp_PadMatrix(Self,HPad,WPad);
end;

{!DOCREF} {
  @method: function TIntMatrix.FloodFill(const Start:TPoint; EightWay:Boolean): TPointArray;
  @desc: ...
}
function TIntMatrix.FloodFill(const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillMatrix(Self, Start, EightWay, Result);
end;




{*=========================================================================================|
| Matrix/xxx.pas                                                                           |
|=========================================================================================*}
{!DOCREF} {
  @method: function TIntMatrix.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
  @desc: ...
}
function TIntMatrix.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
begin exp_IndicesI(Self, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
  @desc: ...
}
function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
begin exp_IndicesE(Self, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
  @desc: ...
}
function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
begin exp_IndicesD(Self, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
  @desc: ...
}
function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
begin exp_IndicesF(Self, Value, Comparator, Result); 
end;    

{!DOCREF} {
  @method: function TIntMatrix.Indices(Value: Integer; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc: ...
}
function TIntMatrix.Indices(Value: Integer; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExI(Self, B, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc: ...
}
function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExE(Self, B, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc: ...
}
function TDoubleMatrix.Indices(Value: Double; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExD(Self, B, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc: ...
}
function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExF(Self, B, Value, Comparator, Result); 
end;  


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TIntMatrix.MinMax(var Min, Max:Integer); 
  @desc: ...
}
procedure TIntMatrix.MinMax(var Min, Max:Integer); 
begin exp_MinMaxI(Self, Min, Max); 
end;

{!DOCREF} {
  @method: procedure TExtMatrix.MinMax(var Min, Max:Extended);
  @desc: ...
}
procedure TExtMatrix.MinMax(var Min, Max:Extended);
begin exp_MinMaxE(Self, Min, Max); 
end;

{!DOCREF} {
  @method: procedure TDoubleMatrix.MinMax(var Min, Max:Double);
  @desc: ...
}
procedure TDoubleMatrix.MinMax(var Min, Max:Double);
begin exp_MinMaxD(Self, Min, Max); 
end;

{!DOCREF} {
  @method: procedure TFloatMatrix.MinMax(var Min, Max:Single);
  @desc: ...
}
procedure TFloatMatrix.MinMax(var Min, Max:Single);
begin exp_MinMaxF(Self, Min, Max); 
end;


{------------|  ArgMin/Max  |------------}
//argmax
{!DOCREF} {
  @method: function TIntMatrix.ArgMax(): TPoint; 
  @desc: ...
}
function TIntMatrix.ArgMax(): TPoint; 
begin Result := exp_ArgMaxI(Self); 
end;

{!DOCREF} {
  @method: function TExtMatrix.ArgMax(): TPoint; 
  @desc: ...
}
function TExtMatrix.ArgMax(): TPoint; 
begin Result := exp_ArgMaxE(Self); 
end;

{!DOCREF} {
  @method: function TDoubleMatrix.ArgMaxD(): TPoint;
  @desc: ...
}
function TDoubleMatrix.ArgMaxD(): TPoint;
begin Result := exp_ArgMaxD(Self); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(): TPoint;
  @desc: ...
}
function TFloatMatrix.ArgMax(): TPoint;
begin Result := exp_ArgMaxF(Self); 
end;

//ArgMaxEx
{!DOCREF} {
  @method: function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExI(Self, B); 
end;

{!DOCREF} {
  @method: function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExE(Self, B); 
end;

{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExD(Self, B); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExF(Self, B); 
end;

//ArgMin
{!DOCREF} {
  @method: function TIntMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TIntMatrix.ArgMin(): TPoint;
begin Result := exp_ArgMinI(Self); 
end;

{!DOCREF} {
  @method: function TExtMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TExtMatrix.ArgMin(): TPoint;
begin Result := exp_ArgMinE(Self);
end;

{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TDoubleMatrix.ArgMin(): TPoint;
begin Result := exp_ArgMinD(Self); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(): TPoint; 
  @desc: ...
}
function TFloatMatrix.ArgMin(): TPoint; 
begin Result := exp_ArgMinF(Self); 
end; 

//ArgMinEx
{!DOCREF} {
  @method: function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExI(Self, B); 
end;

{!DOCREF} {
  @method: function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExE(Self, B); 
end;

{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExD(Self, B); 
end;

{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExF(Self, B); 
end;
