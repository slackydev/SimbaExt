{!DOCTOPIC}{ 
  Matrix functions
} 

//-----------------------------------------------------------------------------
{!DOCREF} {
  @method: Integer matrix
  @desc: [hr]
}

{!DOCREF} {
  @method: function se.NewMatrixEx(W,H, Init:Integer): TIntMatrix;  
  @desc: Creates a new matrix and fills each indix with the given c'init'-value.
}
function SimbaExt.NewMatrixEx(W,H, Init:Integer): TIntMatrix;  
begin
  exp_NewMatrixEx(W,H, Init, Result);
end;

{!DOCREF} {
  @method: function se.NewMatrix(W,H:Integer): TIntMatrix;
  @desc: Creates a new matrix.
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
  @method: function TIntMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safly)
}
function TIntMatrix.Width(): Int32;
begin
  if Length(Self) > 0 then
    Result := Length(Self[0])
  else
    Result := 0;
end;


{!DOCREF} {
  @method: function TIntMatrix.Height(): Int32;
  @desc: Retruns the height of the matrix
}
function TIntMatrix.Height(): Int32;
begin
 Result := Length(Self);
end;


{!DOCREF} {
  @method: procedure TIntMatrix.InsertTPA(const TPA:TPointArray; Value:Integer);  
  @desc: Adds the points to the matrix with the given value.
}
procedure TIntMatrix.InsertTPA(const TPA:TPointArray; Value:Integer);  
begin
  exp_MatInsertTPA(Self, TPA, Value);
end;

{!DOCREF} {
  @method: function TIntMatrix.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;
  @desc:
    Converts a TPA to a matrix, where each element in the TPA will be given a value, and the rest will be the value of c'init'.
    Align must be true if you want to fir each point to the start of the matrix.
}
function SimbaExt.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;  
begin
  exp_TPAToMatrixEx(TPA,Init,Value,Align, Result);
end;

{!DOCREF} {
  @method: function se.MatrixFromTPA(const TPA:TPointArray; Value:Integer; Align:Boolean): TIntMatrix;
  @desc:
    Converts a TPA to a matrix, where each element in the TPA will be given a value, and the rest will be 0.
    Align must be true if you want to fir each point to the start of the matrix.

}
function SimbaExt.MatrixFromTPA(const TPA:TPointArray; Value:Integer; Align:Boolean): TIntMatrix;  
begin
  exp_TPAToMatrix(TPA, Value, Align, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.Normalize(Alpha, Beta:Integer): TIntMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TIntMatrix.Normalize(Alpha, Beta:Integer): TIntMatrix;  
begin
  exp_NormalizeMat(Self, Alpha, Beta, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetValues(const Indices:TPointArray): TIntArray; 
  @desc:
    This function will iterate through the TPA, and return all the elements
    of the given [i]indeices[/i].
    [code=pascal]
    var
      M:TIntMatrix;
    begin
      M := se.NewMatrixEx(100,100,531);
      M[10][10] := 100;
      M[10][13] := 29;
      WriteLn( M.GetValues([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TIntMatrix.GetValues(const Indices:TPointArray): TIntArray;  
begin
  exp_MatGetValues(Self, Indices, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetCol(Column:Integer): TIntArray;  
  @desc: Returns the column
}
function TIntMatrix.GetCol(Column:Integer): TIntArray;  
begin
  exp_MatGetCol(Self, Column, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetRow(Row:Integer): TIntArray;
  @desc: Returns the row
}
function TIntMatrix.GetRow(Row:Integer): TIntArray;  
begin
  exp_MatGetRow(Self, Row, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
  @desc: Returns all the wanted columns as a new matrix.
}
function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
begin
  exp_MatGetCols(Self, FromCol, ToCol, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
  @desc: Returns all the wanted rows as a new matrix.
}
function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
begin
  exp_MatGetRows(Self, FromRow, ToRow, Result);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
  @desc: Crops the matrix to the given box and returns that area.
}
function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
begin
  exp_MatGetArea(Self, X1,Y1,X2,Y2, Result);
end;

{!DOCREF} {
  @method: function se.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix; 
  @desc: Converts a TIntArray to a TIntMatrix of the given width, and height.
}
function SimbaExt.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix;  
begin
  exp_MatFromTIA(Arr, Width, Height, Result);
end;

{!DOCREF} {
  @method: procedure TIntMatrix.Padding(HPad,WPad:Integer);
  @desc: Adds a padding / border around all edges of the matrix.
}
procedure TIntMatrix.Padding(HPad,WPad:Integer);  
begin
  exp_PadMatrix(Self,HPad,WPad);
end;

{!DOCREF} {
  @method: function TIntMatrix.FloodFill(const Start:TPoint; EightWay:Boolean): TPointArray;
  @desc: Follows a value in the matrix and fills the result with all those indices.
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
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TIntMatrix.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
begin 
  exp_IndicesI(Self, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: function TIntMatrix.Indices(Value: Integer; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TIntMatrix.Indices(Value: Integer; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin 
  exp_IndicesExI(Self, B, Value, Comparator, Result); 
end;


{------------|  ArgMin/Max  |------------}
//argmax
{!DOCREF} {
  @method: function TIntMatrix.ArgMax(): TPoint; 
  @desc: ArgMax returns the index of the largest item
}
function TIntMatrix.ArgMax(): TPoint; 
begin 
  Result := exp_ArgMaxI(Self); 
end;


//ArgMaxEx
{!DOCREF} {
  @method: function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'
}
function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMaxExI(Self, B); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMin(): TPoint;
  @desc: ArgMin returns the index of the smallest item
}
function TIntMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMinI(Self); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'
}
function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMinExI(Self, B); 
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TIntMatrix.MinMax(var Min, Max:Integer);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TIntMatrix.MinMax(var Min, Max:Integer);
begin
  exp_MinMaxI(Self, Min, Max);
end;










//-----------------------------------------------------------------------------
{!DOCREF} {
  @method: Single percision floating point matrix
  @desc: [hr]
}


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
  @method: function TFloatMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safly)
}
function TFloatMatrix.Width(): Int32;
begin
  if Length(Self) > 0 then
    Result := Length(Self[0])
  else
    Result := 0;
end;


{!DOCREF} {
  @method: function TFloatMatrix.Height(): Int32;
  @desc: Retruns the height of the matrix
}
function TFloatMatrix.Height(): Int32;
begin
 Result := Length(Self);
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(): TPoint;
  @desc: ArgMax returns the index of the largest item
}
function TFloatMatrix.ArgMax(): TPoint;
begin Result := exp_ArgMaxF(Self); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExF(Self, B); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(): TPoint; 
  @desc: ArgMin returns the index of the smallest item.
}
function TFloatMatrix.ArgMin(): TPoint; 
begin Result := exp_ArgMinF(Self); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExF(Self, B);
end;


{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
begin exp_IndicesF(Self, Value, Comparator, Result); 
end;   


{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExF(Self, B, Value, Comparator, Result); 
end;  


{!DOCREF} {
  @method: procedure TFloatMatrix.MinMax(var Min, Max:Single);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TFloatMatrix.MinMax(var Min, Max:Single);
begin 
  exp_MinMaxF(Self, Min, Max); 
end;












//------------------------------------------------------------------------------------
{!DOCREF} {
  @method: Double percision floating point matrix
  @desc: [hr]
}


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
  @method: function TDobuleMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safly)
}
function TDoubleMatrix.Width(): Int32;
begin
  if Length(Self) > 0 then
    Result := Length(Self[0])
  else
    Result := 0;
end;


{!DOCREF} {
  @method: function TDlobleMatrix.Height(): Int32;
  @desc: Retruns the height of the matrix
}
function TDoubleMatrix.Height(): Int32;
begin
 Result := Length(Self);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
begin 
  exp_IndicesD(Self, Value, Comparator, Result); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TDoubleMatrix.Indices(Value: Double; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin 
  exp_IndicesExD(Self, B, Value, Comparator, Result); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(): TPoint;
  @desc:  ArgMax returns the index of the largest item
}
function TDoubleMatrix.ArgMax(): TPoint;
begin
  Result := exp_ArgMaxD(Self);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc:  ArgMax returns the index of the largest item within the given bounds c'B'
}
function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
begin
  Result := exp_ArgMaxExD(Self, B);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(): TPoint;
  @desc: ArgMin returns the index of the smallest item
}
function TDoubleMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMinD(Self); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'
}
function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMinExD(Self, B); 
end;



{!DOCREF} {
  @method: procedure TDoubleMatrix.MinMax(var Min, Max:Double);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TDoubleMatrix.MinMax(var Min, Max:Double);
begin
  exp_MinMaxD(Self, Min, Max);
end;








//-------------------------------------------------------------------------------------
{!DOCREF} {
  @method: Extended percision floating point matrix
  @desc: [hr]
}


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
  @method: function TExtMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safly)
}
function TExtMatrix.Width(): Int32;
begin
  if Length(Self) > 0 then
    Result := Length(Self[0])
  else
    Result := 0;
end;


{!DOCREF} {
  @method: function TExtMatrix.Height(): Int32;
  @desc: Retruns the height of the matrix
}
function TExtMatrix.Height(): Int32;
begin
 Result := Length(Self);
end;


{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
begin exp_IndicesE(Self, Value, Comparator, Result); 
end;


{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin 
  exp_IndicesExE(Self, B, Value, Comparator, Result); 
end;

{!DOCREF} {
  @method: procedure TExtMatrix.MinMax(var Min, Max:Extended);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TExtMatrix.MinMax(var Min, Max:Extended);
begin 
  exp_MinMaxE(Self, Min, Max); 
end;

{!DOCREF} {
  @method: function TExtMatrix.ArgMax(): TPoint; 
  @desc: ...
}
function TExtMatrix.ArgMax(): TPoint; 
begin 
  Result := exp_ArgMaxE(Self); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMaxExE(Self, B); 
end;



{!DOCREF} {
  @method: function TExtMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TExtMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMinE(Self);
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMinExE(Self, B); 
end;
