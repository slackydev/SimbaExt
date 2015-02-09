{!DOCTOPIC}{ 
  Matrix » TFloatMatrix
} 

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
  @method: function TFloatMatrix.Shape(): TSize2D;
  @desc: Retruns the size of the matrix
}
function TFloatMatrix.Shape(): TSize2D;
begin
  if Length(Self) = 0 then Exit(TSize2D([0,0]));
  Result := [Length(Self), Length(Self[0])];
end;


{!DOCREF} {
  @method: function TFloatMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safly)
}
function TFloatMatrix.Width(): Int32;
begin
  if (Length(Self) > 0) and (@Self <> nil) then
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
  @method: function TFloatMatrix.Size(): Int32;
  @desc: 
    Returns the product of the array’s dimensions.
    IE:
    [code=pascal]
    SetLength(x,10,4);
    WriteLn(x.size());
    [/code]
    >> `40`
}
function TFloatMatrix.Size(): Int32;
begin
  if Length(Self) = 0 then Exit(0);
  Result := Length(Self)*Length(Self[0]);
end;


{!DOCREF} {
  @method: function TFloatMatrix.Get(const Indices:TPointArray): TFloatArray; 
  @desc:
    Gets all the values at the given indices. If any of the points goes out
    of bounds, it will simply be ignored.
    [code=pascal]
    var 
      Matrix:TFloatMatrix;
    begin
      Matrix.SetSize(100,100);
      Matrix[10][10] := 100;
      Matrix[10][13] := 29;
      WriteLn( Matrix.Get([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TFloatMatrix.Get(const Indices:TPointArray): TFloatArray;  
begin
  Result := se.GetData(Self, Indices);
end;


{!DOCREF} {
  @method: procedure TFloatMatrix.Put(const TPA:TPointArray; Values:TFloatArray);  
  @desc: Adds the points to the matrix with the given values.
}
procedure TFloatMatrix.Put(const TPA:TPointArray; Values:TFloatArray);  
begin
  se.SetData(Self, TPA, Values);
end;


{!DOCREF} {
  @method: procedure TFloatMatrix.Put(const TPA:TPointArray; Value:Single); overload;  
  @desc: Adds the points to the matrix with the given value.
}
procedure TFloatMatrix.Put(const TPA:TPointArray; Value:Single); overload; 
begin
  se.SetData(Self, TPA, TFloatArray([Value]));
end;



{!DOCREF} {
  @method: function TFloatMatrix.Merge(): TFloatArray;
  @desc: Merges the matrix is to a flat array of the same type.
}
function TFloatMatrix.Merge(): TFloatArray;
var i,s,wid: Int32;
begin
  S := 0;
  SetLength(Result, Self.Width()*Self.Height());
  Wid := Self.Width();
  for i:=0 to High(Self) do
  begin
    MemMove(Self[i][0], Result[S], Wid*SizeOf(Single));
    S := S + Wid;
  end; 
end;


{!DOCREF} {
  @method: function TFloatMatrix.Sum(): Double;
  @desc: Returns the sum of the matrix
}
function TFloatMatrix.Sum(): Double;
begin
  Result := se.Sum(Self.Merge());
end;




{!DOCREF} {
  @method: function TFloatMatrix.Mean(): Double;
  @desc: Returns the mean of the matrix
}
function TFloatMatrix.Mean(): Double;
begin
  Result := se.Mean(Self.Merge());
end;


{!DOCREF} {
  @method: function TFloatMatrix.Stdev(): Double;
  @desc: Returns the standard deviation of the matrix
}
function TFloatMatrix.Stdev(): Double;
begin
  Result := se.Stdev(Self.Merge());
end;


{!DOCREF} {
  @method: function TFloatMatrix.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the matrix. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TFloatMatrix.Variance(): Double;
begin
  Result := se.Variance(Self.Merge());
end;


{!DOCREF} {
  @method: function TFloatMatrix.Mode(): Single;
  @desc:
    Returns the sample mode of the matrix, which is the most frequently occurring value in the matrix.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TFloatMatrix.Mode(): Single;
begin
  Result := se.Mode(Self.Merge());
end;




{------------|  GetArea, GetCols, GetRows  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.Area(X1,Y1,X2,Y2:Int32): TFloatMatrix;  
  @desc: Crops the matrix to the given box and returns that area.
}
function TFloatMatrix.Area(X1,Y1,X2,Y2:Int32): TFloatMatrix;  
begin
  Result := se.GetArea(Self, X1,Y1,X2,Y2);
end;

{!DOCREF} {
  @method: function TFloatMatrix.Cols(FromCol, ToCol:Integer): TFloatMatrix;  
  @desc: Returns all the wanted columns as a new matrix.
}
function TFloatMatrix.Cols(FromCol, ToCol:Integer): TFloatMatrix;  
begin
  Result := se.GetCols(Self, FromCol, ToCol);
end;

{!DOCREF} {
  @method: function TFloatMatrix.Rows(FromRow, ToRow:Integer): TFloatMatrix;  
  @desc: Returns all the wanted rows as a new matrix.
}
function TFloatMatrix.Rows(FromRow, ToRow:Integer): TFloatMatrix;  
begin
  Result :=  se.GetRows(Self, FromRow, ToRow);
end;



{------------|  FlipMat  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.FlipMat(): TFloatMatrix;  
  @desc: 
    Order of the items in the array is flipped, meaning x becomes y, and y becomes x.
    Example:
    [code=pascal]
    var
      x:TFloatMatrix;
    begin
      x := [[1,2,3],[1,2,3],[1,2,3]];
      WriteLn(x.Flip());
    end.  
    [/code]
    >> `[[1, 1, 1], [2, 2, 2], [3, 3, 3]]`
}
function TFloatMatrix.Flip(): TFloatMatrix;  
begin
  Result :=  se.Flip(Self);
end;



{------------|  Indices  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; const Comparator:EComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: `TPA := Matrix.Indices(10, CMP_LT)` would return where all the items which are less then 10 is.
}
function TFloatMatrix.Indices(Value: Single; const Comparator:EComparator): TPointArray;
begin
  try
    Result := se.Indices(Self, Value, Comparator);
  except;
  end;
end;   


{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:EComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, CMP_LT)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:EComparator): TPointArray; overload;
begin 
  Result := se.Indices(Self, B, Value, Comparator); 
end;  


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(): TPoint;
  @desc: ArgMax returns the index of the largest item
}
function TFloatMatrix.ArgMax(): TPoint;
begin
  Result := se.ArgMax(Self)
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TFloatMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := se.ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
begin
  Result := se.ArgMax(Self, B);
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(): TPoint; 
  @desc: ArgMin returns the index of the smallest item.
}
function TFloatMatrix.ArgMin(): TPoint; 
begin
  if Length(Self) > 0 then
    Result := se.ArgMin(Self);
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TFloatMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := se.ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
begin
  if Length(Self) > 0 then
    Result := se.ArgMin(Self, B);
end;

{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.VarMax(): Single;
  @desc:  ArgMax returns the largest item
}
function TFloatMatrix.VarMax(): Single;
var tmp:TPoint;
begin
  tmp := se.ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMax(Count:Int32): TFloatArray; overload;
  @desc: Returns the n-largest elements
}
function TFloatMatrix.VarMax(Count:Int32): TFloatArray; overload;
begin 
  Result := se.VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMax(B:TBox): Single; overload;
  @desc:  ArgMax returns the largest item within the given bounds `B`
}
function TFloatMatrix.VarMax(B:TBox): Single; overload;
var tmp:TPoint;
begin
  tmp := se.ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(): Single;
  @desc: ArgMin returns the the smallest item
}
function TFloatMatrix.VarMin(): Single;
var tmp:TPoint;
begin 
  tmp := se.ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(Count:Int32): TFloatArray; overload;
  @desc: Returns the n-smallest elements
}
function TFloatMatrix.VarMin(Count:Int32): TFloatArray; overload;
begin 
  Result := se.VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(B:TBox): Single; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TFloatMatrix.VarMin(B:TBox): Single; overload;
var tmp:TPoint;
begin 
  tmp := se.ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TFloatMatrix.MinMax(var Min, Max:Single);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TFloatMatrix.MinMax(var Min, Max:Single);
begin 
  se.MinMax(Self, Min, Max); 
end;


{------------|  CombineMatrix  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.Combine(Other:TFloatMatrix; OP:Char='+'): TFloatMatrix;
  @desc: 
    Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
    
    [code=pascal]
    var Mat:TFloatMatrix;
    begin
      SetLength(Mat, 3);
      Mat[0] := [1,1,1];
      Mat[1] := [2,2,2];
      Mat[2] := [3,3,3];
      WriteLn( Mat.Combine(Mat, '*') );
    end. 
    [/code]
    
    Outputs:
    >>> `[[1, 1, 1], [4, 4, 4], [9, 9, 9]]`
}
function TFloatMatrix.Combine(Other:TFloatMatrix; OP:Char='+'): TFloatMatrix;
begin 
  Result := se.CombineMatrix(Self, Other, OP); 
end;


{------------|  Normalize (Matrix)  |------------}

{!DOCREF} {
  @method: function TFloatMatrix.Normalize(Alpha, Beta:Single): TFloatMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TFloatMatrix.Normalize(Alpha, Beta:Single): TFloatMatrix;  
begin
  Result := se.Normalize(Self, Alpha, Beta);
end;
