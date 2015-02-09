{!DOCTOPIC}{ 
  Matrix » TExtMatrix
} 

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
  @method: function TExtMatrix.Shape(): TSize2D;
  @desc: Retruns the size of the matrix
}
function TExtMatrix.Shape(): TSize2D;
begin
  if Length(Self) = 0 then Exit(TSize2D([0,0]));
  Result := [Length(Self), Length(Self[0])];
end;


{!DOCREF} {
  @method: function TExtMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safely)
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
  @method: function TExtMatrix.Size(): Int32;
  @desc: 
    Returns the product of the array’s dimensions.
    IE:
    [code=pascal]
    SetLength(x,10,4);
    WriteLn(x.size());
    [/code]
    >> `40`
}
function TExtMatrix.Size(): Int32;
begin
  if Length(Self) = 0 then Exit(0);
  Result := Length(Self)*Length(Self[0]);
end;


{!DOCREF} {
  @method: function TExtMatrix.Get(const Indices:TPointArray): TExtArray 
  @desc:
    Gets all the values at the given indices. If any of the points goes out
    of bounds, it will simply be ignored.
    [code=pascal]
    var 
      Matrix:TExtMatrix;
    begin
      Matrix.SetSize(100,100);
      Matrix[10][10] := 100;
      Matrix[10][13] := 29;
      WriteLn( Matrix.Get([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TExtMatrix.Get(const Indices:TPointArray): TExtArray;  
begin
  Result := se.GetData(Self, Indices);
end;


{!DOCREF} {
  @method: procedure TExtMatrix.Put(const TPA:TPointArray; Values:TExtArray);  
  @desc: Adds the points to the matrix with the given values.
}
procedure TExtMatrix.Put(const TPA:TPointArray; Values:TExtArray);  
begin
  se.SetData(Self, TPA, Values);
end;


{!DOCREF} {
  @method: procedure TExtMatrix.Put(const TPA:TPointArray; Value:Extended); overload;  
  @desc: Adds the points to the matrix with the given value.
}
procedure TExtMatrix.Put(const TPA:TPointArray; Value:Extended); overload; 
begin
  se.SetData(Self, TPA, TExtArray([Value]));
end;


{!DOCREF} {
  @method: function TExtMatrix.Merge(): TExtArray;
  @desc: Merges the matrix is to a flat array of the same type.
}
function TExtMatrix.Merge(): TExtArray;
var i,s,wid: Int32;
begin
  S := 0;
  SetLength(Result, Self.Width()*Self.Height());
  Wid := Self.Width();
  for i:=0 to High(Self) do
  begin
    MemMove(Self[i][0], Result[S], Wid*SizeOf(Extended));
    S := S + Wid;
  end; 
end;


{!DOCREF} {
  @method: function TExtMatrix.Sum(): Extended;
  @desc: Returns the sum of the matrix
}
function TExtMatrix.Sum(): Extended;
begin
  Result := se.Sum(Self.Merge());
end;




{!DOCREF} {
  @method: function TExtMatrix.Mean(): Extended;
  @desc: Returns the mean of the matrix
}
function TExtMatrix.Mean(): Extended;
begin
  Result := se.Mean(Self.Merge());
end;


{!DOCREF} {
  @method: function TExtMatrix.Stdev(): Extended;
  @desc: Returns the standard deviation of the matrix
}
function TExtMatrix.Stdev(): Extended;
begin
  Result := se.Stdev(Self.Merge());
end;


{!DOCREF} {
  @method: function TExtMatrix.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the matrix. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TExtMatrix.Variance(): Extended;
begin
  Result := se.Variance(Self.Merge());
end; 


{!DOCREF} {
  @method: function TExtMatrix.Mode(): Extended;
  @desc:
    Returns the sample mode of the matrix, which is the most frequently occurring value in the matrix.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TExtMatrix.Mode(): Extended;
begin
  Result := se.Mode(Self.Merge());
end;




{------------|  GetArea, GetCols, GetRows  |------------}
{!DOCREF} {
  @method: function TExtMatrix.Area(X1,Y1,X2,Y2:Int32): TExtMatrix;  
  @desc: Crops the matrix to the given box and returns that area.
}
function TExtMatrix.Area(X1,Y1,X2,Y2:Int32): TExtMatrix;  
begin
  Result := se.GetArea(Self, X1,Y1,X2,Y2);
end;

{!DOCREF} {
  @method: function TExtMatrix.Cols(FromCol, ToCol:Integer): TExtMatrix;  
  @desc: Returns all the wanted columns as a new matrix.
}
function TExtMatrix.Cols(FromCol, ToCol:Integer): TExtMatrix;  
begin
  Result := se.GetCols(Self, FromCol, ToCol);
end;

{!DOCREF} {
  @method: function TExtMatrix.Rows(FromRow, ToRow:Integer): TExtMatrix;  
  @desc: Returns all the wanted rows as a new matrix.
}
function TExtMatrix.Rows(FromRow, ToRow:Integer): TExtMatrix;  
begin
  Result :=  se.GetRows(Self, FromRow, ToRow);
end;



{------------|  FlipMat  |------------}
{!DOCREF} {
  @method: function TExtMatrix.FlipMat(): TExtMatrix;  
  @desc: 
    Order of the items in the array is flipped, meaning x becomes y, and y becomes x.
    Example:
    [code=pascal]
    var
      x:TExtMatrix;
    begin
      x := [[1,2,3],[1,2,3],[1,2,3]];
      WriteLn(x.Flip());
    end.  
    [/code]
    >> `[[1, 1, 1], [2, 2, 2], [3, 3, 3]]`
}
function TExtMatrix.Flip(): TExtMatrix;  
begin
  Result :=  se.Flip(Self);
end;



{------------|  Indices  |------------}
{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; const Comparator:EComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'TPA := Matrix.Indices(10, CMP_LT)' would return where all the items which are less then 10 is.
}
function TExtMatrix.Indices(Value: Extended; const Comparator:EComparator): TPointArray;
begin 
  Result := se.Indices(Self, Value, Comparator); 
end;


{!DOCREF} {
  @method: function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:EComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, CMP_LT)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TExtMatrix.Indices(Value: Extended; B:TBox; const Comparator:EComparator): TPointArray; overload;
begin 
  Result := se.Indices(Self, B, Value, Comparator); 
end;


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TExtMatrix.ArgMax(): TPoint; 
  @desc: ...
}
function TExtMatrix.ArgMax(): TPoint; 
begin 
  Result := se.ArgMax(Self); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TExtMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := se.ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
begin 
  Result := se.ArgMax(Self, B); 
end;



{!DOCREF} {
  @method: function TExtMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TExtMatrix.ArgMin(): TPoint;
begin 
  Result := se.ArgMin(Self);
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TExtMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := se.ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := se.ArgMin(Self, B); 
end;


{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TExtMatrix.VarMax(): Extended;
  @desc: Returns the largest element
}
function TExtMatrix.VarMax(): Extended;
var tmp:TPoint;
begin
  tmp := se.ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
  @desc: Returns the n-largest elements
}
function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
begin 
  Result := se.VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(B:TBox): Extended; overload;
  @desc: Returns the largest element within the given bounds `B`
}
function TExtMatrix.VarMax(B:TBox): Extended; overload;
var tmp:TPoint;
begin
  tmp := se.ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMin(): Extended;
  @desc: Returns the the smallest element
}
function TExtMatrix.VarMin(): Extended;
var tmp:TPoint;
begin 
  tmp := se.ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
  @desc: Returns the n-smallest elements
}
function TExtMatrix.VarMin(Count:Int32): TExtArray; overload;
begin 
  Result := se.VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMin(B:TBox): Extended; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TExtMatrix.VarMin(B:TBox): Extended; overload;
var tmp:TPoint;
begin 
  tmp := se.ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TExtMatrix.MinMax(var Min, Max:Extended);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TExtMatrix.MinMax(var Min, Max:Extended);
begin
  se.MinMax(Self, Min, Max);
end;


{------------|  CombineMatrix  |------------}
{!DOCREF} {
  @method: function TExtMatrix.Combine(Other:TExtMatrix; OP:Char='+'): TExtMatrix;
  @desc: 
    Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
    
    [code=pascal]
    var Mat:TExtMatrix;
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
function TExtMatrix.Combine(Other:TExtMatrix; OP:Char='+'): TExtMatrix;
begin 
  Result := se.CombineMatrix(Self, Other, OP); 
end;


{------------|  Normalize (Matrix)  |------------}

{!DOCREF} {
  @method: function TExtMatrix.Normalize(Alpha, Beta:Extended): TExtMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TExtMatrix.Normalize(Alpha, Beta:Extended): TExtMatrix;  
begin
  Result := se.Normalize(Self, Alpha, Beta);
end;
