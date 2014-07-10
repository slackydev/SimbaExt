{!DOCTOPIC}{ 
  Matrix » TDoubleMatrix
} 

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
  @desc: Retruns the width of the matrix (safely)
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
  @method: function TDoubleMatrix.Get(const Indices:TPointArray): TDoubleArray; 
  @desc:
    Gets all the values at the given indices. If any of the points goes out
    of bounds, it will simply be ignored.
    [code=pascal]
    var 
      Matrix:TDoubleMatrix;
    begin
      Matrix.SetSize(100,100);
      Matrix[10][10] := 100;
      Matrix[10][13] := 29;
      WriteLn( Matrix.Get([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TDoubleMatrix.Get(const Indices:TPointArray): TDoubleArray;  
begin
  Result := exp_GetValues(Self, Indices);
end;


{!DOCREF} {
  @method: procedure TDoubleMatrix.Put(const TPA:TPointArray; Values:TDoubleArray);  
  @desc: Adds the points to the matrix with the given values.
}
procedure TDoubleMatrix.Put(const TPA:TPointArray; Values:TDoubleArray);  
begin
  exp_PutValues(Self, TPA, Values);
end;


{!DOCREF} {
  @method: procedure TDoubleMatrix.Put(const TPA:TPointArray; Value:Double); overload;  
  @desc: Adds the points to the matrix with the given value.
}
procedure TDoubleMatrix.Put(const TPA:TPointArray; Value:Double); overload; 
begin
  exp_PutValues(Self, TPA, TDoubleArray([Value]));
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Merge(): TDoubleArray;
  @desc: Merges the matrix is to a flat array of the same type.
}
function TDoubleMatrix.Merge(): TDoubleArray;
var i,s,wid: Int32;
begin
  S := 0;
  SetLength(Result, Self.Width()*Self.Height());
  Wid := Self.Width();
  for i:=0 to High(Self) do
  begin
    MemMove(Self[i][0], Result[S], Wid*SizeOf(Double));
    S := S + Wid;
  end; 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Sum(): Double;
  @desc: Returns the sum of the matrix
}
function TDoubleMatrix.Sum(): Double;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;




{!DOCREF} {
  @method: function TDoubleMatrix.Mean(): Double;
  @desc: Returns the mean of the matrix
}
function TDoubleMatrix.Mean(): Double;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Stdev(): Double;
  @desc: Returns the standard deviation of the matrix
}
function TDoubleMatrix.Stdev(): Double;
var
  x,y,i,W,H:Int32;
  avg:Double;
  square:TDoubleArray;
begin
  W := Self.Width() - 1;
  H := Self.Height() - 1;
  avg := Self.Mean();
  SetLength(square,Self.Width()*Self.Height());
  i := -1;
  for y:=0 to H do
    for x:=0 to W do
      Square[inc(i)] := Sqr(Self[y][x] - avg);
  Result := Sqrt(square.Mean());
end;


{!DOCREF} {
  @method: function TDoubleMatrix.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the matrix. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TDoubleMatrix.Variance(): Double;
var
  avg:Double;
  x,y,w,h:Int32;
begin
  W := Self.Width() - 1;
  H := Self.Height() - 1;

  avg := Self.Mean();
  for y:=0 to H do
    for x:=0 to W do
      Result := Result + Sqr(Self[y][x] - avg);
  Result := Result / ((W+1) * (H+1));
end; 


{!DOCREF} {
  @method: function TDoubleMatrix.Mode(Eps:Single=0.0000001): Double;
  @desc:
    Returns the sample mode of the matrix, which is the most frequently occurring value in the matrix.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TDoubleMatrix.Mode(Eps:Double=0.0000001): Double;
begin
  Result := Self.Merge().Mode(Eps);
end;



{------------|  Indices  |------------}
{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'TPA := Matrix.Indices(10, __LT__)' would return where all the items which are less then 10 is.
}
function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
begin 
  Result := exp_Indices(Self, Value, Comparator); 
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
  Result := exp_Indices(Self, B, Value, Comparator); 
end;


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(): TPoint;
  @desc:  ArgMax returns the index of the largest item
}
function TDoubleMatrix.ArgMax(): TPoint;
begin
  Result := exp_ArgMax(Self);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TDoubleMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc:  ArgMax returns the index of the largest item within the given bounds `B`
}
function TDoubleMatrix.ArgMax(B:TBox): TPoint; overload;
begin
  Result := exp_ArgMax(Self, B);
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(): TPoint;
  @desc: ArgMin returns the index of the smallest item
}
function TDoubleMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMin(Self); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TDoubleMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds `B`
}
function TDoubleMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMin(Self, B); 
end;



{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TDoubleMatrix.VarMax(): Double;
  @desc: Returns the largest item
}
function TDoubleMatrix.VarMax(): Double;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TDoubleMatrix.VarMax(Count:Int32): TDoubleArray; overload;
  @desc: Returns the n-largest elements
}
function TDoubleMatrix.VarMax(Count:Int32): TDoubleArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.VarMax(B:TBox): Double; overload;
  @desc: Returns the largest item within the given bounds `B`
}
function TDoubleMatrix.VarMax(B:TBox): Double; overload;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TDoubleMatrix.VarMin(): Double;
  @desc: Returns the the smallest item
}
function TDoubleMatrix.VarMin(): Double;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TDoubleMatrix.VarMin(Count:Int32): TDoubleArray; overload;
  @desc: Returns the n-smallest elements
}
function TDoubleMatrix.VarMin(Count:Int32): TDoubleArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TDoubleMatrix.VarMin(B:TBox): Double; overload;
  @desc: Returns the smallest item within the given bounds `B`
}
function TDoubleMatrix.VarMin(B:TBox): Double; overload;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;



{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TDoubleMatrix.MinMax(var Min, Max:Double);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TDoubleMatrix.MinMax(var Min, Max:Double);
begin
  exp_MinMax(Self, Min, Max);
end;


{------------|  CombineMatrix  |------------}
{!DOCREF} {
  @method: function TDoubleMatrix.Combine(Other:TDoubleMatrix; OP:Char='+'): TDoubleMatrix;
  @desc: 
    Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
    
    [code=pascal]
    var Mat:TDoubleMatrix;
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
function TDoubleMatrix.Combine(Other:TDoubleMatrix; OP:Char='+'): TDoubleMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;


{------------|  Normalize (Matrix)  |------------}

{!DOCREF} {
  @method: function TDoubleMatrix.Normalize(Alpha, Beta:Double): TDoubleMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TDoubleMatrix.Normalize(Alpha, Beta:Double): TDoubleMatrix;  
begin
  Result := exp_Normalize(Self, Alpha, Beta);
end;