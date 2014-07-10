{!DOCTOPIC}{ 
  Matrix » TByteMatrix
} 

{!DOCREF} {
  @method: Single percision floating point matrix
  @desc: [hr]
}


{!DOCREF} {
  @method: procedure TByteMatrix.SetSize(Height,Width:Int32);
  @desc:
    Sets the size (width and height) of the matrix.
    Same as SetLength(Matrix, H,W);
}
procedure TByteMatrix.SetSize(Height,Width:Int32);
begin
  SetLength(Self, Height,Width);
end;


{!DOCREF} {
  @method: function TByteMatrix.Width(): Int32;
  @desc: Retruns the width of the matrix (safely)
}
function TByteMatrix.Width(): Int32;
begin
  if Length(Self) > 0 then
    Result := Length(Self[0])
  else
    Result := 0;
end;


{!DOCREF} {
  @method: function TByteMatrix.Height(): Int32;
  @desc: Retruns the height of the matrix
}
function TByteMatrix.Height(): Int32;
begin
 Result := Length(Self);
end;


{!DOCREF} {
  @method: function TByteMatrix.Get(const Indices:TPointArray): TByteArray 
  @desc:
    Gets all the values at the given indices. If any of the points goes out
    of bounds, it will simply be ignored.
    [code=pascal]
    var 
      Matrix:TByteMatrix;
    begin
      Matrix.SetSize(100,100);
      Matrix[10][10] := 100;
      Matrix[10][13] := 29;
      WriteLn( Matrix.Get([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TByteMatrix.Get(const Indices:TPointArray): TByteArray;  
begin
  Result := exp_GetValues(Self, Indices);
end;


{!DOCREF} {
  @method: procedure TByteMatrix.Put(const TPA:TPointArray; Values:TByteArray);  
  @desc: Adds the points to the matrix with the given values.
}
procedure TByteMatrix.Put(const TPA:TPointArray; Values:TByteArray);  
begin
  exp_PutValues(Self, TPA, Values);
end;


{!DOCREF} {
  @method: procedure TByteMatrix.Put(const TPA:TPointArray; Value:Byte); overload;  
  @desc: Adds the points to the matrix with the given value.
}
procedure TByteMatrix.Put(const TPA:TPointArray; Value:Byte); overload; 
begin
  exp_PutValues(Self, TPA, TByteArray([Value]));
end;



{!DOCREF} {
  @method: function TByteMatrix.Merge(): TByteArray;
  @desc: Merges the matrix is to a flat array of the same type.
}
function TByteMatrix.Merge(): TByteArray;
var i,s,wid: Int32;
begin
  S := 0;
  SetLength(Result, Self.Width()*Self.Height());
  Wid := Self.Width();
  for i:=0 to High(Self) do
  begin
    MemMove(Self[i][0], Result[S], Wid*SizeOf(Byte));
    S := S + Wid;
  end; 
end;


{!DOCREF} {
  @method: function TByteMatrix.Sum(): Int64;
  @desc: Returns the sum of the matrix
}
function TByteMatrix.Sum(): Int64;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;




{!DOCREF} {
  @method: function TByteMatrix.Mean(): Double;
  @desc: Returns the mean of the matrix
}
function TByteMatrix.Mean(): Double;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function TByteMatrix.Stdev(): Single;
  @desc: Returns the standard deviation of the matrix
}
function TByteMatrix.Stdev(): Single;
var
  x,y,W,H,i:Int32;
  avg:Single;
  square:TFloatArray;
begin
  W := Self.Width() - 1;
  H := Self.Height() - 1;
  avg := Self.Mean();
  SetLength(square,Self.Width()*Self.Height());
  i:=-1;
  for y:=0 to H do
    for x:=0 to W do
      Square[inc(i)] := Sqr(Self[y][x] - avg);
  Result := Sqrt(square.Mean());
end;


{!DOCREF} {
  @method: function TByteMatrix.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the matrix. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TByteMatrix.Variance(): Double;
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
  @method: function TByteMatrix.Mode(): Byte;
  @desc:
    Returns the sample mode of the matrix, which is the most frequently occurring value in the matrix.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TByteMatrix.Mode(): Byte;
begin
  Result := Self.Merge().Mode();
end;





{------------|  Indices  |------------}
{!DOCREF} {
  @method: function TByteMatrix.Indices(Value: Byte; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TByteMatrix.Indices(Value: Byte; const Comparator:TComparator): TPointArray;
begin
  if Length(Self) > 0 then
    Result := exp_Indices(Self, Value, Comparator);
end;   


{!DOCREF} {
  @method: function TByteMatrix.Indices(Value: Byte; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TByteMatrix.Indices(Value: Byte; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin 
  Result := exp_Indices(Self, B, Value, Comparator); 
end;  


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TByteMatrix.ArgMax(): TPoint;
  @desc: ArgMax returns the index of the largest item
}
function TByteMatrix.ArgMax(): TPoint;
begin
  Result := exp_ArgMax(Self)
end;


{!DOCREF} {
  @method: function TByteMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TByteMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TByteMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'.
}
function TByteMatrix.ArgMax(B:TBox): TPoint; overload;
begin
  Result := exp_ArgMax(Self, B);
end;


{!DOCREF} {
  @method: function TByteMatrix.ArgMin(): TPoint; 
  @desc: ArgMin returns the index of the smallest item.
}
function TByteMatrix.ArgMin(): TPoint; 
begin
  if Length(Self) > 0 then
    Result := exp_ArgMin(Self);
end;


{!DOCREF} {
  @method: function TByteMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TByteMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TByteMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'.
}
function TByteMatrix.ArgMin(B:TBox): TPoint; overload;
begin
  if Length(Self) > 0 then
    Result := exp_ArgMin(Self, B);
end;

{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TByteMatrix.VarMax(): Byte;
  @desc:  ArgMax returns the largest item
}
function TByteMatrix.VarMax(): Byte;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TByteMatrix.VarMax(Count:Int32): TByteArray; overload;
  @desc: Returns the n-largest elements
}
function TByteMatrix.VarMax(Count:Int32): TByteArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TByteMatrix.VarMax(B:TBox): Byte; overload;
  @desc:  ArgMax returns the largest item within the given bounds `B`
}
function TByteMatrix.VarMax(B:TBox): Byte; overload;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TByteMatrix.VarMin(): Byte;
  @desc: ArgMin returns the the smallest item
}
function TByteMatrix.VarMin(): Byte;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TByteMatrix.VarMin(Count:Int32): TByteArray; overload;
  @desc: Returns the n-smallest elements
}
function TByteMatrix.VarMin(Count:Int32): TByteArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TByteMatrix.VarMin(B:TBox): Byte; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TByteMatrix.VarMin(B:TBox): Byte; overload;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TByteMatrix.MinMax(var Min, Max:Byte);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TByteMatrix.MinMax(var Min, Max:Byte);
begin 
  exp_MinMax(Self, Min, Max); 
end;


{------------|  CombineMatrix  |------------}
{!DOCREF} {
  @method: function TByteMatrix.Combine(Other:TByteMatrix; OP:Char='+'): TByteMatrix;
  @desc: 
    Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
    
    [code=pascal]
    var Mat:TByteMatrix;
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
function TByteMatrix.Combine(Other:TByteMatrix; OP:Char='+'): TByteMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;


{------------|  Normalize (Matrix)  |------------}

{!DOCREF} {
  @method: function TByteMatrix.Normalize(Alpha, Beta:Byte): TByteMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TByteMatrix.Normalize(Alpha, Beta:Byte): TByteMatrix;  
begin
  Result := exp_Normalize(Self, Alpha, Beta);
end;