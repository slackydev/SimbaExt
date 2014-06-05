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


{------------|  Indices  |------------}
{!DOCREF} {
  @method: function TDoubleMatrix.Indices(Value: Double; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
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
  @desc: Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
}
function TDoubleMatrix.Combine(Other:TDoubleMatrix; OP:Char='+'): TDoubleMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;