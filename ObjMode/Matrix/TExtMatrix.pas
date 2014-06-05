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
  @method: function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TExtMatrix.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
begin 
  Result := exp_Indices(Self, Value, Comparator); 
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
  Result := exp_Indices(Self, B, Value, Comparator); 
end;


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TExtMatrix.ArgMax(): TPoint; 
  @desc: ...
}
function TExtMatrix.ArgMax(): TPoint; 
begin 
  Result := exp_ArgMax(Self); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TExtMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMax(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMax(Self, B); 
end;



{!DOCREF} {
  @method: function TExtMatrix.ArgMin(): TPoint;
  @desc: ...
}
function TExtMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMin(Self);
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TExtMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ...
}
function TExtMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMin(Self, B); 
end;


{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TExtMatrix.VarMax(): Extended;
  @desc: Returns the largest element
}
function TExtMatrix.VarMax(): Extended;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
  @desc: Returns the n-largest elements
}
function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(B:TBox): Extended; overload;
  @desc: Returns the largest element within the given bounds `B`
}
function TExtMatrix.VarMax(B:TBox): Extended; overload;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMin(): Extended;
  @desc: Returns the the smallest element
}
function TExtMatrix.VarMin(): Extended;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMax(Count:Int32): TExtArray; overload;
  @desc: Returns the n-smallest elements
}
function TExtMatrix.VarMin(Count:Int32): TExtArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TExtMatrix.VarMin(B:TBox): Extended; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TExtMatrix.VarMin(B:TBox): Extended; overload;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TExtMatrix.MinMax(var Min, Max:Extended);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TExtMatrix.MinMax(var Min, Max:Extended);
begin
  exp_MinMax(Self, Min, Max);
end;


{------------|  CombineMatrix  |------------}
{!DOCREF} {
  @method: function TExtMatrix.Combine(Other:TExtMatrix; OP:Char='+'): TExtMatrix;
  @desc: Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
}
function TExtMatrix.Combine(Other:TExtMatrix; OP:Char='+'): TExtMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;