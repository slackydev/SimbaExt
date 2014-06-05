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
  @desc: Merges the two matrices in to one matrix.. Supports different operatrions/methods for combining ['+','-','*','/'].
}
function TByteMatrix.Combine(Other:TByteMatrix; OP:Char='+'): TByteMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;