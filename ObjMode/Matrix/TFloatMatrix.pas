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
  @method: function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
}
function TFloatMatrix.Indices(Value: Single; const Comparator:TComparator): TPointArray;
begin
  if Length(Self) > 0 then
    Result := exp_Indices(Self, Value, Comparator);
end;   


{!DOCREF} {
  @method: function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'Matrix.Indices(10, __LT__)' would return all the items which are less then 10.
    Takes an extra param to only check a cirtain area of the matrix.
}
function TFloatMatrix.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin 
  Result := exp_Indices(Self, B, Value, Comparator); 
end;  


{------------|  ArgMin/ArgMax  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(): TPoint;
  @desc: ArgMax returns the index of the largest item
}
function TFloatMatrix.ArgMax(): TPoint;
begin
  Result := exp_ArgMax(Self)
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TFloatMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMax(B:TBox): TPoint; overload;
begin
  Result := exp_ArgMax(Self, B);
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(): TPoint; 
  @desc: ArgMin returns the index of the smallest item.
}
function TFloatMatrix.ArgMin(): TPoint; 
begin
  if Length(Self) > 0 then
    Result := exp_ArgMin(Self);
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TFloatMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'.
}
function TFloatMatrix.ArgMin(B:TBox): TPoint; overload;
begin
  if Length(Self) > 0 then
    Result := exp_ArgMin(Self, B);
end;

{------------|  VarMin/VarMax  |------------}
{!DOCREF} {
  @method: function TFloatMatrix.VarMax(): Single;
  @desc:  ArgMax returns the largest item
}
function TFloatMatrix.VarMax(): Single;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMax(Count:Int32): TFloatArray; overload;
  @desc: Returns the n-largest elements
}
function TFloatMatrix.VarMax(Count:Int32): TFloatArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMax(B:TBox): Single; overload;
  @desc:  ArgMax returns the largest item within the given bounds `B`
}
function TFloatMatrix.VarMax(B:TBox): Single; overload;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(): Single;
  @desc: ArgMin returns the the smallest item
}
function TFloatMatrix.VarMin(): Single;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(Count:Int32): TFloatArray; overload;
  @desc: Returns the n-smallest elements
}
function TFloatMatrix.VarMin(Count:Int32): TFloatArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TFloatMatrix.VarMin(B:TBox): Single; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TFloatMatrix.VarMin(B:TBox): Single; overload;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;


{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TFloatMatrix.MinMax(var Min, Max:Single);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TFloatMatrix.MinMax(var Min, Max:Single);
begin 
  exp_MinMax(Self, Min, Max); 
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
  Result := exp_CombineMatrix(Self, Other, OP); 
end;