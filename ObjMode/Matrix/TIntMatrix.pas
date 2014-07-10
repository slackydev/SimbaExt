{!DOCTOPIC}{ 
  Matrix » TIntMatrix
} 

//-----------------------------------------------------------------------------
{!DOCREF} {
  @method: Integer matrix
  @desc: [hr]
}

{!DOCREF} {
  @method: function se.NewMatrix(W,H:Int32; Init:Int32=0): TIntMatrix;
  @desc: Creates a new matrix. Fills it with `Init`.
}
function SimbaExt.NewMatrix(W,H:Int32; Init:Int32=0): TIntMatrix;  
begin
  if Init = 0 then
    SetLength(Result, H,W)
  else
    Result := exp_NewMatrixEx(W,H, Init);
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
  @method: function TIntMatrix.Get(const Indices:TPointArray): TIntArray; 
  @desc:
    Gets all the values at the given indices. If any of the points goes out
    of bounds, it will simply be ignored.
    [code=pascal]
    var 
      Matrix:TIntMatrix;
    begin
      Matrix.SetSize(100,100);
      Matrix[10][10] := 100;
      Matrix[10][13] := 29;
      WriteLn( Matrix.Get([Point(10,10),Point(13,10),Point(20,20)]));
    end;
    [/code]
}
function TIntMatrix.Get(const Indices:TPointArray): TIntArray;  
begin
  Result := exp_GetValues(Self, Indices);
end;


{!DOCREF} {
  @method: procedure TIntMatrix.Put(const TPA:TPointArray; Values:TIntArray);  
  @desc: Adds the points to the matrix with the given values.
}
procedure TIntMatrix.Put(const TPA:TPointArray; Values:TIntArray);  
begin
  exp_PutValues(Self, TPA, Values);
end;


{!DOCREF} {
  @method: procedure TIntMatrix.Put(const TPA:TPointArray; Value:Int32); overload;  
  @desc: Adds the points to the matrix with the given value.
}
procedure TIntMatrix.Put(const TPA:TPointArray; Value:Int32); overload; 
begin
  exp_PutValues(Self, TPA, TIntArray([Value]));
end;



{!DOCREF} {
  @method: function TIntMatrix.Merge(): TIntArray;
  @desc: Merges the matrix is to a flat array of the same type.
}
function TIntMatrix.Merge(): TIntArray;
var i,s,wid: Int32;
begin
  S := 0;
  SetLength(Result, Self.Width()*Self.Height());
  Wid := Self.Width();
  for i:=0 to High(Self) do
  begin
    MemMove(Self[i][0], Result[S], Wid*SizeOf(Int32));
    S := S + Wid;
  end; 
end;


{!DOCREF} {
  @method: function TIntMatrix.Sum(): Int64;
  @desc: Returns the sum of the matrix
}
function TIntMatrix.Sum(): Int64;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;




{!DOCREF} {
  @method: function TIntMatrix.Mean(): Double;
  @desc: Returns the mean of the matrix
}
function TIntMatrix.Mean(): Double;
var i: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function TIntMatrix.Stdev(): Double;
  @desc: Returns the standard deviation of the matrix
}
function TIntMatrix.Stdev(): Double;
var
  x,y,i,W,H:Int32;
  avg:Single;
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
  @method: function TIntMatrix.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the matrix. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TIntMatrix.Variance(): Double;
var
  avg:Single;
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
  @method: function TIntMatrix.Mode(): Int64;
  @desc:
    Returns the sample mode of the matrix, which is the most frequently occurring value in the matrix.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TIntMatrix.Mode(): Int64;
begin
  Result := Self.Merge().Mode();
end;




//---------------------------------------------------------------------------------------------------\\


{!DOCREF} {
  @method: function TIntMatrix.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;
  @desc:
    Converts a TPA to a matrix, where each element in the TPA will be given a value, and the rest will be the value of c'init'.
    Align must be true if you want to fir each point to the start of the matrix.
}
function SimbaExt.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): TIntMatrix;  
begin
  Result := exp_TPAToMatrixEx(TPA,Init,Value,Align);
end;


{!DOCREF} {
  @method: function se.MatrixFromTPA(const TPA:TPointArray; Value:Int32; Align:Boolean): TIntMatrix;
  @desc:
    Converts a TPA to a matrix, where each element in the TPA will be given a value, and the rest will be 0.
    Align must be true if you want to fir each point to the start of the matrix.

}
function SimbaExt.MatrixFromTPA(const TPA:TPointArray; Value:Int32; Align:Boolean): TIntMatrix;  
begin
  Result := exp_TPAToMatrix(TPA, Value, Align);
end;


{!DOCREF} {
  @method: function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
  @desc: Returns all the wanted columns as a new matrix.
}
function TIntMatrix.GetCols(FromCol, ToCol:Integer): TIntMatrix;  
begin
  Result := exp_MatGetCols(Self, FromCol, ToCol);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
  @desc: Returns all the wanted rows as a new matrix.
}
function TIntMatrix.GetRows(FromRow, ToRow:Integer): TIntMatrix;  
begin
  Result :=  exp_MatGetRows(Self, FromRow, ToRow);
end;

{!DOCREF} {
  @method: function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
  @desc: Crops the matrix to the given box and returns that area.
}
function TIntMatrix.GetArea(X1,Y1,X2,Y2:Integer): TIntMatrix;  
begin
  Result := exp_MatGetArea(Self, X1,Y1,X2,Y2);
end;

{!DOCREF} {
  @method: function se.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix; 
  @desc: Converts a TIntArray to a TIntMatrix of the given width, and height.
}
function SimbaExt.MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  TIntMatrix;  
begin
  Result := exp_MatFromTIA(Arr, Width, Height);
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
  Result := exp_FloodFillMatrix(Self, Start, EightWay);
end;




{*=========================================================================================|
| Matrix/xxx.pas                                                                           |
|=========================================================================================*}

{!DOCREF} {
  @method: function TIntMatrix.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
  @desc:
    Returns all the indices which matches the given value, and comperator.
    EG: c'TPA := Matrix.Indices(10, __LT__)' would return where all the items which are less then 10 is.
}
function TIntMatrix.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
begin 
  Result := exp_Indices(Self, Value, Comparator); 
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
  Result := exp_Indices(Self, B, Value, Comparator); 
end;


{----------------|  ArgMin/Max  |----------------}
//argmax
{!DOCREF} {
  @method: function TIntMatrix.ArgMax(): TPoint; 
  @desc: ArgMax returns the index of the largest item
}
function TIntMatrix.ArgMax(): TPoint; 
begin 
  Result := exp_ArgMax(Self); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMax(Count:Int32): TPointArray; overload;
  @desc: Returns the n-largest elements, by index
}
function TIntMatrix.ArgMax(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
  @desc: ArgMax returns the index of the largest item within the given bounds c'B'
}
function TIntMatrix.ArgMax(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMax(Self, B); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMin(): TPoint;
  @desc: ArgMin returns the index of the smallest item
}
function TIntMatrix.ArgMin(): TPoint;
begin 
  Result := exp_ArgMin(Self); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMin(Count:Int32): TPointArray; overload;
  @desc: Returns the n-smallest elements, by index
}
function TIntMatrix.ArgMin(Count:Int32): TPointArray; overload;
begin 
  Result := exp_ArgMulti(Self, Count, False); 
end;


{!DOCREF} {
  @method: function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
  @desc: ArgMin returns the index of the smallest item within the given bounds c'B'
}
function TIntMatrix.ArgMin(B:TBox): TPoint; overload;
begin 
  Result := exp_ArgMin(Self, B); 
end;



{----------------|  VarMin/VarMax  |----------------}
{!DOCREF} {
  @method: function TIntMatrix.VarMax(): Int32;
  @desc:  ArgMax returns the largest item
}
function TIntMatrix.VarMax(): Int32;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TIntMatrix.VarMax(Count:Int32): TIntArray; overload;
  @desc: Returns the n-largest elements
}
function TIntMatrix.VarMax(Count:Int32): TIntArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, True); 
end;


{!DOCREF} {
  @method: function TIntMatrix.VarMax(B:TBox): Int32; overload;
  @desc:  ArgMax returns the largest item within the given bounds `B`
}
function TIntMatrix.VarMax(B:TBox): Int32; overload;
var tmp:TPoint;
begin
  tmp := exp_ArgMax(Self, B);
  Result := Self[tmp.y, tmp.x];
end;


{!DOCREF} {
  @method: function TIntMatrix.VarMin(): Int32;
  @desc: ArgMin returns the the smallest item
}
function TIntMatrix.VarMin(): Int32;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self);
  Result := Self[tmp.y, tmp.x];  
end;


{!DOCREF} {
  @method: function TIntMatrix.VarMin(Count:Int32): TIntArray; overload;
  @desc: Returns the n-smallest elements
}
function TIntMatrix.VarMin(Count:Int32): TIntArray; overload;
begin 
  Result := exp_VarMulti(Self, Count, False); 
end;

{!DOCREF} {
  @method: function TIntMatrix.VarMin(B:TBox): Int32; overload;
  @desc: VarMin returns the smallest item within the given bounds `B`
}
function TIntMatrix.VarMin(B:TBox): Int32; overload;
var tmp:TPoint;
begin 
  tmp := exp_ArgMin(Self, B); 
  Result := Self[tmp.y, tmp.x];
end;



{------------|  MinMax  |------------}
{!DOCREF} {
  @method: procedure TIntMatrix.MinMax(var Min, Max:Integer);
  @desc: Returns the smallest, and the largest element in the matrix.
}
procedure TIntMatrix.MinMax(var Min, Max:Integer);
begin
  exp_MinMax(Self, Min, Max);
end;


{------------|  CombineMatrix (Math operations)  |------------}
{!DOCREF} {
  @method: function TIntMatrix.Combine(Other:TIntMatrix; OP:Char='+'): TIntMatrix;
  @desc: 
    Merges the two matrices in to one matrix.. Supports different math-operatrions for combining ['+','-','*','/'].
    
    [code=pascal]
    var Mat:TIntMatrix;
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
function TIntMatrix.Combine(Other:TIntMatrix; OP:Char='+'): TIntMatrix;
begin 
  Result := exp_CombineMatrix(Self, Other, OP); 
end;

//....
function TIntMatrix.Multiply(Other:TIntMatrix): TIntMatrix;
begin Result := exp_CombineMatrix(Self, Other, '*'); end;

function TIntMatrix.Add(Other:TIntMatrix): TIntMatrix;
begin Result := exp_CombineMatrix(Self, Other, '+'); end;

function TIntMatrix.Divide(Other:TIntMatrix): TIntMatrix;
begin Result := exp_CombineMatrix(Self, Other, '/'); end;

function TIntMatrix.Subtract(Other:TIntMatrix): TIntMatrix;
begin Result := exp_CombineMatrix(Self, Other, '-'); end;


{------------|  Normalize (Matrix)  |------------}

{!DOCREF} {
  @method: function TIntMatrix.Normalize(Alpha, Beta:Int32): TIntMatrix;  
  @desc: Fits each element of the matrix within the values of Alpha and Beta.
}
function TIntMatrix.Normalize(Alpha, Beta:Int32): TIntMatrix;  
begin
  Result := exp_Normalize(Self, Alpha, Beta);
end;