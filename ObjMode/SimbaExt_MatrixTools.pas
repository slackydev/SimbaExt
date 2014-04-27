{*=========================================================================================|
| Matrix.pas || Matrix/xxx.pas                                                             |
|=========================================================================================*}
function SimbaExt.NewMatrixEx(W,H, Init:Integer): T2DIntArray;  
begin
  exp_NewMatrixEx(W,H, Init, Result);
end;

function SimbaExt.NewMatrix(W,H:Integer): T2DIntArray;  
begin
  exp_NewMatrix(W,H, Result);
end;

procedure T2DIntArray.InsertTPA(const TPA:TPointArray; Value:Integer);  
begin
  exp_MatInsertTPA(Self, TPA, Value);
end;

function SimbaExt.MatrixFromTPAEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray;  
begin
  exp_TPAToMatrixEx(TPA,Init,Value,Align, Result);
end;

function SimbaExt.MatrixFromTPA(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray;  
begin
  exp_TPAToMatrix(TPA, Value, Align, Result);
end;

function T2DIntArray.Normalize(Alpha, Beta:Integer): T2DIntArray;  
begin
  exp_NormalizeMat(Self, Alpha, Beta, Result);
end;

function T2DIntArray.GetValues(const Indices:TPointArray): TIntArray;  
begin
  exp_MatGetValues(Self, Indices, Result);
end;

function T2DIntArray.GetCol(Column:Integer): TIntArray;  
begin
  exp_MatGetCol(Self, Column, Result);
end;

function T2DIntArray.GetRow(Row:Integer): TIntArray;  
begin
  exp_MatGetRow(Self, Row, Result);
end;

function T2DIntArray.GetCols(FromCol, ToCol:Integer): T2DIntArray;  
begin
  exp_MatGetCols(Self, FromCol, ToCol, Result);
end;

function T2DIntArray.GetRows(FromRow, ToRow:Integer): T2DIntArray;  
begin
  exp_MatGetRows(Self, FromRow, ToRow, Result);
end;

function T2DIntArray.GetArea(X1,Y1,X2,Y2:Integer): T2DIntArray;  
begin
  exp_MatGetArea(Self, X1,Y1,X2,Y2, Result);
end;

function MatrixFromTIA(const Arr:TIntegerArray; Width,Height:Integer):  T2DIntArray;  
begin
  exp_MatFromTIA(Arr, Width, Height, Result);
end;

procedure T2DIntArray.Padding(HPad,WPad:Integer);  
begin
  exp_PadMatrix(Self,HPad,WPad);
end;

function T2DIntArray.FloodFill(const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillMatrix(Self, Start, EightWay, Result);
end;




{*=========================================================================================|
| Matrix/xxx.pas                                                                           |
|=========================================================================================*}
function T2DIntArray.Indices(Value: Integer; const Comparator:TComparator): TPointArray;
begin exp_IndicesI(Self, Value, Comparator, Result); 
end;

function T2DExtArray.Indices(Value: Extended; const Comparator:TComparator): TPointArray;
begin exp_IndicesE(Self, Value, Comparator, Result); 
end;

function T2DDoubleArray.Indices(Value: Double; const Comparator:TComparator): TPointArray;
begin exp_IndicesD(Self, Value, Comparator, Result); 
end;

function T2DFloatArray.Indices(Value: Single; const Comparator:TComparator): TPointArray;
begin exp_IndicesF(Self, Value, Comparator, Result); 
end;    

function T2DIntArray.Indices(Value: Integer; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExI(Self, B, Value, Comparator, Result); 
end;

function T2DExtArray.Indices(Value: Extended; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExE(Self, B, Value, Comparator, Result); 
end;

function T2DDoubleArray.Indices(Value: Double; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExD(Self, B, Value, Comparator, Result); 
end;

function T2DFloatArray.Indices(Value: Single; B:TBox; const Comparator:TComparator): TPointArray; overload;
begin exp_IndicesExF(Self, B, Value, Comparator, Result); 
end;  


{------------|  MinMax  |------------}
procedure T2DIntArray.MinMax(var Min, Max:Integer); 
begin exp_MinMaxI(Self, Min, Max); 
end;

procedure T2DExtArray.MinMax(var Min, Max:Extended);
begin exp_MinMaxE(Self, Min, Max); 
end;

procedure T2DDoubleArray.MinMax(var Min, Max:Double);
begin exp_MinMaxD(Self, Min, Max); 
end;

procedure T2DFloatArray.MinMax(var Min, Max:Single);
begin exp_MinMaxF(Self, Min, Max); 
end;


{------------|  ArgMin/Max  |------------}
//argmax
function T2DIntArray.ArgMax(): TPoint; 
begin Result := exp_ArgMaxI(Self); 
end;

function T2DExtArray.ArgMax(): TPoint; 
begin Result := exp_ArgMaxE(Self); 
end;

function T2DDoubleArray.ArgMaxD(): TPoint;
begin Result := exp_ArgMaxD(Self); 
end;

function T2DFloatArray.ArgMax(): TPoint;
begin Result := exp_ArgMaxF(Self); 
end;

//ArgMaxEx
function T2DIntArray.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExI(Self, B); 
end;

function T2DExtArray.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExE(Self, B); 
end;

function T2DDoubleArray.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExD(Self, B); 
end;

function T2DFloatArray.ArgMax(B:TBox): TPoint; overload;
begin Result := exp_ArgMaxExF(Self, B); 
end;

//ArgMin
function T2DIntArray.ArgMin(): TPoint;
begin Result := exp_ArgMinI(Self); 
end;

function T2DExtArray.ArgMin(): TPoint;
begin Result := exp_ArgMinE(Self);
end;

function T2DDoubleArray.ArgMin(): TPoint;
begin Result := exp_ArgMinD(Self); 
end;

function T2DFloatArray.ArgMin(): TPoint; 
begin Result := exp_ArgMinF(Self); 
end; 

//ArgMinEx
function T2DIntArray.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExI(Self, B); 
end;

function T2DExtArray.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExE(Self, B); 
end;

function T2DDoubleArray.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExD(Self, B); 
end;

function T2DFloatArray.ArgMin(B:TBox): TPoint; overload;
begin Result := exp_ArgMinExF(Self, B); 
end;