// ---| ToByte |--------------------------------------------------------------\\
function TExtMatrix.AsByte(): TByteMatrix;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TDoubleMatrix.AsByte(): TByteMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TFloatMatrix.AsByte(): TByteMatrix; overload;
var W,H,i,j: Int32; 
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TIntMatrix.AsByte(): TByteMatrix; overload;
var W,H,i,j: Int32; 
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TByteMatrix.AsByte(): TByteMatrix; overload;
begin 
  Result := Copy(self);
end;



// ---| ToInt |---------------------------------------------------------------\\
function TExtMatrix.AsInt32(): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TDoubleMatrix.AsInt32(): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TFloatMatrix.AsInt32(): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Trunc(Self[i,j]);
end;

function TIntMatrix.AsInt32(): TIntMatrix; overload;
begin 
  Result := Copy(Self);
end;

function TByteMatrix.AsInt32(): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;



// ---| ToFloat |-------------------------------------------------------------\\
function TExtMatrix.AsFloat(): TFloatMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TDoubleMatrix.AsFloat(): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TFloatMatrix.AsFloat(): TFloatMatrix; overload;
begin 
  Result := Copy(Self);
end;

function TIntMatrix.AsFloat(): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TByteMatrix.AsFloat(): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;



// ---| ToDouble |------------------------------------------------------------\\
function TExtMatrix.AsDouble(): TDoubleMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TDoubleMatrix.AsDouble(): TDoubleMatrix; overload;
begin 
  Result := Copy(Self);
end;

function TFloatMatrix.AsDouble(): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TIntMatrix.AsDouble(): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TByteMatrix.AsDouble(): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;



// ---| ToExtended |----------------------------------------------------------\\
function TExtMatrix.AsExtended(): TExtMatrix;
begin 
  Result := Copy(self);
end;

function TDoubleMatrix.AsExtended(): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TFloatMatrix.AsExtended(): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TIntMatrix.AsExtended(): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;

function TByteMatrix.AsExtended(): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody_2.inc}{$ENDIF}
  Result[i,j] := Self[i,j];
end;



{------------------------------------------------------------------------------|
 Matrix math operations
 > Ceil,Floor,Trunc,Round,Abs,Sqr,Sqrt,Int
 > Add,Subtract,Multiply,Divide
[------------------------------------------------------------------------------}


// ---| Ceil |----------------------------------------------------------------\\
function SimbaExt.Ceil(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Ceil(Mat[i][j]);
end;

function SimbaExt.Ceil(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Ceil(Mat[i][j]);
end;

function SimbaExt.Ceil(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Ceil(Mat[i][j]);
end;

function SimbaExt.Ceil(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.Ceil(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;


// ---| Floor |---------------------------------------------------------------\\
function SimbaExt.Floor(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Floor(Mat[i][j]);
end;

function SimbaExt.Floor(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Floor(Mat[i][j]);
end;

function SimbaExt.Floor(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Floor(Mat[i][j]);
end;

function SimbaExt.Floor(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.Floor(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;



// ---| Round |---------------------------------------------------------------\\
function SimbaExt.Round(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Round(Mat[i,j]);
end;

function SimbaExt.Round(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Round(Mat[i,j]);
end;

function SimbaExt.Round(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Round(Mat[i,j]);
end;

function SimbaExt.Round(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Copy(Mat);
end;

function SimbaExt.Round(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Copy(Mat);
end;




// ---| Trunc |---------------------------------------------------------------\\
function SimbaExt.Trunc(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Trunc(Mat[i][j]);
end;

function SimbaExt.Trunc(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Trunc(Mat[i][j]);
end;

function SimbaExt.Trunc(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Trunc(Mat[i][j]);
end;

function SimbaExt.Trunc(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.Trunc(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;



// ---| Abs |-----------------------------------------------------------------\\
function SimbaExt.Abs(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Abs(Mat[i][j]);
end;

function SimbaExt.Abs(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Abs(Mat[i][j]);
end;

function SimbaExt.Abs(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Abs(Mat[i][j]);
end;

function SimbaExt.Abs(mat: TIntMatrix): TIntMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Abs(Mat[i][j]);
end;

function SimbaExt.Abs(mat: TByteMatrix): TByteMatrix; overload;
begin
  Result:= Copy(Mat);
end;



// ---| Sqr |-----------------------------------------------------------------\\
function SimbaExt.Sqr(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  try    Result := se.CombineMatrix(mat,mat, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Sqr(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  try    Result := se.CombineMatrix(mat,mat, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Sqr(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  try    Result := se.CombineMatrix(mat,mat, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Sqr(mat: TIntMatrix): TIntMatrix; overload;
var W,H,i,j: Int32;
begin
  try    Result := se.CombineMatrix(mat,mat, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Sqr(mat: TByteMatrix): TByteMatrix; overload;
var W,H,i,j: Int32;
begin
  try    Result := se.CombineMatrix(mat,mat, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;



// ---| Sqrt |----------------------------------------------------------------\\
function SimbaExt.Sqrt(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Sqrt(Mat[i,j]);
end;

function SimbaExt.Sqrt(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Sqrt(Mat[i,j]);
end;

function SimbaExt.Sqrt(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Sqrt(Mat[i,j]);
end;

function SimbaExt.Sqrt(mat: TIntMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Sqrt(Mat[i,j]);
end;

function SimbaExt.Sqrt(mat: TByteMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Sqrt(Mat[i,j]);
end;



// ---| Int |-----------------------------------------------------------------\\
function SimbaExt.Int(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Int(Mat[i,j]);
end;

function SimbaExt.Int(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Int(Mat[i,j]);
end;

function SimbaExt.Int(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Int(Mat[i,j]);
end;

function SimbaExt.Int(mat: TIntMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Int(Mat[i,j]);
end;

function SimbaExt.Int(mat: TByteMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  {$IFNDEF CODEINSIGHT}{$I depend/loopBody.inc}{$ENDIF}
  Result[i,j] := System.Int(Mat[i,j]);
end;


// ---------------------------------------------------------------------------\\



// ---| Mul |-----------------------------------------------------------------\\
function SimbaExt.Multiply(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := se.CombineMatrix(Left,Right, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


function SimbaExt.Multiply(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


function SimbaExt.Multiply(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


function SimbaExt.Multiply(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


function SimbaExt.Multiply(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '*');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


// ---| Add |-----------------------------------------------------------------\\
function SimbaExt.Add(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := se.CombineMatrix(Left,Right, '+');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Add(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '+');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Add(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '+');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Add(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '+');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Add(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '+');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


// ---| Sub |-----------------------------------------------------------------\\
function SimbaExt.Subtract(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := se.CombineMatrix(Left,Right, '-');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Subtract(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '-');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Subtract(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '-');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Subtract(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '-');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Subtract(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;


// ---| Div |-----------------------------------------------------------------\\
function SimbaExt.Divide(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := se.CombineMatrix(Left,Right, '/');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Divide(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '/');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Divide(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '/');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Divide(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '/');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;

function SimbaExt.Divide(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := se.CombineMatrix(Left,Right, '/');
  except RaiseWarning(se.GetException(), ERR_NOTICE);
  end;
end;


