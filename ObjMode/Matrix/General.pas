{$F-}
const
  __CeilingFunc = Ceil;
  __TruncFunc = Trunc;
  __FloorFunc = Floor;
  __SqrtFunc = Sqrt;
{$F+}

// ---| ToByte |--------------------------------------------------------------\\
function SimbaExt.ToByte(mat: TExtMatrix): TByteMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]) and $FF;
end;

function SimbaExt.ToByte(mat: TDoubleMatrix): TByteMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]) and $FF;
end;

function SimbaExt.ToByte(mat: TFloatMatrix): TByteMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]) and $FF;
end;

function SimbaExt.ToByte(mat: TIntMatrix): TByteMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j] and $FF;
end;

function SimbaExt.ToByte(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;



// ---| ToInt |---------------------------------------------------------------\\
function SimbaExt.ToInt(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]);
end;

function SimbaExt.ToInt(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]);
end;

function SimbaExt.ToInt(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Trunc(Mat[i][j]);
end;

function SimbaExt.ToInt(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.ToInt(mat: TByteMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;



// ---| ToFloat |-------------------------------------------------------------\\
function SimbaExt.ToFloat(mat: TExtMatrix): TFloatMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToFloat(mat: TDoubleMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToFloat(mat: TFloatMatrix): TFloatMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.ToFloat(mat: TIntMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToFloat(mat: TByteMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;



// ---| ToDouble |------------------------------------------------------------\\
function SimbaExt.ToDouble(mat: TExtMatrix): TDoubleMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToDouble(mat: TDoubleMatrix): TDoubleMatrix; overload;
begin 
  Result := Mat;
end;

function SimbaExt.ToDouble(mat: TFloatMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToDouble(mat: TIntMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToDouble(mat: TByteMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;



// ---| ToExtended |----------------------------------------------------------\\
function SimbaExt.ToExtended(mat: TExtMatrix): TExtMatrix;
begin 
  Result := Mat;
end;

function SimbaExt.ToExtended(mat: TDoubleMatrix): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToExtended(mat: TFloatMatrix): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToExtended(mat: TIntMatrix): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;

function SimbaExt.ToExtended(mat: TByteMatrix): TExtMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i][j];
end;



{------------------------------------------------------------------------------|
 Matrix math operations
 > Ceil,Floor,Trunc,Round,Abs,Sqr,Sqrt
 > Add,Subtract,Multiply,Divide
[------------------------------------------------------------------------------}


// ---| Ceil |----------------------------------------------------------------\\
function TObjMath.Ceil(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __CeilingFunc(Mat[i][j]);
end;

function TObjMath.Ceil(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __CeilingFunc(Mat[i][j]);
end;

function TObjMath.Ceil(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __CeilingFunc(Mat[i][j]);
end;

function TObjMath.Ceil(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;




// ---| Floor |---------------------------------------------------------------\\
function TObjMath.Floor(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __FloorFunc(Mat[i][j]);
end;

function TObjMath.Floor(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __FloorFunc(Mat[i][j]);
end;

function TObjMath.Floor(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __FloorFunc(Mat[i][j]);
end;

function TObjMath.Floor(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function TObjMath.Floor(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;



// ---| Round |---------------------------------------------------------------\\
function TObjMath.Round(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Abs(Frac(Mat[i,j])) >= 0.5 then
        if Mat[i,j] > 0 then
          Result[i,j] := __CeilingFunc(Mat[i,j])
        else
          Result[i,j] := __CeilingFunc(Mat[i,j] - 1)
      else
        Result[i,j] := __TruncFunc(Mat[i,j])
end;

function TObjMath.Round(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Abs(Frac(Mat[i,j])) >= 0.5 then
        if Mat[i,j] > 0 then
          Result[i,j] := __CeilingFunc(Mat[i,j])
        else
          Result[i,j] := __CeilingFunc(Mat[i,j] - 1)
      else
        Result[i,j] := __TruncFunc(Mat[i,j])
end;

function TObjMath.Round(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Abs(Frac(Mat[i,j])) >= 0.5 then
        if Mat[i,j] > 0 then
          Result[i,j] := __CeilingFunc(Mat[i,j])
        else
          Result[i,j] := __CeilingFunc(Mat[i,j] - 1)
      else
        Result[i,j] := __TruncFunc(Mat[i,j])
end;

function TObjMath.Round(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function TObjMath.Round(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;




// ---| Trunc |---------------------------------------------------------------\\
function TObjMath.Trunc(mat: TExtMatrix): TIntMatrix;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __TruncFunc(Mat[i][j]);
end;

function TObjMath.Trunc(mat: TDoubleMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __TruncFunc(Mat[i][j]);
end;

function TObjMath.Trunc(mat: TFloatMatrix): TIntMatrix; overload;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __TruncFunc(Mat[i][j]);
end;

function TObjMath.Trunc(mat: TIntMatrix): TIntMatrix; overload;
begin 
  Result := Mat;
end;

function TObjMath.Trunc(mat: TByteMatrix): TByteMatrix; overload;
begin 
  Result := Mat;
end;



// ---| Abs |-----------------------------------------------------------------\\
function TObjMath.Abs(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -Mat[i][j]
      else
        Result[i,j] := Mat[i][j];
end;

function TObjMath.Abs(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -Mat[i][j]
      else
        Result[i,j] := Mat[i][j];
end;

function TObjMath.Abs(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -Mat[i][j]
      else
        Result[i,j] := Mat[i][j];
end;

function TObjMath.Abs(mat: TIntMatrix): TIntMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -Mat[i][j]
      else
        Result[i,j] := Mat[i][j];
end;

function TObjMath.Abs(mat: TByteMatrix): TByteMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -Mat[i][j]
      else
        Result[i,j] := Mat[i][j];
end;



// ---| Sqr |-----------------------------------------------------------------\\
function TObjMath.Sqr(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j] * Mat[i,j];
end;

function TObjMath.Sqr(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j] * Mat[i,j];
end;

function TObjMath.Sqr(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j] * Mat[i,j];
end;

function TObjMath.Sqr(mat: TIntMatrix): TIntMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j] * Mat[i,j];
end;

function TObjMath.Sqr(mat: TByteMatrix): TIntMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := Mat[i,j] * Mat[i,j];
end;



// ---| Sqrt |----------------------------------------------------------------\\
function TObjMath.Sqrt(mat: TExtMatrix): TExtMatrix;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := __SqrtFunc(-Mat[i,j])
      else
        Result[i,j] := __SqrtFunc(Mat[i,j]);
end;

function TObjMath.Sqrt(mat: TDoubleMatrix): TDoubleMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := __SqrtFunc(-Mat[i,j])
      else
        Result[i,j] := __SqrtFunc(Mat[i,j]);
end;

function TObjMath.Sqrt(mat: TFloatMatrix): TFloatMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := __SqrtFunc(-Mat[i,j])
      else
        Result[i,j] := __SqrtFunc(Mat[i,j]);
end;

function TObjMath.Sqrt(mat: TIntMatrix): TExtMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if Mat[i,j] < 0 then
        Result[i,j] := -__SqrtFunc(-Mat[i,j])
      else
        Result[i,j] := __SqrtFunc(Mat[i,j]);
end;

function TObjMath.Sqrt(mat: TByteMatrix): TExtMatrix; overload;
var W,H,i,j: Int32;
begin
  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i,j] := __SqrtFunc(Mat[i][j]);
end;




// ---------------------------------------------------------------------------\\



// ---| Mul |-----------------------------------------------------------------\\
function TObjMath.Mul(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := exp_CombineMatrix(Left,Right, '*');
  except RaiseException(erException, se.GetException());
  end;
end;


function TObjMath.Mul(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '*');
  except RaiseException(erException, se.GetException());
  end;
end;


function TObjMath.Mul(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '*');
  except RaiseException(erException, se.GetException());
  end;
end;


function TObjMath.Mul(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '*');
  except RaiseException(erException, se.GetException());
  end;
end;


function TObjMath.Mul(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '*');
  except RaiseException(erException, se.GetException());
  end;
end;


// ---| Add |-----------------------------------------------------------------\\
function TObjMath.Add(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := exp_CombineMatrix(Left,Right, '+');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Add(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '+');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Add(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '+');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Add(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '+');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Add(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '+');
  except RaiseException(erException, se.GetException());
  end;
end;


// ---| Sub |-----------------------------------------------------------------\\
function TObjMath.Sub(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := exp_CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Sub(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Sub(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Sub(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Sub(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '-');
  except RaiseException(erException, se.GetException());
  end;
end;


// ---| Div |-----------------------------------------------------------------\\
function TObjMath.Divide(Left,Right:TByteMatrix): TByteMatrix;
begin
  try    Result := exp_CombineMatrix(Left,Right, '/');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Divide(Left,Right:TIntMatrix): TIntMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '/');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Divide(Left,Right:TFloatMatrix): TFloatMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '/');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Divide(Left,Right:TDoubleMatrix): TDoubleMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '/');
  except RaiseException(erException, se.GetException());
  end;
end;

function TObjMath.Divide(Left,Right:TExtMatrix): TExtMatrix; overload;
begin
  try    Result := exp_CombineMatrix(Left,Right, '/');
  except RaiseException(erException, se.GetException());
  end;
end;


