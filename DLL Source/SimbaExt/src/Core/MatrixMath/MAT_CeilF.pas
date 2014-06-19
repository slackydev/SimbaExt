


function CeilF(mat: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DUInt64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DInt64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DUInt32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DInt32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DUInt8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;



function CeilF(mat: T2DInt8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32; 
begin 
  H := High(mat);
  if (H = -1) then Exit(); 
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      Result[i][j] := Ceil(Mat[i][j]);
end;


