


function Modulo(l: T2DExtended; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DInt64): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DUInt32): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DInt32): T2DInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DUInt8): T2DUInt8; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DInt8): T2DInt8; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DDouble): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DSingle): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DUInt64): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DInt64): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DUInt32): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DInt32): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DUInt8): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: T2DInt8): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DExtended; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: Double): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: Single): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: UInt64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: Int64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: UInt32): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: Int32): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: UInt8): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DExtended; r: Int8): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DSingle): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DUInt64): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DInt64): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DUInt32): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DInt32): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DUInt8): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: T2DInt8): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DDouble; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: Single): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: UInt64): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: Int64): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: UInt32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: Int32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: UInt8): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DDouble; r: Int8): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DUInt64): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DInt64): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DUInt32): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DInt32): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DUInt8): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: T2DInt8): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DSingle; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: UInt64): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: Int64): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: UInt32): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: Int32): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: UInt8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DSingle; r: Int8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DUInt32): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DInt32): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DUInt8): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: T2DInt8): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt64; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: Int64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: UInt32): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: Int32): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: UInt8): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt64; r: Int8): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DUInt32): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DInt32): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DUInt8): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: T2DInt8): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt64; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: Int64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: UInt32): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: Int32): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: UInt8): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt64; r: Int8): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DInt64): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DInt32): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DUInt8): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: T2DInt8): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt32; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: Int64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: UInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: Int32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: UInt8): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt32; r: Int8): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DInt64): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DUInt32): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DUInt8): T2DInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: T2DInt8): T2DInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt32; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: Int64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: UInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: Int32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: UInt8): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt32; r: Int8): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DInt64): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DUInt32): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DInt32): T2DInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: T2DInt8): T2DUInt8; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DUInt8; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: Int64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: UInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: Int32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: UInt8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DUInt8; r: Int8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: T2DExtended): T2DExtended; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DDouble): T2DDouble; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DSingle): T2DSingle; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DUInt64): T2DUInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DInt64): T2DInt64; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DUInt32): T2DUInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DInt32): T2DInt32; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: T2DUInt8): T2DUInt8; overload; cdecl;
var Wl,Hl,Wr,Hr,i,j: Int32; 
begin 
  Hl := High(l);  
  Hr := High(r);
  if (Hl = -1) or (HR = -1) then Exit();
  Wl := High(l[0]);   
  Wr := High(r[0]);
  if (Hl<>Hr) or (Wl<>Wr) then Exit;
  SetLength(Result, Hl+1,Wl+1);
  for i:=0 to Hl do
    for j:=0 to Wl do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r[i][j]) * r[i][j];

end;



function Modulo(l: T2DInt8; r: Extended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: Double): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: Single): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: UInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: Int64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: UInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: Int32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: UInt8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: T2DInt8; r: Int8): T2DInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(l);
  W := High(l[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r > 0 then Continue
      else Result[i][j] := l[i][j] - Floor(l[i][j] / r) * r;

end;



function Modulo(l: Extended; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DDouble): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DSingle): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DUInt64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DInt64): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DUInt32): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DInt32): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DUInt8): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Extended; r: T2DInt8): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DSingle): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DUInt64): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DInt64): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DUInt32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DInt32): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DUInt8): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Double; r: T2DInt8): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DUInt64): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DInt64): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DUInt32): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DInt32): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DUInt8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Single; r: T2DInt8): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DUInt32): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DInt32): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DUInt8): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt64; r: T2DInt8): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DInt64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DUInt32): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DInt32): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DUInt8): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int64; r: T2DInt8): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DInt64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DUInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DUInt8): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt32; r: T2DInt8): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DInt64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DUInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DInt32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DUInt8): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int32; r: T2DInt8): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DInt64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DUInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DInt32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DUInt8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: UInt8; r: T2DInt8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DExtended): T2DExtended; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DDouble): T2DDouble; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DSingle): T2DSingle; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DUInt64): T2DUInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DInt64): T2DInt64; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DUInt32): T2DUInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DInt32): T2DInt32; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DUInt8): T2DUInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;



function Modulo(l: Int8; r: T2DInt8): T2DInt8; overload; cdecl;
var W,H,i,j: Int32;
begin
  H := High(r);
  W := High(r[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
    for j:=0 to W do
      if r[i][j] > 0 then Continue
      else Result[i][j] := l - Floor(l / r[i][j]) * r[i][j];

end;


