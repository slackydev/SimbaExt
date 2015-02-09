  H := High(mat);
  if (H = -1) then Exit();
  W := High(mat[0]);
  SetLength(Result, H+1,W+1);
  for i:=0 to H do
  for j:=0 to W do