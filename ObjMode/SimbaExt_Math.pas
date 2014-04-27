{*=========================================================================================|
| math.pas                                                                                 |
|=========================================================================================*}
function TObjMath.DistManhattan(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistManhattan(pt1,pt2);
end;

function TObjMath.DistEuclidean(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistEuclidean(pt1,pt2);
end;

function TObjMath.DistChebyshev(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistChebyshev(pt1,pt2);
end;

function TObjMath.DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
begin
  Result := exp_DistOctagonal(pt1,pt2);
end;

function TObjMath.DistToLine(Pt,sA,sB:TPoint): Extended;
begin
  Result := exp_DistToLine(Pt,sA,sB);
end;

function TObjMath.Modulo(X,Y:Extended): Extended; overload; 
begin
  Result := exp_Modulo(X,Y);
end;

function TObjMath.Modulo(X,Y:Integer): Integer; overload;  
begin
  Result := exp_IModulo(X,Y);
end;

function TObjMath.InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;  
begin
  Result := exp_InCircle(Pt, Center, Radius);
end;

function TObjMath.InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;  
begin
  Result := exp_InEllipse(Pt, Center, YRad, XRad);
end;

function TObjMath.InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;  
begin
  Result := exp_InRect(Pt, A,B,C,D);
end;

//
function TObjMath.InPoly(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPoly(x,y, poly);
end;

function TObjMath.InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyR(x,y, poly);
end;

function TObjMath.InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyW(x,y, poly);
end;

function TObjMath.DeltaAngle(DegA,DegB:Extended): Extended;  
begin
  Result := exp_DeltaAngle(DegA,DegB);
end;