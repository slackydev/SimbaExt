{*=========================================================================================|
| math.pas                                                                                 |
|=========================================================================================*}
function se_DistManhattan(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistManhattan(pt1,pt2);
end;

function se_DistEuclidean(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistEuclidean(pt1,pt2);
end;

function se_DistChebyshev(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistChebyshev(pt1,pt2);
end;

function se_DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
begin
  Result := exp_DistOctagonal(pt1,pt2);
end;

function se_DistToLine(Pt,sA,sB:TPoint): Extended;
begin
  Result := exp_DistToLine(Pt,sA,sB);
end;

function se_Modulo(X,Y:Extended): Extended; overload; 
begin
  Result := exp_Modulo(X,Y);
end;

function se_Modulo(X,Y:Integer): Integer; overload;  
begin
  Result := exp_IModulo(X,Y);
end;

function se_Mod(X,Y:Extended): Extended; overload; 
begin
  Result := exp_Modulo(X,Y);
end;

function se_Mod(X,Y:Integer): Integer; overload;  
begin
  Result := exp_IModulo(X,Y);
end;

function se_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;  
begin
  Result := exp_InCircle(Pt, Center, Radius);
end;

function se_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;  
begin
  Result := exp_InEllipse(Pt, Center, YRad, XRad);
end;

function se_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;  
begin
  Result := exp_InRect(Pt, A,B,C,D);
end;

//
function se_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPoly(x,y, poly);
end;

function se_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyR(x,y, poly);
end;

function se_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyW(x,y, poly);
end;

function se_DeltaAngle(DegA,DegB:Extended): Extended;  
begin
  Result := exp_DeltaAngle(DegA,DegB);
end;