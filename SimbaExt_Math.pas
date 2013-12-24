{*=========================================================================================|
| math.pas                                                                                 |
|=========================================================================================*}
{$loadlib \..\includes\simbaext\simbaext.dll}

function XT_DistManhattan(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistManhattan(pt1,pt2);
end;

function XT_DistEuclidean(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistEuclidean(pt1,pt2);
end;

function XT_DistChebyshev(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistChebyshev(pt1,pt2);
end;

function XT_DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
begin
  Result := exp_DistOctagonal(pt1,pt2);
end;

function XT_Modulo(X,Y:Extended): Extended;  
begin
  Result := exp_Modulo(X,Y);
end;

function XT_Mod(X,Y:Extended): Extended;  
begin 
  Result := exp_Modulo(X,Y); 
end;


function XT_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;  
begin
  Result := exp_InCircle(Pt, Center, Radius);
end;

function XT_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;  
begin
  Result := exp_InEllipse(Pt, Center, YRad, XRad);
end;

function XT_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;  
begin
  Result := exp_InRect(Pt, A,B,C,D);
end;

//
function XT_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPoly(x,y, poly);
end;

function XT_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyR(x,y, poly);
end;

function XT_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;  
begin
  Result := exp_InPolyW(x,y, poly);
end;

function XT_DeltaAngle(DegA,DegB:Extended): Extended;  
begin
  Result := exp_DeltaAngle(DegA,DegB);
end;