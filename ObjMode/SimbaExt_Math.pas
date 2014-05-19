{!DOCTOPIC}{ 
  Math module
} 

{!DOCREF} {
  @method: var Math = TObjMath;
  @desc: This module provides you with a few math-related functions.
}

{======| Table of contents |====================================================]

function TObjMath.Modulo(X,Y:Extended): Extended; overload; 
function TObjMath.Modulo(X,Y:Int32): Int32; overload;  
function TObjMath.Sign(X:Int32): Int32; overload; 
function TObjMath.Sign(X:Single): Int32; overload; 
function TObjMath.Sign(X:Double): Int32; overload; 
function TObjMath.Sign(X:Extended): Int32; overload; 
function TObjMath.DeltaAngle(x,y:Extended): Extended;  
function TObjMath.DistManhattan(pt1,pt2: TPoint): Extended; 
function TObjMath.DistEuclidean(pt1,pt2: TPoint): Extended;  
function TObjMath.DistChebyshev(pt1,pt2: TPoint): Extended;  
function TObjMath.DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
function TObjMath.DistToLine(Pt,sA,sB:TPoint): Extended;
function TObjMath.InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean; 
function TObjMath.InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean; 
function TObjMath.InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;  
function TObjMath.InPoly(x,y:Integer; const Poly:TPointArray): Boolean;  
function TObjMath.InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;  
function TObjMath.InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;  

[===============================================================================}


{!DOCREF} {
  @method: function Math.Modulo(X,Y:Extended): Extended; overload;
  @desc: "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and many more "modern" programming languages.
}
function TObjMath.Modulo(X,Y:Extended): Extended; overload; 
begin
  Result := exp_Modulo(X,Y);
end;


{!DOCREF} {
  @method: function Math.Modulo(X,Y:Int32): Int32; overload;
  @desc: "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and many more "modern" programming languages.
}
function TObjMath.Modulo(X,Y:Int32): Int32; overload;  
begin
  Result := exp_IModulo(X,Y);
end;


{!DOCREF} {
  @method: function Math.Sign(X:Int32): Int32; overload;
  @desc: 
    Results: c'-1 if x < 0'  ||  c'0 if x = 0'  ||  c'1 if x > 0' 
    Supports: Int32, Single, Double, Extended
}
function TObjMath.Sign(X:Int32): Int32; overload; 
begin
 if (x > 0) then Exit(1) else if (x < 0) then Exit(-1);
 Result := 0;
end;

function TObjMath.Sign(X:Single): Int32; overload; 
begin
 if (x > 0) then Exit(1) else if (x < 0) then Exit(-1);
 Result := 0;
end;

function TObjMath.Sign(X:Double): Int32; overload; 
begin
 if (x > 0) then Exit(1) else if (x < 0) then Exit(-1);
 Result := 0;
end;

function TObjMath.Sign(X:Extended): Int32; overload; 
begin
 if (x > 0) then Exit(1) else if (x < 0) then Exit(-1);
 Result := 0;
end;


{!DOCREF} {
  @method: function Math.DeltaAngle(x,y:Extended): Extended;
  @desc: Computes the delta of two angles. The result is in range of -180..180.
}
function TObjMath.DeltaAngle(x,y:Extended): Extended;  
begin
  Result := exp_DeltaAngle(x,y);
end;


{!DOCREF} {
  @method: function Math.DistManhattan(pt1,pt2: TPoint): Extended;
  @desc: Computes the 'Manhattan distance' between the two given points 'pt1' and 'pt2'
}
function TObjMath.DistManhattan(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistManhattan(pt1,pt2);
end;


{!DOCREF} {
  @method: function Math.DistEuclidean(pt1,pt2: TPoint): Extended;
  @desc: Computes the 'Euclidean distance' between the two given points 'pt1' and 'pt2'
}
function TObjMath.DistEuclidean(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistEuclidean(pt1,pt2);
end;


{!DOCREF} {
  @method: function Math.DistChebyshev(pt1,pt2: TPoint): Extended;
  @desc: Computes the 'Chebyshev distance' between the two given points 'pt1' and 'pt2'
}
function TObjMath.DistChebyshev(pt1,pt2: TPoint): Extended;  
begin
  Result := exp_DistChebyshev(pt1,pt2);
end;


{!DOCREF} {
  @method: function Math.DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
  @desc: 
    Computes the distance between the two given points 'pt1' and 'pt2'. 
    QuasiEuclidian is very similar to euclidean distance, but is cheaper to compute. "Result" is more in the shape of a octagon, then a circle.
}
function TObjMath.DistQuasiEuclidean(pt1,pt2: TPoint): Extended;
begin
  Result := exp_DistOctagonal(pt1,pt2);
end;


{!DOCREF} {
  @method: function Math.DistToLine(Pt,sA,sB:TPoint): Extended;
  @desc: Calculates the distance from Point 'pt' to the line segment defined by 'sA->sB' 
}
function TObjMath.DistToLine(Pt,sA,sB:TPoint): Extended;
begin
  Result := exp_DistToLine(Pt,sA,sB);
end;


{!DOCREF} {
  @method: function Math.InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;  
  @desc: Check if the point 'Pt' is within the given circle
}
function TObjMath.InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;  
begin
  Result := exp_InCircle(Pt, Center, Radius);
end;


{!DOCREF} {
  @method: function Math.InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;
  @desc: Check if the point 'Pt' is within the given ellipse
}
function TObjMath.InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;  
begin
  Result := exp_InEllipse(Pt, Center, YRad, XRad);
end;


{!DOCREF} {
  @method: function Math.InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;
  @desc: Checks if the point 'Pt' is within the given rectangle.
}
function TObjMath.InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;  
begin
  Result := exp_InRect(Pt, A,B,C,D);
end;


{!DOCREF} {
  @method: function Math.InPoly(x,y:Integer; const Poly:TPointArray): Boolean;
  @desc: Checks of the given point defined by 'x' and 'y' is within a polygon defined by 'Poly'
}
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






