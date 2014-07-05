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

function TObjMath.Sign(X:Int32): Int8; overload;
function TObjMath.Sign(X:Int64): Int8; overload;
function TObjMath.Sign(X:Single): Int8; overload;
function TObjMath.Sign(X:Double): Int8; overload;
function TObjMath.Sign(X:Extended): Int8; overload;

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

(* Prime *)
function TObjMath.IsPrime(n: Int64): Boolean; Inline;
function TObjMath.NextPrime(n: Int64): Int64; inline;
function TObjMath.PrevPrime(n: Int64): Int64; inline;

(* Next power of two minus 1 *)
function TObjMath.NextPow2m1(n:Int32): Int32;


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
  @method: function Math.Sign(X:Extended): Int8;
  @desc:   Results: c'-1 if x < 0'  ||  c'0 if x = 0'  ||  c'1 if x > 0'
}
function TObjMath.Sign(X:Extended): uInt8; overload;
begin if (x > 0) then Exit(1) else if (x < 0) then Exit(-1) else Exit(0); end;


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



{!DOCREF} {
  @method: function Math.IsPrime(n: Int64): Boolean; Inline;
  @desc: Returns `True` if `n` is a prime number, otherwise it returns `False`
}
function TObjMath.IsPrime(n: Int64): Boolean;
var i:Integer; Hi: Single;
begin
  if (n = 2) then Exit(True);
  if (n and 2 = 0) or (n<=1) then Exit(False);
  Hi := Sqrt(n)+1;
  i := 3;
  while i <= Hi do begin
    if ((n mod i) = 0) then Exit(False);
    i := i + 2;
  end;
  Result := True;
end;


{!DOCREF} {
  @method: function Math.NextPrime(n: Int64): Int64;
  @desc: Returns the prime that comes next after the number `n`.
}
function TObjMath.NextPrime(n: Int64): Int64;
begin
  Inc(n);
  while Not(IsPrime(n)) do
    Inc(n);
  Result := n;
end;


{!DOCREF} {
  @method: function Math.PrevPrime(n: Int64): Int64;
  @desc: Returns the prime before `n`.
}
function TObjMath.PrevPrime(n: Int64): Int64;
begin
  Dec(n);
  while Not(IsPrime(n)) do
    Dec(n);
  Result := n;
end;


{!DOCREF} {
  @method: function Math.NextPow2m1(n:Int32): Int32;
  @desc: Returns the next power of two minus 1, very quick!
}
function TObjMath.NextPow2m1(n:Int32): Int32;
begin
  n := n - 1;
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  n := n or (n shr 32);
  Result := n;
end;
