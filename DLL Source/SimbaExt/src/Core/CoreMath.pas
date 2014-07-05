Unit CoreMath;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
 
interface

uses
  Math, SysUtils, CoreTypes;
  
  
function Radians(Dgrs: Extended): Extended; Inline;
function Degrees(Rads: Extended): Extended; Inline;

{* "Real" modulo operation *}
function Modulo(X,Y:Extended): Extended; Inline; overload;
function Modulo(X,Y:Double):  Double;  Inline; overload;
function Modulo(X,Y:Single):  Single;  Inline; overload;
function Modulo(X,Y:Int32): Int32; Inline; overload;

{* Angle between two angles *}
function DeltaAngle(DegA,DegB:Extended): Extended; Inline;

{* Distance calcs *}
function DistManhattan(const pt1,pt2: TPoint): Extended; Inline; 
function DistEuclidean(const pt1,pt2: TPoint): Extended; Inline; 
function DistChebyshev(const pt1,pt2: TPoint): Extended; Inline; 
function DistOctagonal(const pt1,pt2: TPoint): Extended; Inline;
function DistToLine(Pt, sA, sB: TPoint): Extended; Inline;

{* In shape *}
function InCircle(const  Pt,Center:TPoint; Radius: Integer): Boolean; Inline; 
function InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean; Inline; 
function InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; Inline; 
function InPoly(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InPolyR(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InPolyW(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InBox(const Pt:TPoint; X1,Y1, X2,Y2: Integer): Boolean; Inline;
function InTriangle(const Pt, v1, v2, v3:TPoint): Boolean; Inline;

{* Prime *}
function IsPrime(n: Integer): Boolean; Inline;
function NextPrime(n: Integer): Integer; inline;
function PrevPrime(n: Integer): Integer; inline;

{* Next power of two minus 1 *}
function NextPow2m1(n: Integer): Integer; Inline;

{* Select min of 3 values *}
function Min(X,Y,Z:Extended): Extended; Inline; overload;
function Min(X,Y,Z:Double): Double; Inline; overload;
function Min(X,Y,Z:Single): Single; Inline; overload;
function Min(X,Y,Z:Int64): Int64; Inline; overload;
function Min(X,Y,Z:Int32): Int32; Inline; overload;

{* Select max of 3 values *}
function Max(X,Y,Z:Extended): Extended; Inline; overload;
function Max(X,Y,Z:Double): Double; Inline; overload;
function Max(X,Y,Z:Single): Single; Inline; overload;
function Max(X,Y,Z:Int64): Int64; Inline; overload;
function Max(X,Y,Z:Int32): Int32; Inline; overload;


//--------------------------------------------------
implementation
uses CoreMisc;

{* Converts Degrees in to radian *}
function Radians(Dgrs: Extended): Extended; Inline;
begin Result := Dgrs * (Pi/180); end;

{* Converts Radian in to Degrees *}
function Degrees(Rads: Extended): Extended; Inline;
begin Result := Rads * (180/Pi); end;


{*
 "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and many more "modern" programming languages.
*}
function Modulo(X,Y:Extended): Extended; Inline; overload;
begin
  Result := X - Floor(X / Y) * Y;
end;

function Modulo(X,Y:Double): Double; Inline; overload;
begin
  Result := X - Floor(X / Y) * Y;
end;

function Modulo(X,Y:Single): Single; Inline; overload;
begin
  Result := X - Floor(X / Y) * Y;
end;

function Modulo(X,Y:Int32): Int32; Inline; overload;
begin
  Result := X - Floor(X / Y) * Y;
end;


{*
 Computes the delta of two angles. The result is in range of -180..180.
*}
function DeltaAngle(DegA,DegB:Extended): Extended; Inline; 
begin
  Result := Modulo((DegA - DegB + 180), 360) - 180;
end;



//============================================================================\\
{============================ DISTANCE CALCULATIONS ===========================}
{==============================================================================}

{*
 Manhattan distance is a simple, and cheap way to get distnace between two points
*}
function DistManhattan(const pt1,pt2: TPoint): Extended; Inline; 
begin
  Result := (Abs(pt1.x - pt2.x) + Abs(pt1.y - pt2.y));
end;

{*
 Distance measured in a streight line from pt1 to pt2.
 Uses pythagorean theorem... 
*}
function DistEuclidean(const pt1,pt2: TPoint): Extended; Inline; 
begin
  Result := Sqrt(Sqr(pt1.x - pt2.x) + Sqr(pt1.y - pt2.y));
end;

{*
 Distance in the form of "amount of steps" in a any direction. 
 EG: Think of the 8 possible moves the King can do on a chessboard, that = 1 distnace. 
*}
function DistChebyshev(const pt1,pt2: TPoint): Extended; Inline; 
begin
  Result := Max(Abs(pt1.x - pt2.x), Abs(pt1.y - pt2.y));
end;

{*
 Distance is measured as a middle-thing of Manhattan and Chebyshev-distance.
 This results in eight 45degr corners, aka a octagon. 
 It's close to as fast as Chebyshev, and Manhattan.
 @magic_number: 0.414213562 = Sqrt(2)-1
*}
function DistOctagonal(const pt1,pt2: TPoint): Extended; Inline; 
var
  dx,dy:Integer;
begin
  dx := Abs(pt1.x - pt2.x);
  dy := Abs(pt1.y - pt2.y);
  if dx >= dy then Result := dx + (dy * 0.414213562)
  else Result := dy + (dx * 0.414213562);
end;


{*
 Distance from Pt to the line-segment defined by sA-sB.
*}
function DistToLine(Pt, sA, sB: TPoint): Extended; Inline;
var
  dx,dy,d:integer;
  f: Single;
  qt:TPoint;
begin
  dx := sB.x - sA.x;
  dy := sB.y - sA.y;
  d := dx*dx + dy*dy;
  if (d = 0) then Exit(DistEuclidean(pt, sA));
  f := ((pt.x - sA.x) * (dx) + (pt.y - sA.y) * (dy)) / d;
  if (f < 0) then Exit(DistEuclidean(pt, sA));
  if (f > 1) then Exit(DistEuclidean(pt, sB));
  qt.x := Round(sA.x + f * dx);
  qt.y := Round(sA.y + f * dy);
  Result := DistEuclidean(pt, qt);
end; 


//============================================================================\\
{============================= SHAPE CALCULATIONS =============================}
{==============================================================================}


{*
 Check if a point is within a circle.
*}
function InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean; Inline; 
begin
  Result := Sqr(Pt.X - Center.X) + Sqr(Pt.Y - Center.Y) <= Sqr(Radius);
end;

  
  
{*
 Check if a point is within a ellipse.
*}
function InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean; Inline; 
var
  X, Y: Integer;
begin
  X := Pt.X - Center.X;
  Y := Pt.Y - Center.Y;
  Result := (Sqr(X)*Sqr(YRad))+(Sqr(Y)*Sqr(XRad)) <= (Sqr(YRad)*Sqr(XRad));
end; 


{*
 Is the coordiants within a rectangle (defined by four points)?
 > C is not actually used, but for future extension/changes, i'll leave it here.
*}
function InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; Inline; 
var
  Vec:TPoint; 
  Dot:Extended;
begin
  Vec := Point(A.x-B.x, A.y-B.y);
  Dot := ((A.x-Pt.x) * Vec.x) + ((A.y-Pt.y) * Vec.y);
  if not((0 <= Dot) and (Dot <= (Sqr(Vec.x) + Sqr(Vec.y)))) then
    Exit(False);
  Vec := Point(A.x-D.x, A.y-D.y);
  Dot := ((A.x-Pt.x) * Vec.x) + ((A.y-Pt.y) * Vec.y);  
  if not((0 <= Dot) and (Dot <= (Sqr(Vec.x) + Sqr(Vec.y)))) then
    Exit(False);
  Result := True;
end;


{*
 Is the coordiants within a box aligned with the axes?
*}
function InBox(const Pt:TPoint; X1,Y1, X2,Y2: Integer): Boolean; Inline;
begin
  Result:= (Pt.X >= X1) and (Pt.X <= X2) and
           (Pt.Y >= Y1) and (Pt.Y <= Y2);
end;


{* 
 Check if a point is within a polygon/shape by the given outline points (poly)
 The points must be in order, as if you would draw a line trough each point.
 @note: Ray casting combined with Winding number algorithm
*}
function InPoly(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
var
  WN,H,i,j:Integer;
  RC:Boolean;
begin
  WN := 0;
  H := High(poly);
  j := H;
  RC := False;
  for i:=0 to H do begin
    if ((Poly[i].x = x) and (Poly[i].y = y)) then
      Exit(True);
    if ((poly[i].y < y) and (poly[j].y >= y) or (poly[j].y < y) and (poly[i].y >= y)) then
      if (poly[i].x+(y-poly[i].y) / (poly[j].y-poly[i].y) * (poly[j].x-poly[i].x) < x) then
         RC := Not(RC);
    if (poly[i].y <= y) then begin
      if (poly[j].y > y) then
        if (((poly[j].x-poly[i].x)*(y-poly[i].y)-(x-poly[i].x)*(poly[j].y-poly[i].y)) > 0) then
          Inc(WN);
    end else
      if poly[j].y <= y then
        if (((poly[j].x-poly[i].x)*(y-poly[i].y)-(x-poly[i].x)*(poly[j].y-poly[i].y)) < 0) then
          Dec(WN);
    j := i;
  end;
  Result := (WN <> 0) or (rc);
end;


{* 
 Check if a point is within a polygon/shape by the given outline points (poly)
 The points must be in order, as if you would draw a line trough each point.
 @note: Ray casting algorithm
*}
function InPolyR(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
var j,i,H: Integer;
begin
  H := High(poly);
  j := H;
  Result := False;
  for i:=0 to H do begin
    if ((poly[i].y < y) and (poly[j].y >= y) or (poly[j].y < y) and (poly[i].y >= y)) then
      if (poly[i].x+(y-poly[i].y) / (poly[j].y-poly[i].y) * (poly[j].x-poly[i].x) < x) then
        Result := not(Result);
    j := i;
  end;
end;


{* 
 Check if a point is within a polygon/shape by the given outline points (poly)
 The points must be in order, as if you would draw a line trough each point.
 @note: Winding number algorithm
*}
function InPolyW(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
var
  wn,H,i,j:Integer;
begin
  wn := 0;
  H := High(poly);
  j := H;
  for i:=0 to H do begin
    //if ((Poly[i].x = x) and (Poly[i].y = y)) then
    //  Exit(True);
    if (poly[i].y <= y) then begin
      if (poly[j].y > y) then
        if (((poly[j].x-poly[i].x)*(y-poly[i].y)-(x-poly[i].x)*(poly[j].y-poly[i].y)) > 0) then
          Inc(wn);
    end else
      if poly[j].y <= y then
        if (((poly[j].x-poly[i].x)*(y-poly[i].y)-(x-poly[i].x)*(poly[j].y-poly[i].y)) < 0) then
          Dec(wn);
    j := i;
  end;
  Result := (wn <> 0);
end;


function InTriangle(const Pt, v1, v2, v3:TPoint): Boolean; Inline;
var
  b1,b2,b3: Boolean;
  p1,p2,p3: TPoint;
begin
  p1:=v1; p2:=v2; p3:=v3;
  if p3.y < p1.y then Exch(p1,p3);
  if p1.x > p2.x then Exch(p1,p2);
  b1 := (pt.x - p2.x) * (p1.y - p2.y) - (p1.x - p2.x) * (pt.y - p2.y) < 0;
  b2 := (pt.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (pt.y - p3.y) < 0;
  if (b1 <> b2) then Exit;
  b3 := (pt.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (pt.y - p1.y) < 0;
  if (b2 <> b3) then Exit;
  Result := True;
end;  



//============================================================================\\
{=================================== GENERAL ==================================}
{==============================================================================}
function IsPrime(n: Integer): Boolean; Inline;
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

function NextPrime(n: Integer): Integer; inline;
begin
  Inc(n);
  while Not(IsPrime(n)) do
    Inc(n);
  Result := n;
end;


function PrevPrime(n: Integer): Integer; inline;
begin
  Dec(n);
  while Not(IsPrime(n)) do
    Dec(n);
  Result := n;
end;


function NextPow2m1(n: Integer): Integer; Inline;
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





{* Select min of 3 values. *}
function Min(X,Y,Z:Extended): Extended; Inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Double): Double; Inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Single): Single; Inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Int64): Int64; Inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Int32): Int32; Inline; overload;
begin
  Result := Min(x,Min(y,z));
end;


{* Select max of 3 values. *}
function Max(X,Y,Z:Extended): Extended; Inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Double): Double; Inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Single): Single; Inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Int64): Int64; Inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Int32): Int32; Inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

end.
