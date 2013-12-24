Unit XT_Math;
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
  XT_Types, Math, SysUtils;
  
  
function Radians(Dgrs: Extended): Extended; Inline;
function Degrees(Rads: Extended): Extended; Inline;
function Modulo(X,Y:Extended): Extended; Inline; 
function DeltaAngle(DegA,DegB:Extended): Extended; Inline; 
function DistManhattan(const pt1,pt2: TPoint): Extended; Inline; 
function DistEuclidean(const pt1,pt2: TPoint): Extended; Inline; 
function DistChebyshev(const pt1,pt2: TPoint): Extended; Inline; 
function DistOctagonal(const pt1,pt2: TPoint): Extended; Inline; 
function InCircle(const  Pt,Center:TPoint; Radius: Integer): Boolean; Inline; 
function InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean; Inline; 
function InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; Inline; 
function InPoly(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InPolyR(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InPolyW(x,y:Integer; const Poly:TPointArray): Boolean; Inline; 
function InBox(const Pt:TPoint; X1,Y1, X2,Y2: Integer): Boolean; Inline;
function IsPrime(n: Integer): Boolean; Inline;


//--------------------------------------------------
implementation

{*
 Converts Degrees in to radian
*}
function Radians(Dgrs: Extended): Extended; Inline;
begin Result := Dgrs * (Pi/180); end;

{*
 Converts Radian in to Degrees
*}
function Degrees(Rads: Extended): Extended; Inline;
begin Result := Rads * (180/Pi); end;

{*
 "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and many more modern programming languages.
*}
function Modulo(X,Y:Extended): Extended; Inline; 
var d: Single;
begin
  d := X / Y;
  Result := (d - floor(d)) * Y;
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
  Result := (X*X*YRad*YRad)+(Y*Y*XRad*XRad) <= (YRad*YRad*XRad*XRad);
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



//============================================================================\\
{=================================== GENERAL ==================================}
{==============================================================================}
function IsPrime(n: Integer): Boolean; Inline;
var i:Integer; Hi: Single;
begin
  if (n = 2) then Exit(True);
  if (n mod 2 = 0) or (n<=1) then Exit(False);
  Hi := Sqrt(n)+1;
  i := 3;
  while i <= Hi do begin
    if ((n mod i) = 0) then Exit(False);
    i := i + 2;
  end;
  Result := True;
end;

end.
