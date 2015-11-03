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

function ftoi(x:double): Int32;
function ftoi(x:Int64): Int32; inline; overload; //dummy

function Radians(Dgrs: Double): Double; inline;
function Degrees(Rads: Double): Double; inline;

{* Random guassian *}
function Gauss(mu,sigma:Double): Double; inline;
function TruncedGauss(lo,hi:Double): Double; inline;

{* "Real" modulus operation *}
function Modulo(X,Y:Double):  Double;  inline; overload;
function Modulo(X,Y:Single):  Single;  inline; overload;
function Modulo(X,Y:Int64): Int64; inline; overload;
function Modulo(X,Y:Int32): Int32; inline; overload;

{* Extracts the sign of a number *}
function Sign(X:Double): Int32; inline; overload;
function Sign(X:Int64): Int32; inline; overload;

{* Cuberoot approximation *}
function fCbrt(x:Single): Single;

{* Angle between two angles *}
function DeltaAngle(DegA,DegB:Double): Double; inline;

{* Distance calcs *}
function DistManhattan(const pt1,pt2: TPoint): Double; inline; 
function DistEuclidean(const pt1,pt2: TPoint): Double; inline; 
function DistChebyshev(const pt1,pt2: TPoint): Double; inline; 
function DistOctagonal(const pt1,pt2: TPoint): Double; inline;
function DistToLine(Pt, sA, sB: TPoint): Double; inline;

{* In shape *}
function InCircle(const  Pt,Center:TPoint; Radius: Int32): Boolean; inline; 
function InEllipse(const Pt,Center:TPoint; YRad, XRad: Int32): Boolean; inline; 
function InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; inline; 
function InPoly(x,y:Int32; const Poly:TPointArray): Boolean; inline; 
function InPolyR(x,y:Int32; const Poly:TPointArray): Boolean; inline; 
function InPolyW(x,y:Int32; const Poly:TPointArray): Boolean; inline; 
function InBox(const Pt:TPoint; X1,Y1, X2,Y2: Int32): Boolean; inline;
function InTriangle(const Pt, v1, v2, v3:TPoint): Boolean; inline;

{* Prime *}
function IsPrime(n: Int32): Boolean; inline;
function NextPrime(n: Int32): Int32; inline;
function PrevPrime(n: Int32): Int32; inline;

{* Next power of two minus 1 *}
function NextPow2m1(n: Int32): Int32; inline;

{* Select min of 3 values *}
function Min(X,Y,Z:Double): Double; inline; overload;
function Min(X,Y,Z:Single): Single; inline; overload;
function Min(X,Y,Z:Int64): Int64; inline; overload;
function Min(X,Y,Z:Int32): Int32; inline; overload;

{* Select max of 3 values *}
function Max(X,Y,Z:Double): Double; inline; overload;
function Max(X,Y,Z:Single): Single; inline; overload;
function Max(X,Y,Z:Int64): Int64; inline; overload;
function Max(X,Y,Z:Int32): Int32; inline; overload;



const 
  E:Extended = 2.718281828459;

//--------------------------------------------------
implementation
uses CoreMisc;

{*
  Faster alternative to Trunc. Well.. when available
  ftoi = FloatToInt
*}
function ftoi(x:double): Int32;
begin
  {$IFDEF CPU386}
  {$ASMMODE intel} //SSE3
  asm
    fld		x
    fisttp	Result
  end;
  {$ELSE}
    //Fallback
    Result := Trunc(x);
  {$ENDIF}
end;

function ftoi(x:Int64): Int32; overload;
begin
  Result := x;
end;

{* Converts Degrees in to radian *}
function Radians(Dgrs: Double): Double; Inline;
begin Result := Dgrs * (Pi/180); end;

{* Converts Radian in to Degrees *}
function Degrees(Rads: Double): Double; Inline;
begin Result := Rads * (180/Pi); end;


{* Random *}
function __random(): Double; inline; //never ever return 0 (can't have any div by zero errors)
begin Result := Max(Random(),1.0e-14); end;

function Gauss(mu,sigma:Double): Double;
var len:Double;
begin
  len := sigma * Sqrt(-2 * Ln(__random()));
  Result := mu + len * Cos(2 * PI * __random());
end;

function TruncedGauss(lo,hi:Double): Double;
const LIMIT = 5.5;
begin
  Result := Sqrt(-2 * Ln(__random())) * Cos(2 * PI * __random());
  Result := Min(Abs(Result), LIMIT);
  Result := Result / LIMIT * (Hi-Lo) + Lo;
end;


{*
 "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and other "modern" programming languages.
*}
function Modulo(X,Y:Double): Double; inline; overload;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Single): Single; inline; overload;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Int32): Int32; inline; overload;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Int64): Int64; inline; overload;
begin Result := X - Floor(X / Y) * Y; end;


{* 
  Extracts the sign of a number 
*}
function Sign(X:Double): Int32; Inline; overload;
begin 
  if (x > 0) then Exit(1) else if (x < 0) then Exit(-1) else Exit(0); 
end;

function Sign(X:Int64): Int32; Inline; overload;
begin 
  if (x > 0) then Exit(1) else if (x < 0) then Exit(-1) else Exit(0); 
end;


{*
 Fast "approximation" of cuberoot (Accuracy ±0.001%) using SSE2
 Falls back to using `Power` if SSE is not available
 
 Most x86 Intel/AMD/VIA CPU produced in+after 2004 got SSE2
*}
function fCbrt(x:Single): Single;
const
  three:Single = 3.0;
begin
  {$IFDEF CPU386}
  {$ASMMODE intel}
  asm
    mov		eax,	x
    movss	xmm2,	x
    movss	xmm1,	three
    mov		ecx,	eax		//Int magic
    and		eax,	$7FFFFFFF
    sub		eax,	$3F800000
    sar		eax,	10
    imul	eax,	341
    add		eax,	$3F800000
    and		eax,	$7FFFFFFF
    and		ecx,	$80000000
    or		eax,	ecx
    mov		Result,	eax

    movss	xmm0,	Result		//iteration 1
    movss	xmm3,	xmm0
    mulss	xmm3,	xmm0
    movss	xmm4,	xmm3
    mulss	xmm3,	xmm1
    rcpss	xmm3,	xmm3
    mulss	xmm4,	xmm0
    subss	xmm4,	xmm2
    mulss	xmm4,	xmm3
    subss	xmm0,	xmm4

    movss	xmm3,	xmm0		//iteration 2
    mulss	xmm3,	xmm0
    movss	xmm4,	xmm3
    mulss	xmm3,	xmm1
    rcpss	xmm3,	xmm3
    mulss	xmm4,	xmm0
    subss	xmm4,	xmm2
    mulss	xmm4,	xmm3
    subss	xmm0,	xmm4

    movss	Result,	xmm0;
  end;
  {$ELSE}
    //Fallback for ARM
    Result := x**1/3;
  {$ENDIF}
end;


{*
 Computes the delta of two angles. The result is in range of -180..180.
*}
function DeltaAngle(DegA,DegB:Double): Double; inline;
begin
  Result := Modulo((DegA - DegB + 180), 360) - 180;
end;




//============================================================================\\
{============================ DISTANCE CALCULATIONS ===========================}
{==============================================================================}

{*
 Manhattan distance is a simple, and cheap way to get distnace between two points
*}
function DistManhattan(const pt1,pt2: TPoint): Double; inline;
begin
  Result := (Abs(pt1.x - pt2.x) + Abs(pt1.y - pt2.y));
end;

{*
 Distance measured in a streight line from pt1 to pt2.
 Uses pythagorean theorem... 
*}
function DistEuclidean(const pt1,pt2: TPoint): Double; inline;
begin
  Result := Sqrt(Sqr(pt1.x - pt2.x) + Sqr(pt1.y - pt2.y));
end;

{*
 Distance in the form of "amount of steps" in a any direction. 
 EG: Think of the 8 possible moves the King can do on a chessboard, that = 1 distnace. 
*}
function DistChebyshev(const pt1,pt2: TPoint): Double; inline;
begin
  Result := Max(Abs(pt1.x - pt2.x), Abs(pt1.y - pt2.y));
end;

{*
 Distance is measured as a middle-thing of Manhattan and Chebyshev-distance.
 This results in eight 45degr corners, aka a octagon. 
 It's close to as fast as Chebyshev, and Manhattan.
*}
function DistOctagonal(const pt1,pt2: TPoint): Double; inline;
var dx,dy:Int32;
const SQRT2_M1 = 0.414213562;
begin
  dx := Abs(pt1.x - pt2.x);
  dy := Abs(pt1.y - pt2.y);
  if dx >= dy then Result := dx + (dy * SQRT2_M1)
  else Result := dy + (dx * SQRT2_M1);
end;


{*
 Distance from Pt to the line-segment defined by sA-sB.
*}
function DistToLine(Pt, sA, sB: TPoint): Double; inline;
var
  dx,dy,d:Int32;
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
function InCircle(const Pt, Center: TPoint; Radius: Int32): Boolean; inline;
begin
  Result := Sqr(Pt.X - Center.X) + Sqr(Pt.Y - Center.Y) <= Sqr(Radius);
end;

  
  
{*
 Check if a point is within a ellipse.
*}
function InEllipse(const Pt,Center:TPoint; YRad, XRad: Int32): Boolean; inline;
var
  X, Y: Int32;
begin
  X := Pt.X - Center.X;
  Y := Pt.Y - Center.Y;
  Result := (Sqr(X)*Sqr(YRad))+(Sqr(Y)*Sqr(XRad)) <= (Sqr(YRad)*Sqr(XRad));
end; 


{*
 Is the coordiants within a rectangle (defined by four points)?
 > C is not actually used, but for future extension/changes, i'll leave it here.
*}
function InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; inline;
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
function InBox(const Pt:TPoint; X1,Y1,X2,Y2:Int32): Boolean; inline;
begin
  Result:= (Pt.X >= X1) and (Pt.X <= X2) and
           (Pt.Y >= Y1) and (Pt.Y <= Y2);
end;


{* 
 Check if a point is within a polygon/shape by the given outline points (poly)
 The points must be in order, as if you would draw a line trough each point.
 @note: Ray casting combined with Winding number algorithm
*}
function InPoly(x,y:Int32; const Poly:TPointArray): Boolean; inline;
var
  WN,H,i,j:Int32;
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
  Result := (WN <> 0) or RC;
end;


{* 
 Check if a point is within a polygon/shape by the given outline points (poly)
 The points must be in order, as if you would draw a line trough each point.
 @note: Ray casting algorithm
*}
function InPolyR(x,y:Int32; const Poly:TPointArray): Boolean; inline;
var j,i,H: Int32;
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
function InPolyW(x,y:Int32; const Poly:TPointArray): Boolean; inline;
var
  wn,H,i,j:Int32;
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


function InTriangle(const Pt, v1, v2, v3:TPoint): Boolean; inline;
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
function IsPrime(n: Int32): Boolean; inline;
var i:Int32; Hi: Single;
begin
  if (n = 2) then Exit(True);
  if (n and 2 = 0) or (n<=1) then Exit(False);
  Hi := Sqrt(n)+1;
  i := 3;
  while i <= Hi do begin
    if (n mod i = 0) then Exit(False);
    i := i + 2;
  end;
  Result := True;
end;

function NextPrime(n: Int32): Int32; inline;
begin
  Inc(n);
  while Not(IsPrime(n)) do Inc(n);
  Result := n;
end;


function PrevPrime(n: Int32): Int32; inline;
begin
  Dec(n);
  while Not(IsPrime(n)) do Dec(n);
  Result := n;
end;


function NextPow2m1(n: Int32): Int32; inline;
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
function Min(X,Y,Z:Double): Double; inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Single): Single; inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Int64): Int64; inline; overload;
begin
  Result := Min(x,Min(y,z));
end;

function Min(X,Y,Z:Int32): Int32; inline; overload;
begin
  Result := Min(x,Min(y,z));
end;


{* Select max of 3 values. *}
function Max(X,Y,Z:Double): Double; inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Single): Single; inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Int64): Int64; inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

function Max(X,Y,Z:Int32): Int32; inline; overload;
begin
  Result := Max(x,Max(y,z));
end;

end.
