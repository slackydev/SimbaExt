Unit Spline;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Note: I think it's bugged
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
interface
uses
  CoreTypes, Math, SysUtils;

type
  TPointF = packed record
    X:Double;
    Y:Double;
  end;
  TPointFArray = array of TPointF;
  
function Spline(TPA:TPointArray; Tension:Double; Connect:Boolean): TPointArray;

//--------------------------------------------------
implementation
uses
  PointTools;

function Point(X,Y:Double): TPointF; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;
  
function TPFAToTPA(Arr:TPointFArray): TPointArray;
var i:Int32;
begin
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
    Result[i] := CoreTypes.Point(Round(Arr[i].x), Round(Arr[i].y));
end;


function TPAToTPFA(Arr:TPointArray): TPointFArray;
var i:Int32;
begin
  SetLength(Result, Length(Arr));
  for i:=0 to High(Arr) do
  begin
    Result[i].x := Arr[i].x;
    Result[i].y := Arr[i].y;
  end;
end;


function __Tangent(const p0,p1:TPointF; T:Double): TPointF; inline;
begin
  Result := Point(T * (p1.x - p0.x), T * (p1.y - p0.y));
end;


function Tangents(const Points:TPointFArray; Tension:Double): TPointFArray; inline;
var
  L,i: Integer;
begin
  L := Length(points);
  if L = 0 then Exit;
  if L = 1 then 
  begin   
    SetLength(Result, 1);
    Result[0] := Points[0];
    Exit;
  end;

  SetLength(Result, L);
  Result[0] := __Tangent(points[0], points[1], tension); 
  for i:=1 to L-2 do
    Result[i] :=  __Tangent(points[i-1], points[i+1], tension);
  Result[L-1] := __Tangent(points[L-2], points[L-1], tension);  
end;

function Interpolate(t:Double; const p0,m0,p1,m1:TPointF): TPointF; inline;
var
  fa,fb,fc,fd:Double;
begin
    fA := ((1 + 2 * t) * Sqr(1 - t)); 
    fB := (t * Sqr(1 - t)); 
    fC := (Sqr(t) * (3 - 2 * t)); 
    fD := (Sqr(t) * (t - 1)); 
    Result.x := (p0.x*fA) + (m0.x*fB) + (p1.x*fC) + (m1.x*fD);
    Result.y := (p0.y*fA) + (m0.y*fB) + (p1.y*fC) + (m1.y*fD);
end;


function SplinePoints(const p0,m0,p1,m1:TPointF): TPointFArray; inline;
var
  L,i: Integer;
  delta, t:Double;
  pt,last: TPointF; 
begin
  delta := 1.0 / Max(Abs(p0.x - p1.x), Abs(p0.y - p1.y));
    
  last := Interpolate(0, p0, m0, p1, m1);
  SetLength(Result, 1);
  Result[0] := last;
  L := 1;  
  T := delta;  
  i := 0;
  while (T < 1) do
  begin
    pt := Interpolate(T, p0, m0, p1, m1);
    if (pt.y <> last.y) or (pt.x <> last.x) then
    begin
      if (i mod 3 = 0) then 
      begin
        SetLength(Result, L+1);
        Result[L] := pt;
        Inc(L);
      end;
      Inc(i);
      last := pt;
    end;
    T := T + delta;   
  end;
end;


function SplineEx(const Pts:TPointFArray; Tension:Double): TPointFArray;
var
 Tngs,Fpts: TPointFArray; 
 i,j,l,h:Integer;
begin
  if Length(Pts) < 2 then
  begin
    Result := Pts; 
    Exit;
  end;

  Tngs := Tangents(Pts, Tension);
  L := 0; 
  for i:=0 to High(Pts)-1 do
  begin  
    FPts := SplinePoints(Pts[i], Tngs[i], Pts[i + 1], Tngs[i + 1]);
    H := High(FPts);
    SetLength(Result, L + H + 1);
    for j:=0 to H do
      Result[L+j] := FPts[j];
    L := (L + H);
    SetLength(FPts, 0);
  end;
  SetLength(Result, L+1);
  Result[L] := Pts[High(Pts)];
  SetLength(Tngs, 0);
end;


function Spline(TPA:TPointArray; Tension:Double; Connect:Boolean): TPointArray;
var 
  FPts: TPointFArray;
  TMP: TPointArray;
  i,j,h: Integer;
  f,t:TPoint;
begin
  TPARemoveDupes(TPA);
  FPts := SplineEx(TPAToTPFA(TPA), Tension);
  TMP := TPFAToTPA(FPts);
  if Connect then
  begin
    H := High(TMP);
    for i:=0 to H-1 do
    begin
      j := i+1;
      f := TMP[i];
      t := TMP[j];
      if not(f = t) then
        TPALine(Result, f, t)
      else begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)] := f;
      end;
    end;
  end
  else
    Result := TMP;
end;

end.



