Unit Spline;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface
uses
  CoreTypes, Math, SysUtils;


function __Tangent(const p0,p1:TFPoint; T:Extended): TFPoint; Inline;
function Tangents(const Points:TFPointArray; Tension:Extended): TFPointArray; Inline;
function Interpolate(t:Extended; const p0,m0,p1,m1:TFPoint): TFPoint; Inline;
function CSPlinePoints(const p0,m0,p1,m1:TFPoint): TFPointArray; Inline;
function CSplineTFPA(const Pts:TFPointArray; Tension:Extended): TFPointArray;
function CSpline(TPA:TPointArray; Tension:Extended; Connect:Boolean): TPointArray;


//--------------------------------------------------
implementation
uses
  PointTools;


function __Tangent(const p0,p1:TFPoint; T:Extended): TFPoint; Inline;
begin
  Result := FPoint(T * (p1.x - p0.x), T * (p1.y - p0.y)); 
end;


function Tangents(const Points:TFPointArray; Tension:Extended): TFPointArray; Inline;
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

function Interpolate(t:Extended; const p0,m0,p1,m1:TFPoint): TFPoint; Inline;
var
  fa,fb,fc,fd:Extended;
begin
    fA := ((1 + 2 * t) * Sqr(1 - t)); 
    fB := (t * Sqr(1 - t)); 
    fC := (Sqr(t) * (3 - 2 * t)); 
    fD := (Sqr(t) * (t - 1)); 
    Result.x := (p0.x*fA) + (m0.x*fB) + (p1.x*fC) + (m1.x*fD);
    Result.y := (p0.y*fA) + (m0.y*fB) + (p1.y*fC) + (m1.y*fD);
end;


function CSPlinePoints(const p0,m0,p1,m1:TFPoint): TFPointArray; Inline;
var
  L,i: Integer;
  domain, delta, t:Extended;
  Pt,Last: TFPoint; 
begin
  Domain := Max(Abs(p0.x - p1.x), Abs(p0.y - p1.y));
  Delta := 1.0 / Domain;
    
  Last := Interpolate(0, p0, m0, p1, m1);
  SetLength(Result, 1);
  Result[0] := Last;
  L := 1;  
  T := Delta;  
  i := 0;
  while (T < 1) do
  begin
    Pt := Interpolate(T, p0, m0, p1, m1);
    if ((pt.x <> Last.x) and (pt.y <> Last.y)) then
    begin 
      if (i mod 3 = 0) then 
      begin
        SetLength(Result, L+1);
        Result[L] := Pt;
        Inc(L);
      end;
      Inc(i);
      Last := Pt;
    end;
    T := T + Delta   
  end;
end;


function CSplineTFPA(const Pts:TFPointArray; Tension:Extended): TFPointArray;
var
 Tngs,Fpts: TFPointArray; 
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
    FPts := CSPlinePoints(Pts[i], Tngs[i], Pts[i + 1], Tngs[i + 1]);
    H := High(FPts);
    SetLength(Result, L + H + 1);
    for j:=0 to H do
    begin 
    Result[L+j] := FPts[j];
    end; 
    L := (L + H);
    SetLength(FPts, 0);
  end;
  SetLength(Result, L+1);
  Result[L] := Pts[High(Pts)];
  SetLength(Tngs, 0);
end;


function CSpline(TPA:TPointArray; Tension:Extended; Connect:Boolean): TPointArray;
var 
  FPts: TFPointArray;
  TMP: TPointArray;
  i,j,h: Integer;
  f,t:TPoint;
begin
  TPARemoveDupes(TPA);
  FPts := CSplineTFPA(TPAToTFPA(TPA), Tension);
  case Connect of
  False: Result := TFPAToTPA(FPts);
  True:
    begin 
      TMP := TFPAToTPA(FPts);
      H := High(TMP);
      for i:=0 to H-1 do
      begin
        j := i+1;
        f := TMP[i];
        t := TMP[j];
        if not(SamePoints(f, t)) then
          TPALine(Result, f, t)
        else begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := f;
        end;
      end;
      SetLength(TMP, 0);
    end;
  end;
  SetLength(FPts, 0); 
end;

end.



