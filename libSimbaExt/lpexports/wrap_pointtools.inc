{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap pointtools and related methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure exp_ScalePoint(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PPoint(Result)^ := ScalePoint(PPoint(Params^[1])^, PPoint(Params^[2])^, PInt32(Params^[3])^);
end;

function exp_SumTPA(var SE:Pointer; const TPA: TPointArray): TPoint; Cdecl;
begin
  Result := SumTPA(TPA);
end;

procedure exp_TPASplitAxis(var SE:Pointer; const TPA: TPointArray; var X:TIntArray; var Y:TIntArray); Cdecl;
begin
  TPASplitAxis(TPA, X,Y);
end;

procedure exp_TPAJoinAxis(var SE:Pointer; const X:TIntArray; const Y:TIntArray; var TPA:TPointArray); Cdecl;
begin
  TPAJoinAxis(X,Y, TPA);
end;

procedure exp_TPAFilter(var SE:Pointer; var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint); Cdecl;
begin
  TPAFilter(TPA, Shape, TopLeft);
end;

procedure exp_TPAFilterBounds(var SE:Pointer; var TPA: TPointArray; x1,y1,x2,y2:Integer); Cdecl;
begin
  TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

procedure exp_ATPAFilter(var SE:Pointer; var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:LongBool); Cdecl;
begin
  ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

function exp_TPAExtremes(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPAExtremes(TPA);
end;

function exp_MinAreaRect(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := MinAreaRect(TPA);
end;

function exp_TPACenter(var SE:Pointer; const TPA: TPointArray; Method: ECenterAlgo): TPoint; Cdecl;
begin
  Result := TPACenter(TPA, Method);
end;

procedure exp_GetAdjacent(Params: PParamArray); cdecl; export;
begin
  GetAdjacent(PPointArray(Params^[1])^, PPoint(Params^[2])^, PLongBool(Params^[3])^);
end;

procedure exp_TPACircularity(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PExtended(Result)^ := TPACircularity(PPointArray(Params^[1])^);
end; 

procedure exp_TPAConvexity(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PExtended(Result)^ := TPAConvexity(PPointArray(Params^[1])^);
end;

procedure exp_ReverseTPA(var SE:Pointer; var TPA: TPointArray); Cdecl;
begin
  ReverseTPA(TPA);
end;

procedure exp_TPARemoveDupes(var SE:Pointer; var TPA: TPointArray); Cdecl;
begin
  TPARemoveDupes(TPA);
end;

procedure exp_LongestPolyVector(var SE:Pointer; const Poly:TPointArray; var A,B:TPoint); Cdecl;
begin
  LongestPolyVector(Poly, A,B);
end;

function exp_InvertTPA(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := InvertTPA(TPA);
end;

function exp_RotateTPA(var SE:Pointer; const TPA: TPointArray; Radians: Extended): TPointArray; cdecl;
begin
  Result := RotateTPA(TPA, Radians);
end;

function exp_TPAPartition(var SE:Pointer; const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray; cdecl;
begin
  Result := TPAPartition(TPA, BoxWidth, BoxHeight);
end;

function exp_AlignTPA(var SE:Pointer; const TPA:TPointArray; Method: EAlignAlgo; var Angle:Extended): TPointArray; cdecl;
begin
  Result := AlignTPA(TPA, Method, Angle);
end;

function exp_CleanSortTPA(var SE:Pointer; const TPA: TPointArray): TPointArray; cdecl;
begin
  Result := CleanSortTPA(TPA);
end;

function exp_UniteTPA(var SE:Pointer; const TPA1, TPA2: TPointArray; RemoveDupes:LongBool): TPointArray; cdecl;
begin
  Result := UniteTPA(TPA1, TPA2, RemoveDupes);
end;

procedure exp_TPALine(var SE:Pointer; var TPA:TPointArray; const P1:TPoint; const P2: TPoint); Cdecl;
begin
  TPALine(TPA, P1,P2);
end;

function exp_ConnectTPA(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := ConnectTPA(TPA);
end;

function exp_XagonPoints(var SE:Pointer; const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; cdecl;
begin
  Result := XagonPoints(Center, Sides, Dir);
end;

function exp_TPAEllipseBase(var SE:Pointer; const Center: TPoint; RadiusX, RadiusY:Integer): TPointArray; cdecl;
begin
  Result := TPAEllipseBase(Center, RadiusX, RadiusY);
end;

function exp_TPAEllipse(var SE:Pointer; const Center: TPoint; RadX,RadY:Integer; Filled:LongBool): TPointArray; cdecl;
begin
  Result := TPAEllipse(Center, RadX,RadY, Filled);
end;

function exp_TPACircle(var SE:Pointer; const Center: TPoint; Radius:Integer; Filled:LongBool): TPointArray; cdecl;
begin
  Result := TPACircle(Center, Radius, Filled);
end;

function exp_TPASimplePoly(var SE:Pointer; const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; cdecl;
begin
  Result := TPASimplePoly(Center, Sides, Dir);
end;

function exp_TPACross(var SE:Pointer; const Center:TPoint; Radius:Int32): TPointArray; cdecl;
begin
  Result := TPACross(Center, Radius);
end;

function exp_ConvexHull(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := ConvexHull(TPA);
end;

function exp_FloodFillTPAEx(var SE:Pointer; const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:LongBool): TPointArray; cdecl;
begin
  Result := FloodFillTPAEx(TPA, Start, EightWay, KeepEdges);
end;


function exp_FloodFillTPA(var SE:Pointer; const TPA:TPointArray; const Start:TPoint; EightWay:LongBool): TPointArray; cdecl;
begin
  Result := FloodFillTPA(TPA,Start,EightWay);
end;

function exp_TPAOutline(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPAOutline(TPA);
end;

function exp_TPABorder(var SE:Pointer; const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPABorder(TPA);
end;

function exp_FloodFillPoly(var SE:Pointer; const Poly:TPointArray; EightWay:LongBool): TPointArray; cdecl;
begin
  Result := FloodFillPolygon(Poly, EightWay);
end;

function exp_ClusterTPAEx(var SE:Pointer; const TPA: TPointArray; Distx,Disty: Int32; EightWay:LongBool): T2DPointArray; cdecl;
begin
  Result := ClusterTPAEx(TPA,DistX,DistY, Eightway);
end;

function exp_ClusterTPA(var SE:Pointer; const TPA: TPointArray; Distance: Int32; EightWay:LongBool): T2DPointArray; cdecl;
begin
  Result := ClusterTPA(TPA,Distance, Eightway);
end;

function exp_TPAEdges(var SE:Pointer; const TPA: TPointArray): TPointArray; cdecl;
begin
  Result := TPAEdges(TPA);
end;



{*-----------------------------------------------------------------------------|
| Spline.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_Spline(var SE:Pointer; TPA:TPointArray; Tension:Double; Connect:LongBool): TPointArray; cdecl;
begin
  Result := Spline.Spline(TPA,Tension,connect);
end;



{*-----------------------------------------------------------------------------|
| Morphology.pas                                                               |
|-----------------------------------------------------------------------------*}
function exp_TPASkeleton(var SE:Pointer; const TPA:TPointArray; FMin,FMax:Integer): TPointArray; cdecl;
begin
  Result := TPASkeleton(TPA,FMin,FMax);
end;

function exp_TPAReduce(var SE:Pointer; const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray; cdecl;
begin
  Result := TPAReduce(TPA,FMin,FMax,Iterations);
end;

function exp_TPAExpand(var SE:Pointer; const TPA:TPointArray; Iterations:Integer): TPointArray; cdecl;
begin
  Result := TPAExpand(TPA,Iterations);
end;

function exp_DistTransform(var SE:Pointer; const data:TIntArray; w,h:Int32; distanceUnit:EDistUnit): T2DIntArray; cdecl;
begin
  Result := DistanceTransform(data,w,h, distanceUnit);
end;

function exp_DistTransformP(var SE:Pointer; const TPA:TPointArray; distanceUnit:EDistUnit): T2DIntArray; cdecl;
begin
  Result := DistanceTransform(TPA, distanceUnit);
end;

procedure exp_TPAHeatSort(var SE:Pointer; var Arr: TPointArray; distanceUnit:EDistUnit); cdecl;
begin
  TPAHeatSort(Arr, distanceUnit);
end;

{*-----------------------------------------------------------------------------|
| DensityMap.pas                                                               |
|-----------------------------------------------------------------------------*}
function exp_DensityMapNormed(var SE:Pointer; const TPA:TPointArray; Radius, Passes, Beta:Integer): T2DIntArray; cdecl;
begin
  Result := DensityMapNormed(TPA,Radius,Passes,Beta);
end;

procedure exp_TPADensitySort(var SE:Pointer; var Arr: TPointArray; Radius, Passes:Integer); cdecl; 
begin
  TPADensitySort(Arr,Radius,Passes);
end;
