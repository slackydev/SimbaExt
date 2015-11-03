{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports PointTools and related methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

AddMethod(@exp_ScalePoint,	'function SimbaExt.ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint; native;');
AddMethod(@exp_SumTPA,		'function SimbaExt.SumTPA(const TPA: TPointArray): TPoint;');
AddMethod(@exp_TPASplitAxis,	'procedure SimbaExt.TPASplitAxis(const TPA: TPointArray; var X:TIntArray; var Y:TIntArray);');
AddMethod(@exp_TPAJoinAxis,	'procedure SimbaExt.TPAJoinAxis(const X:TIntArray; const Y:TIntArray; var TPA:TPointArray);');
AddMethod(@exp_TPAFilter,	'procedure SimbaExt.TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);');
AddMethod(@exp_TPAFilterBounds,	'procedure SimbaExt.TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);');
AddMethod(@exp_ATPAFilter,	'procedure SimbaExt.ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:LongBool);');
AddMethod(@exp_TPAExtremes,	'function SimbaExt.TPAExtremes(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_MinAreaRect,	'function SimbaExt.MinAreaRect(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_TPACenter,	'function SimbaExt.TPACenter(const TPA: TPointArray; Method: ECenterAlgo=ECA_MEAN): TPoint;');
AddMethod(@exp_GetAdjacent,	'procedure SimbaExt.GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:LongBool); native;');
AddMethod(@exp_TPACircularity,	'function SimbaExt.TPACircularity(const TPA: TPointArray): Extended; native;');
AddMethod(@exp_TPAConvexity,	'function SimbaExt.TPAConvexity(const TPA: TPointArray): Extended; native;');
AddMethod(@exp_ReverseTPA,	'procedure SimbaExt.ReverseTPA(var TPA: TPointArray);');
AddMethod(@exp_TPARemoveDupes,	'procedure SimbaExt.TPARemoveDupes(var TPA: TPointArray);');
AddMethod(@exp_LongestPolyVector,'procedure SimbaExt.LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);');
AddMethod(@exp_InvertTPA,	'function SimbaExt.InvertTPA(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_RotateTPA,	'function SimbaExt.RotateTPA(const TPA: TPointArray; Radians: Extended): TPointArray;');
AddMethod(@exp_TPAPartition,	'function SimbaExt.TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;');
AddMethod(@exp_AlignTPA,	'function SimbaExt.AlignTPA(const TPA:TPointArray; Method: EAlignAlgo; var Angle:Extended): TPointArray;');
AddMethod(@exp_CleanSortTPA,	'function SimbaExt.CleanSortTPA(const TPA: TPointArray): TPointArray;');
AddMethod(@exp_UniteTPA,	'function SimbaExt.UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:LongBool): TPointArray;');

AddMethod(@exp_TPALine,		'procedure SimbaExt.TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);');
AddMethod(@exp_ConnectTPA,	'function SimbaExt.ConnectTPA(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_XagonPoints,	'function SimbaExt.XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
AddMethod(@exp_TPAEllipseBase,	'function SimbaExt.TPAEllipseBase(const Center: TPoint; RadX, RadY:Integer): TPointArray;');
AddMethod(@exp_TPAEllipse,	'function SimbaExt.TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:LongBool): TPointArray;');
AddMethod(@exp_TPACircle,	'function SimbaExt.TPACircle(const Center: TPoint; Radius:Integer; Filled:LongBool): TPointArray;');
AddMethod(@exp_TPASimplePoly,	'function SimbaExt.TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
AddMethod(@exp_TPACross,	'function SimbaExt.TPACross(const center:TPoint; Radius:Int32): TPointArray;');

AddMethod(@exp_ConvexHull,	'function SimbaExt.ConvexHull(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_FloodFillTPAEx,	'function SimbaExt.FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:LongBool): TPointArray;');
AddMethod(@exp_FloodFillTPA,	'function SimbaExt.FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:LongBool): TPointArray;');
AddMethod(@exp_TPAOutline,	'function SimbaExt.TPAOutline(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_TPABorder,	'function SimbaExt.TPABorder(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_FloodFillPoly,	'function SimbaExt.FloodFillPoly(const Poly:TPointArray; EightWay:LongBool): TPointArray;');
AddMethod(@exp_ClusterTPAEx,	'function SimbaExt.ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Int32; EightWay:LongBool): T2DPointArray;');
AddMethod(@exp_ClusterTPA,	'function SimbaExt.ClusterTPA(const TPA: TPointArray; Distance: Int32; EightWay:LongBool): T2DPointArray;');
AddMethod(@exp_TPAEdges,	'function SimbaExt.TPAEdges(const TPA: TPointArray): TPointArray;');

AddMethod(@exp_Spline,	'function SimbaExt.Spline(TPA:TPointArray; Tension:Double; Connect:LongBool): TPointArray;');

//morph
AddMethod(@exp_TPASkeleton,	'function SimbaExt.TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray;');
AddMethod(@exp_TPAReduce,	'function SimbaExt.TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;');
AddMethod(@exp_TPAExpand,	'function SimbaExt.TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray;');
AddMethod(@exp_DistTransform,	'function SimbaExt.DistTransform(const data:TIntArray; w,h:Int32; distanceUnit:EDistUnit): T2DIntArray;');
AddMethod(@exp_DistTransformP,	'function SimbaExt.DistTransform(const TPA:TPointArray; distanceUnit:EDistUnit): T2DIntArray; overload;');
AddMethod(@exp_TPAHeatSort,	'procedure SimbaExt.TPAHeatSort(var Arr: TPointArray; distanceUnit:EDistUnit);');

//densitymap
AddMethod(@exp_TPADensitySort,	'procedure SimbaExt.DensitySort(var Arr: TPointArray; Radius, Passes:Int32); ');
AddMethod(@exp_DensityMapNormed,	'function SimbaExt.DensityMap(const TPA:TPointArray; Radius, Passes, Beta:Int32): T2DIntArray;');

