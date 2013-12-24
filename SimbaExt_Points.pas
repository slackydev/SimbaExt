{*=========================================================================================|
| Points.pas                                                                               |
|=========================================================================================*}
function XT_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;  
begin
  Result := exp_ScalePoint(Center, Pt, Radius);
end;


function XT_SumTPA(const TPA: TPointArray): TPoint;  
begin
  Result := exp_SumTPA(TPA);
end;

procedure XT_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntegerArray);  
begin
  exp_TPASplitAxis(TPA, X,Y);
end;


procedure XT_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);  
begin
  exp_TPAJoinAxis(X,Y, TPA);
end;

procedure XT_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);  
begin
  exp_TPAFilter(TPA, Shape, TopLeft);
end;

procedure XT_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);  
begin
  exp_TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

procedure XT_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);  
begin
  exp_ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

function XT_TPAExtremes(const TPA:TPointArray): TPointArray;  
begin
  exp_TPAExtremes(TPA,Result);
end;

function XT_TPABBox(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function XT_TPABoundingBox(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function XT_TPACenter(const TPA: TPointArray; Method: TxCenterMethod; Inside:Boolean): TPoint;  
begin
  Result := exp_TPACenter(TPA, Method, Inside);
end;


procedure XT_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);  
begin
  exp_GetAdjacent(adj, n, EightWay);
end;

function XT_TPACircularity(const TPA: TPointArray): Extended;
begin
  Result := exp_TPACircularity(TPA);
end; 

function XT_TPAConvexity(const TPA: TPointArray): Extended;
begin
  Result := exp_TPAConvexity(TPA);
end;

procedure XT_ReverseTPA(var TPA: TPointArray);  
begin
  exp_ReverseTPA(TPA);
end;

procedure XT_TPARemoveDupes(var TPA: TPointArray);  
begin
  exp_TPARemoveDupes(TPA);
end;


procedure XT_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);  
begin
  exp_LongestPolyVector(Poly, A,B);
end;

function XT_InvertTPA(const TPA:TPointArray): TPointArray;  
begin
  exp_InvertTPA(TPA, Result);
end;

function XT_RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended): TPointArray;  
begin
  exp_RotateTPAEx(TPA, Center, Radians, Result);
end;

function XT_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;  
begin
  exp_TPAPartition(TPA, BoxWidth, BoxHeight,Result);
end;

function XT_AlignTPA(const TPA:TPointArray; Method: TxAlignMethod; var Angle:Extended): TPointArray;  
begin
  exp_AlignTPA(TPA, Method, Angle,Result);
end;

function XT_CleanSortTPA(const TPA: TPointArray): TPointArray;  
begin
  exp_CleanSortTPA(TPA,Result);
end;


function XT_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean):  TPointArray;  
begin
  exp_UniteTPA(TPA1, TPA2, RemoveDupes,Result);
end;


procedure XT_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);  
begin
  exp_TPALine(TPA, P1,P2);
end;

function XT_ConnectTPA(const TPA:TPointArray): TPointArray;  
begin
  exp_ConnectTPA(TPA, Result);
end;

function XT_ConnectTPAEx(TPA:TPointArray; Tension:Extended):  TPointArray;  
begin
  exp_ConnectTPAEx(TPA, Tension, Result);
end;

function XT_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;  
begin
  exp_XagonPoints(Center, Sides, Dir,Result);
end;

procedure XT_TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer);  
begin
  exp_TPAEllipse(TPA,Center,RadX,RadY);
end;

procedure XT_TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer);  
begin
  exp_TPACircle(TPA,Center,Radius);
end;

procedure XT_TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint);  
begin
  exp_TPASimplePoly(TPA, Center, Sides, Dir);
end;

function XT_ConvexHull(const TPA:TPointArray):  TPointArray;  
begin
  exp_ConvexHull(TPA,Result);
end;

function XT_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean):  TPointArray;  
begin
  exp_FloodFillTPAEx(TPA, Start, EightWay, KeepEdges, Result);
end;


function XT_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillTPA(TPA,Start,EightWay, Result);
end;

function XT_TPAOutline(const TPA:TPointArray): TPointArray;  
begin
  exp_TPAOutline(TPA, Result);
end;

function XT_TPABorder(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABorder(TPA, Result);
end;

function XT_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillPolygon(Poly, EightWay, Result);
end;

function XT_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPAEx(TPA,DistX,DistY, Eightway, Result);
end;

function XT_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPA(TPA,Distance, Eightway, Result);
end;

function XT_TPAEdges(const TPA: TPointArray): TPointArray;  
begin
  exp_TPAEdges(TPA, Result);
end;





{*=========================================================================================|
| CSpline.pas                                                                              |
|=========================================================================================*}
//Another day..


{*=========================================================================================|
| TPAExtShape.pas                                                                          |
|=========================================================================================*}
//How about never?






{*=========================================================================================|
| Morphology.pas                                                                           |
|=========================================================================================*}
function XT_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray;  
begin
  exp_TPASkeleton(TPA,FMin,FMax, Result);
end;


function XT_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;  
begin
  exp_TPASkeleton(TPA,FMin,FMax, Result);
end;

function XT_TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray;  
begin
  exp_TPAExpand(TPA,Iterations, Result);
end;