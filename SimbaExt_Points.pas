{*=========================================================================================|
| Points.pas                                                                               |
|=========================================================================================*}
function se_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;  
begin
  Result := exp_ScalePoint(Center, Pt, Radius);
end;


function se_SumTPA(const TPA: TPointArray): TPoint;  
begin
  Result := exp_SumTPA(TPA);
end;

procedure se_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntegerArray);  
begin
  exp_TPASplitAxis(TPA, X,Y);
end;


procedure se_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);  
begin
  exp_TPAJoinAxis(X,Y, TPA);
end;

procedure se_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);  
begin
  exp_TPAFilter(TPA, Shape, TopLeft);
end;

procedure se_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);  
begin
  exp_TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

procedure se_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);  
begin
  exp_ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

function se_TPAExtremes(const TPA:TPointArray): TPointArray;  
begin
  exp_TPAExtremes(TPA,Result);
end;

function se_TPABBox(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function se_TPABoundingBox(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function se_TPACenter(const TPA: TPointArray; Method: TxCenterMethod; Inside:Boolean): TPoint;  
begin
  Result := exp_TPACenter(TPA, Method, Inside);
end;


procedure se_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);  
begin
  exp_GetAdjacent(adj, n, EightWay);
end;

function se_TPACircularity(const TPA: TPointArray): Extended;
begin
  Result := exp_TPACircularity(TPA);
end; 

function se_TPAConvexity(const TPA: TPointArray): Extended;
begin
  Result := exp_TPAConvexity(TPA);
end;

procedure se_ReverseTPA(var TPA: TPointArray);  
begin
  exp_ReverseTPA(TPA);
end;

procedure se_TPARemoveDupes(var TPA: TPointArray);  
begin
  exp_TPARemoveDupes(TPA);
end;


procedure se_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);  
begin
  exp_LongestPolyVector(Poly, A,B);
end;

function se_InvertTPA(const TPA:TPointArray): TPointArray;  
begin
  exp_InvertTPA(TPA, Result);
end;

function se_RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended): TPointArray;  
begin
  exp_RotateTPAEx(TPA, Center, Radians, Result);
end;

function se_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;  
begin
  exp_TPAPartition(TPA, BoxWidth, BoxHeight,Result);
end;

function se_AlignTPA(const TPA:TPointArray; Method: TxAlignMethod; var Angle:Extended): TPointArray;  
begin
  exp_AlignTPA(TPA, Method, Angle,Result);
end;

function se_CleanSortTPA(const TPA: TPointArray): TPointArray;  
begin
  exp_CleanSortTPA(TPA,Result);
end;


function se_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean):  TPointArray;  
begin
  exp_UniteTPA(TPA1, TPA2, RemoveDupes,Result);
end;


procedure se_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);  
begin
  exp_TPALine(TPA, P1,P2);
end;

function se_ConnectTPA(const TPA:TPointArray): TPointArray;  
begin
  exp_ConnectTPA(TPA, Result);
end;

function se_ConnectTPAEx(TPA:TPointArray; Tension:Extended):  TPointArray;  
begin
  exp_ConnectTPAEx(TPA, Tension, Result);
end;

function se_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;  
begin
  exp_XagonPoints(Center, Sides, Dir,Result);
end;

procedure se_TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer);  
begin
  exp_TPAEllipse(TPA,Center,RadX,RadY);
end;

procedure se_TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer);  
begin
  exp_TPACircle(TPA,Center,Radius);
end;

procedure se_TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint);  
begin
  exp_TPASimplePoly(TPA, Center, Sides, Dir);
end;

function se_ConvexHull(const TPA:TPointArray):  TPointArray;  
begin
  exp_ConvexHull(TPA,Result);
end;

function se_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean):  TPointArray;  
begin
  exp_FloodFillTPAEx(TPA, Start, EightWay, KeepEdges, Result);
end;


function se_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillTPA(TPA,Start,EightWay, Result);
end;

function se_TPAOutline(const TPA:TPointArray): TPointArray;  
begin
  exp_TPAOutline(TPA, Result);
end;

function se_TPABorder(const TPA:TPointArray): TPointArray;  
begin
  exp_TPABorder(TPA, Result);
end;

function se_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillPolygon(Poly, EightWay, Result);
end;

function se_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPAEx(TPA,DistX,DistY, Eightway, Result);
end;

function se_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPA(TPA,Distance, Eightway, Result);
end;

function se_TPAEdges(const TPA: TPointArray): TPointArray;  
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
function se_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray;  
begin
  exp_TPASkeleton(TPA,FMin,FMax, Result);
end;


function se_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;  
begin
  exp_TPASkeleton(TPA,FMin,FMax, Result);
end;

function se_TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray;  
begin
  exp_TPAExpand(TPA,Iterations, Result);
end;