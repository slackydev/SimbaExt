{*=========================================================================================|
| Points.pas                                                                               |
|=========================================================================================*}
function SimbaExt.ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;  
begin
  Result := exp_ScalePoint(Center, Pt, Radius);
end;


function SimbaExt.SumTPA(TPA: TPointArray): TPoint;  
begin
  Result := exp_SumTPA(TPA);
end;

procedure SimbaExt.TPASplitAxis(TPA: TPointArray; var X:TIntegerArray; var Y:TIntegerArray);  
begin
  exp_TPASplitAxis(TPA, X,Y);
end;


procedure SimbaExt.TPAJoinAxis(X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);  
begin
  exp_TPAJoinAxis(X,Y, TPA);
end;

procedure SimbaExt.TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);  
begin
  exp_TPAFilter(TPA, Shape, TopLeft);
end;

procedure SimbaExt.TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);  
begin
  exp_TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

procedure SimbaExt.ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);  
begin
  exp_ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

function SimbaExt.TPAExtremes(TPA:TPointArray): TPointArray;  
begin
  exp_TPAExtremes(TPA,Result);
end;

function SimbaExt.TPABBox(TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function SimbaExt.TPABoundingBox(TPA:TPointArray): TPointArray;  
begin
  exp_TPABBox(TPA,Result);
end;

function SimbaExt.TPACenter(TPA: TPointArray; Method: TxCenterMethod; Inside:Boolean): TPoint;  
begin
  Result := exp_TPACenter(TPA, Method, Inside);
end;


procedure SimbaExt.GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);  
begin
  exp_GetAdjacent(adj, n, EightWay);
end;

function SimbaExt.TPACircularity(TPA: TPointArray): Extended;
begin
  Result := exp_TPACircularity(TPA);
end; 

function SimbaExt.TPAConvexity(TPA: TPointArray): Extended;
begin
  Result := exp_TPAConvexity(TPA);
end;

procedure SimbaExt.ReverseTPA(var TPA: TPointArray);  
begin
  exp_ReverseTPA(TPA);
end;

procedure SimbaExt.TPARemoveDupes(var TPA: TPointArray);  
begin
  exp_TPARemoveDupes(TPA);
end;


procedure SimbaExt.LongestPolyVector(Poly:TPointArray; var A,B:TPoint);  
begin
  exp_LongestPolyVector(Poly, A,B);
end;

function SimbaExt.InvertTPA(TPA:TPointArray): TPointArray;  
begin
  exp_InvertTPA(TPA, Result);
end;

function SimbaExt.RotateTPA(TPA: TPointArray; Radians: Extended): TPointArray;  
begin
  exp_RotateTPA(TPA, Radians, Result);
end;

function SimbaExt.TPAPartition(TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;  
begin
  exp_TPAPartition(TPA, BoxWidth, BoxHeight,Result);
end;

function SimbaExt.AlignTPA(TPA:TPointArray; Method: TxAlignMethod; var Angle:Extended): TPointArray;  
begin
  exp_AlignTPA(TPA, Method, Angle,Result);
end;

function SimbaExt.CleanSortTPA(TPA: TPointArray): TPointArray;  
begin
  exp_CleanSortTPA(TPA,Result);
end;


function SimbaExt.UniteTPA(TPA1, TPA2: TPointArray; RemoveDupes:Boolean):  TPointArray;  
begin
  exp_UniteTPA(TPA1, TPA2, RemoveDupes,Result);
end;


procedure SimbaExt.TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);  
begin
  exp_TPALine(TPA, P1,P2);
end;

function SimbaExt.ConnectTPA(TPA:TPointArray): TPointArray;  
begin
  exp_ConnectTPA(TPA, Result);
end;

function SimbaExt.ConnectTPAEx(TPA:TPointArray; Tension:Extended):  TPointArray;  
begin
  exp_ConnectTPAEx(TPA, Tension, Result);
end;

function SimbaExt.XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;  
begin
  exp_XagonPoints(Center, Sides, Dir,Result);
end;

function SimbaExt.TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean=False): TPointArray;  
begin
  exp_TPAEllipse(Center,RadX,RadY,Filled,Result);
end;

function SimbaExt.TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean=False): TPointArray;   
begin
  exp_TPACircle(Center,Radius,Filled,Result);
end;

function SimbaExt.TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;     
begin
  exp_TPASimplePoly(Center, Sides, Dir, Result);
end;

function SimbaExt.ConvexHull(TPA:TPointArray):  TPointArray;  
begin
  exp_ConvexHull(TPA,Result);
end;

function SimbaExt.FloodFillTPAEx(TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean):  TPointArray;  
begin
  exp_FloodFillTPAEx(TPA, Start, EightWay, KeepEdges, Result);
end;


function SimbaExt.FloodFillTPA(TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillTPA(TPA,Start,EightWay, Result);
end;

function SimbaExt.TPAOutline(TPA:TPointArray): TPointArray;  
begin
  exp_TPAOutline(TPA, Result);
end;

function SimbaExt.TPABorder(TPA:TPointArray): TPointArray;  
begin
  exp_TPABorder(TPA, Result);
end;

function SimbaExt.FloodFillPolygon(Poly:TPointArray; EightWay:Boolean): TPointArray;  
begin
  exp_FloodFillPolygon(Poly, EightWay, Result);
end;

function SimbaExt.ClusterTPAEx(TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPAEx(TPA,DistX,DistY, Eightway, Result);
end;

function SimbaExt.ClusterTPA(TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;  
begin
  exp_ClusterTPA(TPA,Distance, Eightway, Result);
end;

function SimbaExt.TPAEdges(TPA: TPointArray): TPointArray;  
begin
  exp_TPAEdges(TPA, Result);
end;



{*=========================================================================================|
| Spline.pas                                                                              |
|=========================================================================================*}
//Another day..





{*=========================================================================================|
| Morphology.pas                                                                           |
|=========================================================================================*}
function SimbaExt.TPASkeleton(TPA:TPointArray; FMin,FMax:Integer): TPointArray;  
begin
  exp_TPASkeleton(TPA,FMin,FMax, Result);
end;


function SimbaExt.TPAReduce(TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;  
begin
  exp_TPAReduce(TPA,FMin,FMax,Iterations,Result);
end;

function SimbaExt.TPAExpand(TPA:TPointArray; Iterations:Integer): TPointArray;  
begin
  exp_TPAExpand(TPA,Iterations, Result);
end;