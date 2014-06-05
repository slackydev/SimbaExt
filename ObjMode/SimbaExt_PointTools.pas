{!DOCTOPIC}{ 
  PointTools
}

{!DOCREF} {
  @method: function se.ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;
  @desc: ...
}
function SimbaExt.ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;  
begin
  Result := exp_ScalePoint(Center, Pt, Radius);
end;


{!DOCREF} {
  @method: procedure se.SumTPA(TPA: TPointArray): TPoint;  
  @desc: Sums the TPA and returns a TPoint with the sum of X, and Y
}
function SimbaExt.SumTPA(TPA: TPointArray): TPoint;  
begin
  Result := exp_SumTPA(TPA);
end;

{!DOCREF} {
  @method: procedure se.TPASplitAxis(TPA: TPointArray; var X:TIntArray; var Y:TIntArray);  
  @desc: Separates the axis in the TPA, returning two TIntArrays, one for each axis
}
procedure SimbaExt.TPASplitAxis(TPA: TPointArray; var X:TIntArray; var Y:TIntArray);  
begin
  exp_TPASplitAxis(TPA, X,Y);
end;


{!DOCREF} {
  @method: procedure se.TPAJoinAxis(X:TIntArray; const Y:TIntArray; var TPA:TPointArray);    
  @desc: Joins two axis to create a TPA, takes two TIntArrays, one for each axis
}
procedure SimbaExt.TPAJoinAxis(X:TIntArray; const Y:TIntArray; var TPA:TPointArray);  
begin
  exp_TPAJoinAxis(X,Y, TPA);
end;

{!DOCREF} {
  @method: procedure se.TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);
  @desc: ...
}
procedure SimbaExt.TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);  
begin
  exp_TPAFilter(TPA, Shape, TopLeft);
end;

{!DOCREF} {
  @method: procedure se.TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);
  @desc: ...
}
procedure SimbaExt.TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);  
begin
  exp_TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

{!DOCREF} {
  @method: procedure se.ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);
  @desc: ...
}
procedure SimbaExt.ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);  
begin
  exp_ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

{!DOCREF} {
  @method: function se.TPAExtremes(TPA:TPointArray): TPointArray;
  @desc: ...
}
function SimbaExt.TPAExtremes(TPA:TPointArray): TPointArray;  
begin
  Result := exp_TPAExtremes(TPA);
end;

{!DOCREF} {
  @method: function se.TPABBox(TPA:TPointArray): TPointArray;
  @desc: Returns the minimum bounding rectangle that can fit around the given TPA. Rectangle is represented by four TPoints.
}
function SimbaExt.TPABBox(TPA:TPointArray): TPointArray;  
begin
  Result := exp_TPABBox(TPA);
end;

{!DOCREF} {
  @method: function se.TPABoundingBox(TPA:TPointArray): TPointArray;
  @desc: Same as se.TPABBox
}
function SimbaExt.TPABoundingBox(TPA:TPointArray): TPointArray;  
begin
  Result := exp_TPABBox(TPA);
end;

{!DOCREF} {
  @method: function se.TPACenter(TPA: TPointArray; @method: TCenterAlgo; Inside:Boolean): TPoint;
  @desc: ...
}
function SimbaExt.TPACenter(TPA: TPointArray; method: TCenterAlgo; Inside:Boolean): TPoint;
begin
  Result := exp_TPACenter(TPA, method, Inside);
end;

{!DOCREF} {
  @method: function se.GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);
  @desc: ...
}
procedure SimbaExt.GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);  
begin
  exp_GetAdjacent(adj, n, EightWay);
end;

{!DOCREF} {
  @method: function se.TPACircularity(TPA: TPointArray): Extended;
  @desc: Returns the circularity ratio '0.0 - 1.0' of the given TPA
}
function SimbaExt.TPACircularity(TPA: TPointArray): Extended;
begin
  Result := exp_TPACircularity(TPA);
end; 

{!DOCREF} {
  @method: function se.TPAConvexity(TPA: TPointArray): Extended;
  @desc: Returns the convexity ratio '0.0 - 1.0' of the given TPA
}
function SimbaExt.TPAConvexity(TPA: TPointArray): Extended;
begin
  Result := exp_TPAConvexity(TPA);
end;

{!DOCREF} {
  @method: procedure se.ReverseTPA(var TPA: TPointArray);  
  @desc: Reverses the TPA
}
procedure SimbaExt.ReverseTPA(var TPA: TPointArray);  
begin
  exp_ReverseTPA(TPA);
end;


{!DOCREF} {
  @method: procedure se.TPARemoveDupes(var TPA: TPointArray);
  @desc: Removes all the duplicates in the TPA
}
procedure SimbaExt.TPARemoveDupes(var TPA: TPointArray);  
begin
  exp_TPARemoveDupes(TPA);
end;


{!DOCREF} {
  @method: procedure se.LongestPolyVector(Poly:TPointArray; var A,B:TPoint);
  @desc: Returns the points of the longest side in the Polygon
}
procedure SimbaExt.LongestPolyVector(Poly:TPointArray; var A,B:TPoint);  
begin
  exp_LongestPolyVector(Poly, A,B);
end;

{!DOCREF} {
  @method: function se.InvertTPA(TPA:TPointArray): TPointArray; 
  @desc: Inverts the TPA, meaning that all points within the bounds of the TPA that is NOT in the TPA will be returned
}
function SimbaExt.InvertTPA(TPA:TPointArray): TPointArray;  
begin
  Result := exp_InvertTPA(TPA);
end;

{!DOCREF} {
  @method: function se.RotateTPA(TPA: TPointArray; Radians: Extended): TPointArray;  
  @desc: Rotates the TPA
}
function SimbaExt.RotateTPA(TPA: TPointArray; Radians: Extended): TPointArray;  
begin
  Result := exp_RotateTPA(TPA, Radians);
end;

{!DOCREF} {
  @method: function se.TPAPartition(TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;
  @desc: Split the points in to partitions of BoxWidth, and BoxHeight. 
}
function SimbaExt.TPAPartition(TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;  
begin
  Result := exp_TPAPartition(TPA, BoxWidth, BoxHeight);
end;

{!DOCREF} {
  @method: function se.AlignTPA(TPA:TPointArray; method: TAlignAlgo; var Angle:Extended): TPointArray;
  @desc: Tries to align the TPA horizontally so that the longest side is faced downwards.
}
function SimbaExt.AlignTPA(TPA:TPointArray; method: TAlignAlgo; var Angle:Extended): TPointArray;
begin
  Result := exp_AlignTPA(TPA, method, Angle);
end;

{!DOCREF} {
  @method: function se.CleanSortTPA(TPA: TPointArray): TPointArray;
  @desc: Sorts the TPA by row and at the same time removes all the duplicates.
}
function SimbaExt.CleanSortTPA(TPA: TPointArray): TPointArray;  
begin
  Result := exp_CleanSortTPA(TPA);
end;


{!DOCREF} {
  @method: function se.UniteTPA(TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray;
  @desc: Combines two TPAs, if RemoveDups is 'True' then it will also remove all the duplicates
}
function SimbaExt.UniteTPA(TPA1, TPA2: TPointArray; RemoveDupes:Boolean):  TPointArray;  
begin
  Result := exp_UniteTPA(TPA1, TPA2, RemoveDupes);
end;


{!DOCREF} {
  @method: procedure se.TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);
  @desc: ...
}
procedure SimbaExt.TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);  
begin
  exp_TPALine(TPA, P1,P2);
end;


{!DOCREF} {
  @method: function se.TPACross(const center:TPoint; Radius:Int32): TPointArray;
  @desc: ...
}
function SimbaExt.TPACross(const center:TPoint; Radius:Int32): TPointArray;
var P1,P2:TPoint;
begin
  P1 := Point(center.x-Radius, center.y);
  P2 := Point(center.x+Radius, center.y);
  exp_TPALine(Result, P1,P2);
  P1 := Point(center.x, center.y-Radius);
  P2 := Point(center.x, center.y+Radius);
  exp_TPALine(Result, P1,P2);
end;



{!DOCREF} {
  @method: function se.ConnectTPA(TPA:TPointArray): TPointArray;
  @desc: ...
}
function SimbaExt.ConnectTPA(TPA:TPointArray): TPointArray;  
begin
  Result := exp_ConnectTPA(TPA);
end;

{!DOCREF} {
  @method: function se.ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray;
  @desc: ...
}
function SimbaExt.ConnectTPAEx(TPA:TPointArray; Tension:Extended):  TPointArray;  
begin
  Result := exp_ConnectTPAEx(TPA, Tension);
end;

{!DOCREF} {
  @method: function se.XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;   
  @desc: ...
}
function SimbaExt.XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;  
begin
  Result := exp_XagonPoints(Center, Sides, Dir);
end;

{!DOCREF} {
  @method: function se.TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean=False): TPointArray;   
  @desc: ...
}
function SimbaExt.TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean=False): TPointArray;  
begin
  Result := exp_TPAEllipse(Center,RadX,RadY,Filled);
end;

{!DOCREF} {
  @method: function se.TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean=False): TPointArray;   
  @desc: ...
}
function SimbaExt.TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean=False): TPointArray;   
begin
  Result := exp_TPACircle(Center,Radius,Filled);
end;

{!DOCREF} {
  @method: function se.TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;     
  @desc: ...
}
function SimbaExt.TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;     
begin
  Result := exp_TPASimplePoly(Center, Sides, Dir);
end;

{!DOCREF} {
  @method: function se.ConvexHull(TPA:TPointArray):  TPointArray;  
  @desc: 
    Returns the Convex Hull of the TPA
    [url]http://en.wikipedia.org/wiki/Convex_hull[/url]
}
function SimbaExt.ConvexHull(TPA:TPointArray):  TPointArray;  
begin
  Result := exp_ConvexHull(TPA);
end;

function SimbaExt.FloodFillTPAEx(TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean):  TPointArray;  
begin
  Result := exp_FloodFillTPAEx(TPA, Start, EightWay, KeepEdges);
end;


function SimbaExt.FloodFillTPA(TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;  
begin
  Result := exp_FloodFillTPA(TPA,Start,EightWay);
end;

{!DOCREF} {
  @method: function se.TPAOutline(TPA:TPointArray): TPointArray;
  @desc: Returns the outline of the TPA, meaning all the points on the outer edge of the TPA
}
function SimbaExt.TPAOutline(TPA:TPointArray): TPointArray;  
begin
  Result := exp_TPAOutline(TPA);
end;

{!DOCREF} {
  @method: function se.TPABorder(TPA:TPointArray): TPointArray;
  @desc: Returns the border of the TPA, meaning all the points just outside the edge of the TPA
}
function SimbaExt.TPABorder(TPA:TPointArray): TPointArray;  
begin
  Result := exp_TPABorder(TPA);
end;

{!DOCREF} {
  @method: function se.FloodFillPolygon(Poly:TPointArray; EightWay:Boolean): TPointArray;  
  @desc: Given a polygon, this function will fill all the points in that polygon and return them
}
function SimbaExt.FloodFillPolygon(Poly:TPointArray; EightWay:Boolean): TPointArray;  
begin
  Result := exp_FloodFillPolygon(Poly, EightWay);
end;

{!DOCREF} {
  @method: function se.ClusterTPAEx(TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;
  @desc: 
    This function is very similar to 'SplitTPAEx' and 'ClusterTPAEx' in Simba, only differance is the order of the result. 
    It groups the TPA in to many clusters by the given DistX, and DistY which represents the max distance (chebyshev) from each point to it's neighbor for the to create a group.
    But in almost all cases this function is faster then the "equal" simba functions.
}
function SimbaExt.ClusterTPAEx(TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;  
begin
  if Length(TPA) > 0 then
    Result := exp_ClusterTPAEx(TPA,DistX,DistY, Eightway);
end;

{!DOCREF} {
  @method: function se.ClusterTPA(TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;
  @desc: 
    This function is very similar to 'SplitTPAEx' and 'ClusterTPAEx' in Simba, only differance is the order of the result, and that it onlytakse a single param. 
    It groups the TPA in to many clusters by the given Distance which represents the max distance (chebyshev) from each point to it's neighbor for the to create a group.
    But in almost all cases this function is faster then the "equal" simba functions.
  
    [note]Do not mix this with simbas 'SplitTPA', this function clusters by using Chebyshev distance, and not Euclidean distance[/note]
}
function SimbaExt.ClusterTPA(TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;  
begin
  if Length(TPA) > 0 then
    Result := exp_ClusterTPA(TPA,Distance, Eightway);
end;


{!DOCREF} {
  @method: function se.TPAEdges(TPA: TPointArray): TPointArray;
  @desc: Returns the edges of the TPA
}
function SimbaExt.TPAEdges(TPA: TPointArray): TPointArray;  
begin
  Result := exp_TPAEdges(TPA);
end;



{*=========================================================================================|
| Spline.pas                                                                              |
|=========================================================================================*}
{!DOCREF} {
  @method: function se.Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean=False): TPointArray; 
  @desc: [warning]Not working, yet[/warning]
}
function SimbaExt.Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean=False): TPointArray;  
begin
  Result := exp_Spline(TPA, Tension, Connect);
end;



{*=========================================================================================|
| Morphology.pas                                                                           |
|=========================================================================================*}
{!DOCREF} {
  @method: function se.TPASkeleton(TPA:TPointArray; FMin,FMax:Integer): TPointArray;  
  @desc: Returns the skeleton of the TPA, best FMin, and FMax is usually 2 and 6
}
function SimbaExt.TPASkeleton(TPA:TPointArray; FMin,FMax:Integer): TPointArray;  
begin
  Result := exp_TPASkeleton(TPA,FMin,FMax);
end;

{!DOCREF} {
  @method: function se.TPAReduce(TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;
  @desc: Same as se.TPASkeleton but it allows you to decide ow much you want to reduce it by
}
function SimbaExt.TPAReduce(TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;  
begin
  Result := exp_TPAReduce(TPA,FMin,FMax,Iterations);
end;

{!DOCREF} {
  @method: function se.TPAExpand(TPA:TPointArray; Iterations:Integer): TPointArray;
  @desc: Expands the TPA by adding new layers out side the TPA
}
function SimbaExt.TPAExpand(TPA:TPointArray; Iterations:Integer): TPointArray;  
begin
  Result := exp_TPAExpand(TPA,Iterations);
end;
