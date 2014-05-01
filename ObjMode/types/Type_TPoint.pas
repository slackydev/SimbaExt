(*=============================================================================|
 TPoint functionality
|=============================================================================*)
//Restuns the magnitude/length of the tpoint
function TPoint.Magnitude(): Extended;
begin
  Result := Sqrt(Sqr(Self.x) + Sqr(Self.y));
end;

//Returns the Distance from Pt
function TPoint.DistanceTo(Pt:TPoint): Extended;
begin
  Result := Math.DistEuclidean(Self, Pt);
end;

//Returns the Distance from given line segment defined by sA-sB
function TPoint.DistanceToLine(sA, sB:TPoint): Extended;
begin
  Result := Math.DistToLine(Self, sA, sB);
end;

//Moves the point
procedure TPoint.Offset(Pt:TPoint);
begin
  Self.x := Self.x + Pt.x;
  Self.y := Self.y + Pt.y;
end;

//Rotates the point, lazy method (returns a new point).
function TPoint.Rotate(Angle:Extended; cx,cy:Integer): TPoint;
begin
  Result := RotatePoint(Self, Angle, cx,cy); 
end;