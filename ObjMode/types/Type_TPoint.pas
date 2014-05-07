{!DOCTOPIC}{ 
  Type » TPoint
}
{!DOCREF} {
  @method: function TPoint.Magnitude(): Extended;
  @desc: Returns the magnitude/length of the tpoint
}
function TPoint.Magnitude(): Extended;
begin
  Result := Sqrt(Sqr(Self.x) + Sqr(Self.y));
end;


{!DOCREF} {
  @method: function TPoint.DistanceTo(Pt:TPoint): Extended;
  @desc: Returns the Distance from Pt
}
function TPoint.DistanceTo(Pt:TPoint): Extended;
begin
  Result := Math.DistEuclidean(Self, Pt);
end;


{!DOCREF} {
  @method: function TPoint.DistanceToLine(sA, sB:TPoint): Extended;
  @desc: Returns the Distance from given line segment defined by sA-sB
}
function TPoint.DistanceToLine(sA, sB:TPoint): Extended;
begin
  Result := Math.DistToLine(Self, sA, sB);
end;


{!DOCREF} {
  @method: procedure TPoint.Offset(Pt:TPoint);
  @desc: Moves the point
}
procedure TPoint.Offset(Pt:TPoint);
begin
  Self.x := Self.x + Pt.x;
  Self.y := Self.y + Pt.y;
end;


{!DOCREF} {
  @method: function TPoint.Rotate(Angle:Extended; cx,cy:Integer): TPoint;
  @desc: Rotates the point, lazy method (returns a new point).
}
function TPoint.Rotate(Angle:Extended; cx,cy:Integer): TPoint;
begin
  Result := RotatePoint(Self, Angle, cx,cy); 
end;


{!DOCREF} {
  @method: function TPoint.Equal(Pt:TPoint): Boolean;
  @desc: Compares equal
}
function TPoint.Equal(Pt:TPoint): Boolean;
begin
  Result := (Self.x = Pt.x) and (Self.y = Pt.y);
end;


{!DOCREF} {
  @method: function TPoint.Compare(Pt:TPoint): TComparator;
  @desc: Compares the two points. Result = (__LT__,__EQ__,__GT__);
}
function TPoint.Compare(Pt:TPoint): TComparator;
begin
  if (Self.x = PT.x) and (Self.y = PT.y) then
    Exit(__EQ__);
  if (Self.X <= PT.X) and (Self.Y <= PT.Y) then
    Exit(__LT__) 
  else
    Exit(__GT__);
    
end;
