{!DOCTOPIC}{ 
  Type » TPoint
}
{$IFDEF SE_TOSTR}
function ToString(x:TPoint): String; override;
begin
  Result := Format('[%d,%d]', [x.x, x.y]);
end;
{$ENDIF}


{!DOCREF} {
  @method: function TPoint.Magnitude(): Extended;
  @desc: Returns the magnitude/length of the tpoint
}
function TPoint.Magnitude(): Extended;
begin
  Result := Sqrt(Sqr(Self.x) + Sqr(Self.y));
end;


{!DOCREF} {
  @method: function TPoint.Random(xR,yR: Int32): TPoint;
  @desc: Adds a random value in the range c'-xR..xR' and c'-yR..yR' to the point.
}
function TPoint.Random(xR,yR: Int32): TPoint;
begin
  Result.x := Self.x + RandomRange(-xR,xR);
  Result.y := Self.y + RandomRange(-yR,yR);
end;


{!DOCREF} {
  @method: function TPoint.Random(R: Int32): TPoint;
  @desc: Adds a random value in the range c'-R..R' to the point.
}
function TPoint.Random(R: Int32): TPoint; overload;
begin
  Result.x := Self.x + RandomRange(-R,R);
  Result.y := Self.y + RandomRange(-R,R);
end;


{!DOCREF} {
  @method: function TPoint.DistanceTo(Pt:TPoint): Extended;
  @desc: Returns the Distance from Pt
}
function TPoint.DistanceTo(Pt:TPoint): Extended;
begin
  Result := se.DistEuclidean(Self, Pt);
end;


{!DOCREF} {
  @method: function TPoint.DistanceToLine(sA, sB:TPoint): Extended;
  @desc: Returns the Distance from given line segment defined by sA-sB
}
function TPoint.DistanceToLine(sA, sB:TPoint): Extended;
begin
  Result := se.DistToLine(Self, sA, sB);
end;


{!DOCREF} {
  @method: procedure TPoint.Offset(pt:TPoint);
  @desc: Moves the point
}
{$IFNDEF SRL6}
procedure TPoint.Offset(pt:TPoint);
begin
  Self.x := Self.x + pt.x;
  Self.y := Self.y + pt.y;
end;
{$ENDIF}

{!DOCREF} {
  @method: function TPoint.Rotate(Angle:Extended; cx,cy:Integer): TPoint;
  @desc: Rotates the point, lazy method (returns a new point).
}
{$IFNDEF SRL6}
function TPoint.Rotate(Angle:Extended; cx,cy:Integer): TPoint;
{$ELSE}
function TPoint._Rotate(Angle:Extended; cx,cy:Integer): TPoint;
{$ENDIF}
begin
  Result := RotatePoint(Self, Angle, cx,cy); 
end;


{!DOCREF} {
  @method: function TPoint.Flip(): TPoint;
  @desc: x->y, y->x
}
function TPoint.Flip(): TPoint;
begin
  Result := Point(Self.y,Self.x);
end;


{!DOCREF} {
  @method: function TPoint.EQ(PT:TPoint): Boolean;
  @desc: Compares "EQual"
}
function TPoint.EQ(PT:TPoint): Boolean;
begin
  Result := (Self.x = PT.x) and (Self.y = PT.y);
end;


{!DOCREF} {
  @method: function TPoint.LT(PT:TPoint): Boolean;
  @desc: Compares "Less Then"
}
function TPoint.LT(PT:TPoint): Boolean;
begin
  Result := (Self.X < PT.X) and (Self.Y < PT.Y);
end;


{!DOCREF} {
  @method: function TPoint.GT(PT:TPoint): Boolean;
  @desc: Compares "Greater Then"
}
function TPoint.GT(PT:TPoint): Boolean;
begin
  Result := not((Self.X <= PT.X) and (Self.Y <= PT.Y));
end;


{!DOCREF} {
  @method: function TPoint.Compare(Pt:TPoint): Int32;
  @desc: Compares the two points. Result = (-1,0,1);
}
function TPoint.Compare(Pt:TPoint): Int32;
begin
  if (Self.x = PT.x) and (Self.y = PT.y) then
    Exit(0);
  if (Self.X <= PT.X) and (Self.Y <= PT.Y) then
    Exit(-1)
  else
    Exit(1);
end;


{$IFNDEF SRL6}
{!DOCREF} {
  @method: procedure TPoint.Swap(var PT:TPoint);
  @desc: Swaps the points
}
procedure TPoint.Swap(var PT:TPoint);
var tmp:TPoint;
begin
  tmp  := Self;
  Self := Pt;
  PT   := tmp;
end;
{$ENDIF}


{!DOCREF} {
  @method: function TPoint.InBox(B:TBox): Boolean;
  @desc: Checks if the Point is within the given box.
}
function TPoint.InBox(B:TBox): Boolean;
begin
  Result := InRange(Self.x, B.x1, B.x2) and InRange(Self.y, B.y1, B.y2);
end;


{!DOCREF} {
  @method: function TPoint.AngleTo(PT: TPoint): Extended;
  @desc: Computes the angle between c'Self' and c'PT'.
}
function TPoint.AngleTo(PT: TPoint): Extended;
begin
  Result := ArcTan2(-(PT.y-Self.y), (PT.x-Self.x));
end;
