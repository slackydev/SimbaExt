(*=============================================================================|
 Some basic Move functionallity.
|=============================================================================*)
procedure se_Move(const InArr:AnsiString; var DestArr:AnsiString; Source, Dest, Size:Integer); overload;
begin
  exp_Move(InArr, DestArr, Source, Dest, Size);
end;

procedure se_Move(const InArr:TIntArray; var DestArr:TIntArray; Source, Dest, Size:Integer); overload;
begin
  exp_Move(InArr, DestArr, Source, Dest, Size);
end;

procedure se_Move(const InArr:TExtArray; var DestArr:TExtArray; Source, Dest, Size:Integer); overload;
begin
  exp_Move(InArr, DestArr, Source, Dest, Size);
end;

procedure se_Move(const InArr:TPointArray; var DestArr:TPointArray; Source, Dest, Size:Integer); overload;
begin
  exp_Move(InArr, DestArr, Source, Dest, Size);
end;


(*=============================================================================|
 TBox functionality (prefix "se" for SRL compatiblity)
|=============================================================================*)
function TBox.seWidth(): Integer;
begin
  Result := (X2-X1+1);
end;

function TBox.seHeight(): Integer;
begin
  Result := (Y2-Y1+1);
end;

//gives the area the box covers
function TBox.seArea(): Integer;
begin
  Result := Self.seWidth() * Self.seHeight();
end;

//gives the center of the box
function TBox.seCenter(): TPoint;
begin
  Result.X := Self.X1 + (Self.seWidth() shr 1);
  Result.Y := Self.Y1 + (Self.seHeight() shr 1);
end;

//expand/(shrink if negative) the TBox by sizechange.
procedure TBox.seExpand(const SizeChange: Integer);
begin
  Self.X1 := Self.X1 - SizeChange;
  Self.Y1 := Self.Y1 - SizeChange;
  Self.X2 := Self.X2 + SizeChange;
  Self.Y2 := Self.Y2 + SizeChange;
end;

//Return true if a point is inside the rectangle.
function TBox.seContains(Pt:TPoint): Boolean;
begin
  Result := (self.x1 <= pt.x) and (pt.x <= self.x2) and
            (self.y1 <= pt.y) and (pt.y <= self.y2);
end;  
  
//Return true if a this box overlaps the other box.
function TBox.seOverlaps(Other:TBox): Boolean;
begin
  Result:= (self.x2 > other.x1) and (self.x1 < other.x2) and
           (self.y1 < other.y2) and (self.y2 > other.y1);
end;

//Combine two boxes - Lazy (does not expand on current)
function TBox.seCombine(Other:TBox): TBox;
begin
  Result := ToBox(Min(Min(Other.X1, Other.X2), Min(Self.X1, Self.X2)),
                  Min(Min(Other.Y1, Other.Y2), Min(Self.Y1, Self.Y2)),
                  Max(Max(Other.X1, Other.X2), Max(Self.X1, Self.X2)),
                  Max(Max(Other.Y1, Other.Y2), Max(Self.Y1, Self.Y2)));
end;  

//Return a TPA of the corner points (clockwise).
function TBox.seToCoords(): TPointArray;
begin
  Result := [Point(self.x1,self.y1), Point(self.x2,self.y1), 
             Point(self.x2,self.y2), Point(self.x1,self.y2)];
end;


(*=============================================================================|
 TPoint functionality (prefix "se" for SRL compatiblity)
|=============================================================================*)
//Restuns the magnitude/length of the tpoint
function TPoint.seMagnitude(): Extended;
begin
  Result := Sqrt(Sqr(Self.x) + Sqr(Self.y));
end;

//Returns the Distance from Pt
function TPoint.seDistanceTo(Pt:TPoint): Extended;
begin
  Result := se_DistEuclidean(Self, Pt);
end;

//Returns the Distance from given line segment defined by sA-sB
function TPoint.seDistanceToLine(sA, sB:TPoint): Extended;
begin
  Result := se_DistToLine(Self, sA, sB);
end;

//Moves the point
procedure TPoint.seOffset(Pt:TPoint);
begin
  Self.x := Self.x + Pt.x;
  Self.y := Self.y + Pt.y;
end;

//Rotates the point, lazy method (returns a new point).
function TPoint.seRotate(Angle:Extended; cx,cy:Integer): TPoint;
begin
  Result := RotatePoint(Self, Angle, cx,cy); 
end;