{!DOCTOPIC}{ 
  Type » TPointArray
}

{!DOCREF} {
  @method: function TPointArray.Clone(): TPointArray;
  @desc: Returns a copy of the array
}
function TPointArray.Clone(): TPointArray;
begin
  Result := Copy(Self);
end;


{!DOCREF} {
  @method: function TPointArray.Len(): Int32;
  @desc: Returns the length of the TPA. Same as `Length(TPA)`
}
function TPointArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TPointArray.IsEmpty(): Boolean;
  @desc: Returns True if the TPA is empty. Same as `Length(TPA) = 0`
}
function TPointArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TPointArray.Append(const PT:TPoint);
  @desc: Add another TP to the TPA
}
procedure TPointArray.Append(const PT:TPoint);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := PT;
end;



{!DOCREF} {
  @method: procedure TPointArray.Del(idx:Int32);
  @desc: Removes the element at the given index `idx`
}
procedure TPointArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(TPoint));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TPointArray.Remove(Value:TPoint);
  @desc: Removes the first element from left which is equal to `Value`
}
procedure TPointArray.Remove(Value:TPoint);
begin
  Self.Del( Self.Find(Value) );
end;




{!DOCREF} {
  @method: function TPointArray.Pop(): TPoint;
  @desc: Removes and returns the last item in the array
}
function TPointArray.Pop(): TPoint;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TPointArray.PopLeft(): TPoint;
  @desc: Removes and returns the first item in the array
}
function TPointArray.PopLeft(): TPoint;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(TPoint)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TPointArray.Slice(Start,Stop: Int32; Step:Int32=1): TPointArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If `Start >= Stop`, and `Step <= -1` it will result in reversed output.
    
    [note]Don't pass positive `Step`, combined with `Start > Stop`, that is undefined[/note]
}
function TPointArray.Slice(Start,Stop: Int32; Step:Int32=1): TPointArray;
begin
  if Step = 0 then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except end;
end;


{!DOCREF} {
  @method: procedure TPointArray.Extend(TPA:TPointArray);
  @desc: Extends the TPA with a TPA
}
procedure TPointArray.Extend(TPA:TPointArray);
begin
  Self := se.UniteTPA(Self, TPA, False);
end; 


{!DOCREF} {
  @method: function TPointArray.Find(Value:TPoint): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TPointArray.Find(Value:TPoint): Int32;
begin
  Result := exp_Find(Self,[Value]);
end;


{!DOCREF} {
  @method: function TPointArray.Find(Sequence:TPointArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TPointArray.Find(Sequence:TPointArray): Int32; overload;
begin
  Result := exp_Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TPointArray.FindAll(Value:TPoint): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TPointArray.FindAll(Value:TPoint): TIntArray;
begin
  Result := exp_FindAll(Self,[Value]);
end;


{!DOCREF} {
  @method: function TPointArray.FindAll(Sequence:TPointArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TPointArray.FindAll(Sequence:TPointArray): TIntArray; overload;
begin
  Result := exp_FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TPointArray.Contains(Pt:TPoint): Boolean;
  @desc: Checks if the TPA contains the given TPoint `PT`
}
function TPointArray.Contains(Pt:TPoint): Boolean;
begin
  Result := Self.Find(PT) <> -1;
end;


{!DOCREF} {
  @method: function TPointArray.Count(Pt:TPoint): Boolean;
  @desc: Checks if the TPA contains the given TPoint `PT`
}
function TPointArray.Count(Pt:TPoint): Boolean;
begin
  Result := Length(Self.FindAll(PT));
end;


{!DOCREF} {
  @method: function TPointArray.Sorted(Key:TSortKey=sort_Default): TPointArray;
  @desc: 
    Sorts a copy of the TPA
    Supported keys: `sort_Default, sort_Magnitude, sort_ByRow, sort_ByColumn, sort_ByX, sort_ByY`
}
function TPointArray.Sorted(Key:TSortKey=sort_Default): TPointArray;
begin
  Result := Self.Clone();
  case Key of
    sort_Default, sort_Magnitude: se.SortTPA(Result);
    sort_ByRow: se.SortTPAByRow(Result);
    sort_ByColumn: se.SortTPAByColumn(Result);
    sort_ByX: se.SortTPAByX(Result);
    sort_ByY: se.SortTPAByY(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;

{!DOCREF} {
  @method: function TPointArray.Sorted(From:TPoint): TPointArray; overload;
  @desc: Sorts a copy of the TPA from ..
}
function TPointArray.Sorted(From:TPoint): TPointArray; overload;
begin
  Result := Self.Clone();
  se.SortTPAFrom(Result, From)
end;


{!DOCREF} {
  @method: procedure TPointArray.Sort(Key:TSortKey=sort_Default);
  @desc: 
    Sorts the TPA
    Supported keys: c'sort_Default, sort_Magnitude, sort_ByRow, sort_ByColumn, sort_ByX, sort_ByY'
}
procedure TPointArray.Sort(Key:TSortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Magnitude: se.SortTPA(Self);
    sort_ByRow: se.SortTPAByRow(Self);
    sort_ByColumn: se.SortTPAByColumn(Self);
    sort_ByX: se.SortTPAByX(Self);
    sort_ByY: se.SortTPAByY(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;

{!DOCREF} {
  @method: procedure TPointArray.Sort(From:TPoint); overload;
  @desc: Sorts the TPA from ..
}
procedure TPointArray.Sort(From:TPoint); overload;
begin
  se.SortTPAFrom(Self, From)
end;


{!DOCREF} {
  @method: procedure TPointArray.Reverse();
  @desc: Reverses the TPA
}
procedure TPointArray.Reverse();
begin
  Self := Self.Slice(-1,0,-1);
end; 


{!DOCREF} {
  @method: function TPointArray.Reversed(): TPointArray;
  @desc: Returns a reversed copy of the TPA
}
function TPointArray.Reversed(): TPointArray;
begin
  Result := Self.Slice(-1,0,-1);
end; 






{=============================================================================}
// The functions below this line is not in the standard array functionality
//
// By "standard array functionality" I mean, functions that all standard
// array types should have.
{=============================================================================}





{!DOCREF} {
  @method: function TPointArray.Combine(TPA:TPointArray): TPointArray;
  @desc: Combines two TPAs and returns the resulting TPA
}
function TPointArray.Combine(TPA:TPointArray): TPointArray;
begin
  Result := se.UniteTPA(Self, TPA, False);
end; 


{!DOCREF} {
  @method: function TPointArray.Bounds(): TBox;
  @desc: Returns the squared minimum bounding box covering the TPA
}
function TPointArray.Bounds(): TBox;
begin
  Result := GetTPABounds(Self);
end;


{!DOCREF} {
  @method: function TPointArray.BoundingBox(): TPointArray;
  @desc: Returns the minimum bounding recatangle covering the TPA (four TPoint)
}
function TPointArray.BoundingBox(): TPointArray;
begin
  Result := se.TPABBox(Self);
end;


{!DOCREF} {
  @method: function TPointArray.ConvexHull(): TPointArray;
  @desc: Returns the convex hull of the points
}
function TPointArray.ConvexHull(): TPointArray;
begin
  Result := se.ConvexHull(Self);
end;


{!DOCREF} {
  @method: function TPointArray.Invert(): TPointArra
  @desc: Inverts the TPA based on the bounds of the TPA, so each point within the bounds, but not in the TPA is returned
}
function TPointArray.Invert(): TPointArray;
begin
  Result := se.InvertTPA(self);
end; 

 
{!DOCREF} {
  @method: function TPointArray.Cluster(Dist:Int32; Eightway:Boolean=True): T2DPointArray;
  @desc: Clusters the TPA in to groups separated by a given minimum distance
}
function TPointArray.Cluster(Dist:Int32; Eightway:Boolean=True): T2DPointArray;
begin
  Result := se.ClusterTPA(Self, dist, eightway);
end;


{!DOCREF} {
  @method: function TPointArray.ClusterEx(Distx, Disty:Int32; Eightway:Boolean=True): T2DPointArray;
  @desc: Clusters the TPA in to groups separated by a given minimum distance horizontally, and vertiacally
}
function TPointArray.ClusterEx(Distx, Disty:Int32; Eightway:Boolean=True): T2DPointArray;
begin
  Result := se.ClusterTPAEx(Self, distx,disty, eightway);
end;


{!DOCREF} {
  @method: function TPointArray.Partition(Width, Height:Int32): T2DPointArray;
  @desc: Splits the TPA in to boxes of the given size
}
function TPointArray.Partition(Width, Height:Int32): T2DPointArray;
begin
  se.TPAPartition(Self, Width, Height);
end;


{!DOCREF} {
  @method: function TPointArray.Mean(): TPoint;
  @desc: Returns the geometric mean of the TPA
}
function TPointArray.Mean(): TPoint;
begin
  Result := MiddleTPA(Self); 
end;


{!DOCREF} {
  @method: function TPointArray.Center(Method:TCenterAlgo): TPoint;
  @desc: Returns the center of the TPA, defined by the given method
}
function TPointArray.Center(Method:TCenterAlgo): TPoint;
begin
  Result := se.TPACenter(Self, method, False); 
end;


{!DOCREF} {
  @method: function TPointArray.Rotate(Angle:Extended): TPointArray;
  @desc: 
    Rotates the TPA
    [note][b]Not[/b] the same as RotatePoints in Simba![/note]
}
function TPointArray.Rotate(Angle:Extended): TPointArray;
begin
  Result := se.RotateTPA(Self, Angle);
end;


{!DOCREF} {
  @method: function TPointArray.RotatePts(Angle:Extended; CX,CY: Int32): TPointArray;
  @desc: 
    Rotates the TPA, but each point is threated "induvidually"
    [note]The same as RotatePoints in Simba![/note]
}
function TPointArray.RotatePts(Angle:Extended; CX,CY: Int32): TPointArray;
begin
  Result := RotatePoints(Self, Angle, CX,CY);
end;


{!DOCREF} {
  @method: procedure TPointArray.Offset(OffX,OffY: Int32);
  @desc: offsets each point in the TPA, both horizontally, and vertically by the given amount
}
procedure TPointArray.Offset(OffX,OffY: Int32);
begin
  OffsetTPA(Self, Point(OffX, OffY));
end;


{!DOCREF} {
  @method: function TPointArray.Sum(): TPoint;
  @desc: Adds up the array and returns the sum from each axis
}
function TPointArray.Sum(): TPoint;
begin
  Result := se.SumTPA(Self);
end;
