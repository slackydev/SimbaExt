(*=============================================================================|
 TPointArray functionality
|=============================================================================*)
{#DOCUMENT} {
  [method]function TPointArray.Clone(): TPointArray;[/method]
  [desc]Returns a copy of the array[desc]
}{#END}
function TPointArray.Clone(): TPointArray;
begin
  Result := Copy(Self);
end;


{#DOCUMENT} {
  [method]function TPointArray.Len(): Int32;[/method]
  [desc]Returns the length of the TPA. Same as 'Length(TPA)'[/desc]
}{#END}
function TPointArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{#DOCUMENT} {
  [method]function TPointArray.IsEmpty(): Boolean;[/method]
  [desc]Returns True if the TPA is empty. Same as 'Length(TPA) = 0'[/desc]
}{#END}
function TPointArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{#DOCUMENT} {
  [method]procedure TPointArray.Append(const PT:TPoint);[/method]
  [desc]Add another TP to the TPA[/desc]
}{#END}
procedure TPointArray.Append(const PT:TPoint);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := PT;
end;


{#DOCUMENT} {
  [method]function TPointArray.Pop(): TPoint;[/method]
  [desc]Removes and returns the last item in the array[/desc]
}{#END}
function TPointArray.Pop(): TPoint;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{#DOCUMENT} {
  [method]function TPointArray.Slice(Start,Stop: Int32): TPointArray;[/method]
  [desc]Returns a slice of the array[/desc]
}{#END}
function TPointArray.Slice(Start,Stop: Int32): TPointArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop);
end;


{#DOCUMENT} {
  [method]procedure TPointArray.Extend(TPA:TPointArray);[/method]
  [desc]Extends the TPA with a TPA[/desc]
}{#END}
procedure TPointArray.Extend(TPA:TPointArray);
begin
  Self := se.UniteTPA(Self, TPA, False);
end; 


{#DOCUMENT} {
  [method]function TPointArray.Combine(TPA:TPointArray): TPointArray;[/method]
  [desc]Combines two TPAs and returns the resulting TPA[/desc]
}{#END}
function TPointArray.Combine(TPA:TPointArray): TPointArray;
begin
  Result := se.UniteTPA(Self, TPA, False);
end; 


{#DOCUMENT} {
  [method]function TPointArray.Bounds(): TBox;[/method]
  [desc]Returns the squared minimum bounding box covering the TPA[/desc]
}{#END}
function TPointArray.Bounds(): TBox;
begin
  Result := GetTPABounds(Self);
end;


{#DOCUMENT} {
  [method]function TPointArray.BoundingBox(): TPointArray;[/method]
  [desc]Returns the minimum bounding recatangle covering the TPA (four TPoint)[/desc]
}{#END}
function TPointArray.BoundingBox(): TPointArray;
begin
  Result := se.TPABBox(Self);
end;


{#DOCUMENT} {
  [method]function TPointArray.ConvexHull(): TPointArray;[/method]
  [desc]Returns the convex hull of the points[/desc]
}{#END}
function TPointArray.ConvexHull(): TPointArray;
begin
  Result := se.ConvexHull(Self);
end;


{#DOCUMENT} {
  [method]procedure TPointArray.Reverse();[/method]
  [desc]Reverses the TPA[/desc]
}{#END}
procedure TPointArray.Reverse();
begin
  se.ReverseTPA(Self);
end; 


{#DOCUMENT} {
  [method]function TPointArray.Reversed(): TPointArray;[/method]
  [desc]Returns a reversed copy of the TPA[/desc]
}{#END}
function TPointArray.Reversed(): TPointArray;
begin
  Result := Self.Clone();
  se.ReverseTPA(Result);
end; 


{#DOCUMENT} {
  [method]function TPointArray.Invert(): TPointArra[/method]
  [desc]Inverts the TPA based on the bounds of the TPA, so each point within the bounds, but not in the TPA is returned[/desc]
}{#END}
function TPointArray.Invert(): TPointArray;
begin
  Result := se.InvertTPA(self);
end; 


{#DOCUMENT} {
  [method]function TPointArray.Contains(Pt:TPoint): Boolean;[/method]
  [desc]Checks if the TPA contains the given TPoint 'Pt'[/desc]
}{#END}
function TPointArray.Contains(Pt:TPoint): Boolean;
begin
  Result := PointInTPA(Pt, Self);
end;

 
{#DOCUMENT} {
  [method]function TPointArray.Cluster(Dist:Int32; Eightway:Boolean=True): T2DPointArray;[/method]
  [desc]Clusters the TPA in to groups separated by a given minimum distance[/desc]
}{#END}
function TPointArray.Cluster(Dist:Int32; Eightway:Boolean=True): T2DPointArray;
begin
  Result := se.ClusterTPA(Self, dist, eightway);
end;


{#DOCUMENT} {
  [method]function TPointArray.ClusterEx(Distx, Disty:Int32; Eightway:Boolean=True): T2DPointArray;[/method]
  [desc]Clusters the TPA in to groups separated by a given minimum distance horizontally, and vertiacally[/desc]
}{#END}
function TPointArray.ClusterEx(Distx, Disty:Int32; Eightway:Boolean=True): T2DPointArray;
begin
  Result := se.ClusterTPAEx(Self, distx,disty, eightway);
end;


{#DOCUMENT} {
  [method]function TPointArray.Partition(Width, Height:Int32): T2DPointArray;[/method]
  [desc]Splits the TPA in to boxes of the given size[/desc]
}{#END}
function TPointArray.Partition(Width, Height:Int32): T2DPointArray;
begin
  se.TPAPartition(Self, Width, Height);
end;


{#DOCUMENT} {
  [method]function TPointArray.Mean(): TPoint;[/method]
  [desc]Returns the geometric mean of the TPA[/desc]
}{#END}
function TPointArray.Mean(): TPoint;
begin
  Result := MiddleTPA(Self); 
end;


{#DOCUMENT} {
  [method]function TPointArray.Center(Method:TxCenterMethod): TPoint;[/method]
  [desc]Returns the center of the TPA, defined by the given method[/desc]
}{#END}
function TPointArray.Center(Method:TxCenterMethod): TPoint;
begin
  Result := se.TPACenter(Self, method, False); 
end;


{#DOCUMENT} {
  [method]function TPointArray.Rotate(Angle:Extended): TPointArray;[/method]
  [desc]Rotates the TPA[/desc]
}{#END}
function TPointArray.Rotate(Angle:Extended): TPointArray;
begin
  Result := se.RotateTPA(Self, Angle);
end;


{#DOCUMENT} {
  [method]function TPointArray.RotatePts(Angle:Extended; CX,CY: Int32): TPointArray;[/method]
  [desc]Rotates the TPA, but each point is threated "induvidually"[/desc]
}{#END}
function TPointArray.RotatePts(Angle:Extended; CX,CY: Int32): TPointArray;
begin
  Result := RotatePoints(Self, Angle, CX,CY);
end;


{#DOCUMENT} {
  [method]procedure TPointArray.Offset(OffX,OffY: Int32);[/method]
  [desc]offsets each point in the TPA, both horizontally, and vertically by the given amount[/desc]
}{#END}
procedure TPointArray.Offset(OffX,OffY: Int32);
begin
  OffsetTPA(Self, Point(OffX, OffY));
end;


{#DOCUMENT} {
  [method]function TPointArray.Sum(): TPoint;[/method]
  [desc]Sum of the TPA[/desc]
}{#END}
function TPointArray.Sum(): TPoint;
begin
  Result := se.SumTPA(Self);
end;


{#DOCUMENT} {
  [method]function TPointArray.Sorted(Key:TSortKey=sort_Default): TPointArray;[/method]
  [desc]Sorts a copy of the TPA[/desc]
}{#END}
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

{#DOCUMENT} {
  [method]function TPointArray.Sorted(From:TPoint): TPointArray; overload;[/method]
  [desc]Sorts a copy of the TPA from ..[/desc]
}{#END}
function TPointArray.Sorted(From:TPoint): TPointArray; overload;
begin
  Result := Self.Clone();
  se.SortTPAFrom(Result, From)
end;


{#DOCUMENT} {
  [method]procedure TPointArray.Sort(Key:TSortKey=sort_Default);[/method]
  [desc]Sorts the TPA[/desc]
}{#END}
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

{#DOCUMENT} {
  [method]procedure TPointArray.Sort(From:TPoint); overload;[/method]
  [desc]Sorts the TPA from ..[/desc]
}{#END}
procedure TPointArray.Sort(From:TPoint); overload;
begin
  se.SortTPAFrom(Self, From)
end;
