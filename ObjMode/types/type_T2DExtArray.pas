{!DOCTOPIC}{ 
  Type � T2DExtArray
}

{!DOCREF} {
  @method: function T2DExtArray.Clone(): T2DExtArray;
  @desc: Returns a copy of the array
}
function T2DExtArray.Clone(): T2DExtArray;
var i:Int32;
begin
  SetLength(Result, Length(Self));
  for i:=0 to High(Self) do
    Result[i] := Copy(Self[i]);
end;


{!DOCREF} {
  @method: function T2DExtArray.Len(): Int32;
  @desc: Returns the length of the arr. Same as c'Length(Arr)'
}
function T2DExtArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function T2DExtArray.IsEmpty(): Boolean;
  @desc: Returns True if the ATEA is empty. Same as c'Length(ATIA) = 0'
}
function T2DExtArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Append(const Arr:TExtArray);
  @desc: Add another element to the array
}
procedure T2DExtArray.Append(const Arr:TExtArray);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Arr;
end;


{!DOCREF} {
  @method: function T2DExtArray.Pop(): TExtArray;
  @desc: Removes and returns the last item in the array
}
function T2DExtArray.Pop(): TExtArray;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function T2DExtArray.Slice(Start,Stop: Int32): T2DExtArray;
  @desc: Returns a slice of the array
}
function T2DExtArray.Slice(Start,Stop: Int32): T2DExtArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); //hum hum
end;


{!DOCREF} {
  @method: function T2DExtArray.Merge(): TExtArray;
  @desc: Merges all the groups in the array, and return a TIA
}
function T2DExtArray.Merge(): TExtArray;
var i,s,l: Integer;
begin
  s := 0;
  for i:=0 to High(Self) do
  begin
    L := Length(Self[i]);
    SetLength(Result, S+L);
    MemMove(Self[i][0], Result[S], L*SizeOf(Extended));
    S := S + L;
  end; 
end;


{!DOCREF} {
  @method: function T2DExtArray.Sorted(Key:TSortKey=sort_Default): T2DExtArray;
  @desc: Sorts a copy of the ATEA with the given key, returns a copy
}
function T2DExtArray.Sorted(Key:TSortKey=sort_Default): T2DExtArray;
begin
  Result := Self.Clone();
  case Key of
    sort_Default, sort_Length: se.SortATEAByLength(Result);
    sort_Mean: se.SortATEAByMean(Result);
    sort_First: se.SortATEAByFirst(Result);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function T2DExtArray.Sorted(Index:Integer): T2DPointArray; overload;
  @desc: Sorts a copy of the ATEA by the given index in each group
}
function T2DExtArray.Sorted(Index:Integer): T2DExtArray; overload;
begin
  Result := Self.Clone();
  se.SortATEAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Sort(Key:TSortKey=sort_Default);
  @desc: Sorts the ATIA with the given key, returns a copy
}
procedure T2DExtArray.Sort(Key:TSortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATEAByLength(Self);
    sort_Mean: se.SortATEAByMean(Self);
    sort_First: se.SortATEAByFirst(Self);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Sort(Index:Integer); overload;
  @desc: 
    Sorts the ATEA by the given index in each group. If the group is not that large it will be set to the last item in that group.
    Negative 'index' will result in counting from right to left: High(Arr[i]) - index
  
}
procedure T2DExtArray.Sort(Index:Integer); overload;
begin
  se.SortATEAByIndex(Self, Index);
end;



{!DOCREF} {
  @method: function T2DExtArray.Reversed(): T2DExtArray;
  @desc: Creates a reversed copy of the array
}
function T2DExtArray.Reversed(): T2DExtArray;
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DExtArray.Reverse();
var
  i,Hi,Mid: Integer;
  tmp:TExtArray;
begin
  Hi := High(Self);
  if (Hi < 0) then Exit;
  Mid := Hi div 2;
  for i := 0 to Mid do begin
    tmp := Self[Hi-i];
    Self[Hi-i] := Self[i];
    Self[i] := tmp;
  end;
end;


{!DOCREF} {
  @method: function T2DExtArray.Sum(): Extended;
  @desc: Returns the sum of the 2d array
}
function T2DExtArray.Sum(): Extended;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;


{!DOCREF} {
  @method: function T2DExtArray.Avg(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DExtArray.Avg(): Extended;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Avg();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function T2DExtArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function T2DExtArray.Stdev(): Extended;
begin
  Result := Self.Merge().Stdev();
end;


{!DOCREF} {
  @method: function T2DExtArray.Mode(Eps:Extended=0.0000001): Extended;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
    Takes an extra parameter c'Eps', can be used to allow some tolerance in the floating point comparison.
}
function T2DExtArray.Mode(Eps:Extended=0.0000001): Extended;
begin
  Result := Self.Merge().Mode(Eps);
end;


{!DOCREF} {
  @method: function T2DExtArray.Min(): Extended;
  @desc: Returns the minimum value in the array
}
function T2DExtArray.Min(): Extended;
var _:Extended;
begin
  se.MinMaxTEA(Self.Merge(),Result,_);
end;



{!DOCREF} {
  @method: function T2DExtArray.Max(): Extended;
  @desc: Returns the maximum value in the array
}
function T2DExtArray.Max(): Extended;
var _:Extended;
begin
  se.MinMaxTEA(Self.Merge(),_,Result);
end;


{!DOCREF} {
  @method: function T2DExtArray.ArgMin(): TPoint;
  @desc: Returns the index containing the smallest element in the array
}
function T2DExtArray.ArgMin(): TPoint;
var i,j:Int32;
begin
  Result := Point(0,0);
  for i:=0 to High(self) do
  begin
    j := Self[i].ArgMin();
    if (Self[Result.y][Result.x] > Self[i][j]) then
      Result := Point(j,i);
  end;  
end;



{!DOCREF} {
  @method: function T2DExtArray.ArgMax(): TPoint;
  @desc: Returns the index containing the largest element in the array
}
function T2DExtArray.ArgMax(): TPoint;
var i,j:Int32;
begin
  Result := Point(0,0);
  for i:=0 to High(self) do
  begin
    j := Self[i].ArgMax();
    if (Self[Result.y][Result.x] < Self[i][j]) then
      Result := Point(j,i);
  end;  
end;
