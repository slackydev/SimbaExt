{!DOCTOPIC}{ 
  Type » T2DExtArray
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
  @method: function T2DExtArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DExtArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DExtArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DExtArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Extend(Arr:T2DExtArray);
  @desc: Extends the 2d-array with a 2d-array
}
procedure T2DExtArray.Extend(Arr:T2DExtArray);
var L,i:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  for i:=L to High(Self) do
  begin
    SetLength(Self[i],Length(Arr[i-L]));
    MemMove(Arr[i-L][0], Self[i][0], Length(Arr[i-L])*SizeOf(Extended));
  end;
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
  @desc: 
    Sorts a copy of the ATEA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
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
  @desc: 
    Sorts the ATIA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
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
begin
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DExtArray.Reverse();
begin
  Self := Self.Slice(-1,0,-1);
end;




{=============================================================================}
// The functions below this line is not in the standard array functionality
//
// By "standard array functionality" I mean, functions that all standard
// array types should have.
{=============================================================================}





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
  @method: function T2DExtArray.Mean(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DExtArray.Mean(): Extended;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
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
  @method: function T2DExtArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function T2DExtArray.Variance(): Extended;
var
  Arr:TExtArray;
  avg:Extended;
  i:Int32;
begin
  Arr := Self.Merge();
  avg := Arr.Mean();
  for i:=0 to High(Arr) do
    Result := Result + Sqr(Arr[i] - avg);
  Result := Result / length(Arr);
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
