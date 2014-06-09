{!DOCTOPIC}{ 
  Type » T2DIntArray
}

{!DOCREF} {
  @method: function T2DIntArray.Clone(): T2DIntArray;
  @desc: Returns a copy of the array
}
function T2DIntArray.Clone(): T2DIntArray;
var i:Int32;
begin
  SetLength(Result, Length(Self));
  for i:=0 to High(Self) do
    Result[i] := Copy(Self[i]);
end;


{!DOCREF} {
  @method: function T2DIntArray.Len(): Int32;
  @desc: Returns the length of the arr. Same as c'Length(Arr)'
}
function T2DIntArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function T2DIntArray.IsEmpty(): Boolean;
  @desc: Returns True if the ATIA is empty. Same as c'Length(ATIA) = 0'
}
function T2DIntArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Append(const Arr:TIntArray);
  @desc: Add another element to the array
}
procedure T2DIntArray.Append(const Arr:TIntArray);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Arr;
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Insert(idx:Int32; Value:TIntArray);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure T2DIntArray.Insert(idx:Int32; Value:TIntArray);
var i,l:Int32;
begin
  l := Length(Self);
  if (idx < 0) then
    idx := math.modulo(idx,l);

  if (l <= idx) then begin
    self.append(value);
    Exit();
  end;

  SetLength(Self, l+1);
  for i:=l downto idx+1 do
    Self[i] := Self[i-1];
  Self[i] := Value;
end;



{!DOCREF} {
  @method: function T2DIntArray.Pop(): TIntArray;
  @desc: Removes and returns the last item in the array
}
function T2DIntArray.Pop(): TIntArray;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function T2DIntArray.PopLeft(): TIntArray;
  @desc: Removes and returns the first item in the array
}
function T2DIntArray.PopLeft(): TIntArray;
begin
  Result := Self[0];
  Self := Self.Slice(1,-1);
end;


{!DOCREF} {
  @method: function T2DIntArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DIntArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DIntArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DIntArray;
begin
  if (Step = 0) then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except end;
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Extend(Arr:T2DIntArray);
  @desc: Extends the 2D-array with a 2D-array
}
procedure T2DIntArray.Extend(Arr:T2DIntArray);
var L,i:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  for i:=L to High(Self) do
  begin
    SetLength(Self[i],Length(Arr[i-L]));
    MemMove(Arr[i-L][0], Self[i][0], Length(Arr[i-L])*SizeOf(Int32));
  end;
end;


{!DOCREF} {
  @method: function T2DIntArray.Merge(): TIntArray;
  @desc: Merges all the groups in the array, and return a TIA
}
function T2DIntArray.Merge(): TIntArray;
var i,s,l: Integer;
begin
  s := 0;
  for i:=0 to High(Self) do
  begin
    L := Length(Self[i]);
    SetLength(Result, S+L);
    MemMove(Self[i][0], Result[S], L*SizeOf(Int32));
    S := S + L;
  end; 
end;


{!DOCREF} {
  @method: function T2DIntArray.Sorted(Key:TSortKey=sort_Default): T2DIntArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
function T2DIntArray.Sorted(Key:TSortKey=sort_Default): T2DIntArray;
begin
  Result := Self.Clone();
  case Key of
    sort_Default, sort_Length: se.SortATIAByLength(Result);
    sort_Mean: se.SortATIAByMean(Result);
    sort_First: se.SortATIAByFirst(Result);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function T2DIntArray.Sorted(Index:Integer): T2DIntArray; overload;
  @desc: Sorts a copy of the ATIA by the given index in each group
}
function T2DIntArray.Sorted(Index:Integer): T2DIntArray; overload;
begin
  Result := Self.Clone();
  se.SortATIAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Sort(Key:TSortKey=sort_Default);
  @desc: 
    Sorts the ATIA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
procedure T2DIntArray.Sort(Key:TSortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATIAByLength(Self);
    sort_Mean: se.SortATIAByMean(Self);
    sort_First: se.SortATIAByFirst(Self);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Sort(Index:Integer); overload;
  @desc: 
    Sorts the ATIA by the given index in each group. If the group is not that large it will be set to the last item in that group.
    Negative 'index' will result in counting from right to left: High(Arr[i]) - index
  
}
procedure T2DIntArray.Sort(Index:Integer); overload;
begin
  se.SortATIAByIndex(Self, Index);
end;



{!DOCREF} {
  @method: function T2DIntArray.Reversed(): T2DIntArray;
  @desc: Creates a reversed copy of the array
}
function T2DIntArray.Reversed(): T2DIntArray;
begin
  Self := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure T2DIntArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DIntArray.Reverse();
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
  @method: function T2DIntArray.Sum(): Int64;
  @desc: Returns the sum of the 2d array
}
function T2DIntArray.Sum(): Int64;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;


{!DOCREF} {
  @method: function T2DIntArray.Mean(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DIntArray.Mean(): Extended;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function T2DIntArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function T2DIntArray.Stdev(): Extended;
begin
  Result := Self.Merge().Stdev();
end;


{!DOCREF} {
  @method: function T2DIntArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function T2DIntArray.Variance(): Extended;
var
  Arr:TIntArray;
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
  @method: function T2DIntArray.Mode(): Int32;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function T2DIntArray.Mode(): Int32;
begin
  Result := Self.Merge().Mode();
end;
