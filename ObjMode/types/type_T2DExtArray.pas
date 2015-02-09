{!DOCTOPIC}{ 
  Type » T2DExtArray
}


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
  @method: procedure T2DExtArray.Insert(idx:Int32; Value:TExtArray);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure T2DExtArray.Insert(idx:Int32; Value:TExtArray);
var i,l:Int32;
begin
  l := Length(Self);
  if (idx < 0) then
    idx := se.modulo(idx,l);

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
  @method: function T2DExtArray.Slice(Start,Stop:Int64; Step:Int32=1): T2DExtArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DExtArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): T2DExtArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
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
  @method: function T2DExtArray.Sorted(Key:ESortKey=sort_Default): T2DExtArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
function T2DExtArray.Sorted(Key:ESortKey=sort_Default): T2DExtArray;
begin
  Result := Self.Slice();
  case Key of
    sort_Default, sort_Length: se.SortATEAByLen(Result);
    sort_Mean: se.SortATEAByMean(Result);
    sort_First: se.SortATEAByFirst(Result);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function T2DExtArray.Sorted(Index:Integer): T2DPointArray; overload;
  @desc: Returns a new sorted array from the input array. Sorts by the given index in each group.
}
function T2DExtArray.Sorted(Index:Integer): T2DExtArray; overload;
begin
  Result := Self.Slice();
  se.SortATEAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Sort(Key:ESortKey=sort_Default);
  @desc: 
    Sorts the array with the given key
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
procedure T2DExtArray.Sort(Key:ESortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATEAByLen(Self);
    sort_Mean: se.SortATEAByMean(Self);
    sort_First: se.SortATEAByFirst(Self);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Sort(Index:Integer); overload;
  @desc: 
    Sorts the array by the given index in each group. If the group is not that the last index in the group is used.
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
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure T2DExtArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DExtArray.Reverse();
begin
  Self := Self.Slice(,,-1);
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
begin
  Result := se.Sum(Self.Merge());
end;


{!DOCREF} {
  @method: function T2DExtArray.Mean(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DExtArray.Mean(): Extended;
begin
  Result := se.Mean(Self.Merge());
end;


{!DOCREF} {
  @method: function T2DExtArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function T2DExtArray.Stdev(): Extended;
begin
  Result := se.Stdev(Self.Merge());
end;


{!DOCREF} {
  @method: function T2DExtArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function T2DExtArray.Variance(): Extended;
begin
  Result := se.Variance(Self.Merge());
end; 


{!DOCREF} {
  @method: function T2DExtArray.Mode(): Extended;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function T2DExtArray.Mode(): Extended;
begin
  Result := se.Mode(Self.Merge());
end;

