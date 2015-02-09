{!DOCTOPIC}{ 
  Type » T2DByteArray
}


{!DOCREF} {
  @method: function T2DByteArray.Len(): Int32;
  @desc: Returns the length of the arr. Same as c'Length(Arr)'
}
function T2DByteArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function T2DByteArray.IsEmpty(): Boolean;
  @desc: Returns True if the ATBA is empty. Same as c'Length(ATBA) = 0'
}
function T2DByteArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Append(const Arr:TByteArray);
  @desc: Add another element to the array
}
procedure T2DByteArray.Append(const Arr:TByteArray);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Arr;
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Insert(idx:Int32; Value:TByteArray);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure T2DByteArray.Insert(idx:Int32; Value:TByteArray);
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
  @method: function T2DByteArray.Pop(): TByteArray;
  @desc: Removes and returns the last item in the array
}
function T2DByteArray.Pop(): TByteArray;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function T2DByteArray.PopLeft(): TByteArray;
  @desc: Removes and returns the first item in the array
}
function T2DByteArray.PopLeft(): TByteArray;
begin
  Result := Self[0];
  Self := Self.Slice(1,-1);
end;


{!DOCREF} {
  @method: function T2DByteArray.Slice(Start,Stop:Int64; Step:Int32=1): T2DByteArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DByteArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): T2DByteArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Extend(Arr:T2DByteArray);
  @desc: Extends the 2d-array with a 2d-array
}
procedure T2DByteArray.Extend(Arr:T2DByteArray);
var L,i:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  for i:=L to High(Self) do
  begin
    SetLength(Self[i],Length(Arr[i-L]));
    MemMove(Arr[i-L][0], Self[i][0], Length(Arr[i-L])*SizeOf(Byte));
  end;
end; 



{!DOCREF} {
  @method: function T2DByteArray.Merge(): TByteArray;
  @desc: Merges all the groups in the array, and return a TIA
}
function T2DByteArray.Merge(): TByteArray;
var i,s,l: Integer;
begin
  s := 0;
  for i:=0 to High(Self) do
  begin
    L := Length(Self[i]);
    SetLength(Result, S+L);
    MemMove(Self[i][0], Result[S], L);
    S := S + L;
  end; 
end;


{!DOCREF} {
  @method: function T2DByteArray.Sorted(Key:ESortKey=sort_Default): T2DByteArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
function T2DByteArray.Sorted(Key:ESortKey=sort_Default): T2DByteArray;
begin
  Result := Self.Slice();
  case Key of
    sort_Default, sort_Length: se.SortATBAByLen(Result);
    sort_Mean: se.SortATBAByMean(Result);
    sort_First: se.SortATBAByFirst(Result);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function T2DByteArray.Sorted(Index:Integer): T2DByteArray; overload;
  @desc: Sorts a copy of the ATBA by the given index in each group
}
function T2DByteArray.Sorted(Index:Integer): T2DByteArray; overload;
begin
  Result := Self.Slice();
  se.SortATBAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Sort(Key:ESortKey=sort_Default);
  @desc: 
    Sorts the ATBA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
procedure T2DByteArray.Sort(Key:ESortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATBAByLen(Self);
    sort_Mean: se.SortATBAByMean(Self);
    sort_First: se.SortATBAByFirst(Self);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Sort(Index:Integer); overload;
  @desc: 
    Sorts the ATBA by the given index in each group. If the group is not that large it will be set to the last item in that group.
    Negative 'index' will result in counting from right to left: c'High(Arr[i]) - index'
  
}
procedure T2DByteArray.Sort(Index:Integer); overload;
begin
  se.SortATBAByIndex(Self, Index);
end;



{!DOCREF} {
  @method: function T2DByteArray.Reversed(): T2DByteArray;
  @desc: Creates a reversed copy of the array
}
function T2DByteArray.Reversed(): T2DByteArray;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DByteArray.Reverse();
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
  @method: function T2DByteArray.Sum(): Int64;
  @desc: Returns the sum of the 2d array
}
function T2DByteArray.Sum(): Int64;
begin
  Result := se.Sum(Self.Merge());   
end;


{!DOCREF} {
  @method: function T2DByteArray.Mean(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DByteArray.Mean(): Extended;
begin
  Result := se.Mean(Self.Merge());   
end;


{!DOCREF} {
  @method: function T2DByteArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function T2DByteArray.Stdev(): Extended;
begin
  Result := se.Stdev(Self.Merge());   
end;


{!DOCREF} {
  @method: function T2DByteArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. 
    A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function T2DByteArray.Variance(): Extended;
begin
  Result := se.Variance(Self.Merge());   
end;

{!DOCREF} {
  @method: function T2DByteArray.Mode(): Byte;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function T2DByteArray.Mode(): Byte;
begin
  Result := se.Mode(Self.Merge());   
end;
