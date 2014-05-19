{!DOCTOPIC}{ 
  Type » T2DByteArray
}

{!DOCREF} {
  @method: function T2DByteArray.Clone(): T2DIntArray;
  @desc: Returns a copy of the array
}
function T2DByteArray.Clone(): T2DByteArray;
var i:Int32;
begin
  SetLength(Result, Length(Self));
  for i:=0 to High(Self) do
    Result[i] := Copy(Self[i]);
end;


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
  @method: function T2DByteArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DByteArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DByteArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DByteArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
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
  @method: function T2DByteArray.Sorted(Key:TSortKey=sort_Default): T2DByteArray;
  @desc: 
    Sorts a copy of the ATBA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
function T2DByteArray.Sorted(Key:TSortKey=sort_Default): T2DByteArray;
begin
  Result := Self.Clone();
  case Key of
    sort_Default, sort_Length: se.SortATBAByLength(Result);
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
  Result := Self.Clone();
  se.SortATBAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Sort(Key:TSortKey=sort_Default);
  @desc: 
    Sorts the ATBA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
procedure T2DByteArray.Sort(Key:TSortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATBAByLength(Self);
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
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure T2DByteArray.Reverse();
  @desc: Reverses the array  
}
procedure T2DByteArray.Reverse();
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
  @method: function T2DByteArray.Sum(): Int32;
  @desc: Returns the sum of the 2d array (32 bit version only goes ~2,147,483,647)
}
function T2DByteArray.Sum(): Int32;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum();
end;


{!DOCREF} {
  @method: function T2DByteArray.Sum64(): Int64;
  @desc: Returns the sum of the 2d array
}
function T2DByteArray.Sum64(): Int64;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Sum64();
end;


{!DOCREF} {
  @method: function T2DByteArray.Mean(): Extended;
  @desc: Returns the mean of the 2d array
}
function T2DByteArray.Mean(): Extended;
var i,L: Integer;
begin
  for i:=0 to High(Self) do
    Result := Result + Self[i].Mean();
  Result := Result / High(Self);
end;


{!DOCREF} {
  @method: function T2DByteArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function T2DByteArray.Stdev(): Extended;
begin
  Result := Self.Merge().Stdev();
end;


{!DOCREF} {
  @method: function T2DByteArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function T2DByteArray.Variance(): Extended;
var
  Arr:TByteArray;
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
  @method: function T2DByteArray.Mode(): Byte;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function T2DByteArray.Mode(): Byte;
begin
  Result := Self.Merge().Mode();
end;


{!DOCREF} {
  @method: function T2DByteArray.VarMin(): Byte;
  @desc: Returns the minimum value in the array
}
function T2DByteArray.VarMin(): Byte;
var _:Byte;
begin
  se.MinMaxTBA(Self.Merge(),Result,_);
end;



{!DOCREF} {
  @method: function T2DByteArray.VarMax(): Byte;
  @desc: Returns the maximum value in the array
}
function T2DByteArray.VarMax(): Byte;
var _:Byte;
begin
  se.MinMaxTBA(Self.Merge(),_,Result);
end;


{!DOCREF} {
  @method: function T2DByteArray.ArgMin(): TPoint;
  @desc: Returns the index containing the smallest element in the array
}
function T2DByteArray.ArgMin(): TPoint;
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
  @method: function T2DByteArray.ArgMax(): TPoint;
  @desc: Returns the index containing the largest element in the array
}
function T2DByteArray.ArgMax(): TPoint;
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
