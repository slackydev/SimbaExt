{!DOCTOPIC}{ 
  Type » TByteArray
}

{!DOCREF} {
  @method: function TByteArray.Clone(): TByteArray;
  @desc: Returns a copy of the array
}
function TByteArray.Clone(): TByteArray;
begin
  Result := Copy(Self);
end;


{!DOCREF} {
  @method: function TByteArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TByteArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TByteArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TByteArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TByteArray.Append(const Value:Byte);
  @desc: Add another item to the array
}
procedure TByteArray.Append(const Value:Byte);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
end;


{!DOCREF} {
  @method: procedure TByteArray.Del(idx:Int32);
  @desc: Removes the element at the given index c'idx'
}
procedure TByteArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(Byte));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TByteArray.Remove(Value:Byte);
  @desc: Removes the first element from left which is equal to c'Value'
}
procedure TByteArray.Remove(Value:Byte);
begin
  Self.Del( Self.Find(Value) );
end;



{!DOCREF} {
  @method: function TByteArray.Pop(): Byte;
  @desc: Removes and returns the last item in the array
}
function TByteArray.Pop(): Byte;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TByteArray.PopLeft(): Byte;
  @desc: Removes and returns the first item in the array
}
function TByteArray.PopLeft(): Byte;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(Byte)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TByteArray.Slice(Start,Stop: Int32; Step:Int32=1): TByteArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TByteArray.Slice(Start,Stop: Int32; Step:Int32=1): TByteArray;
begin
  if Step = 0 then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except end;
end;


{!DOCREF} {
  @method: procedure TByteArray.Extend(Arr:TByteArray);
  @desc: Extends the array with an array
}
procedure TByteArray.Extend(Arr:TByteArray);
var L:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  MemMove(Arr[0],Self[L],Length(Arr)*SizeOf(Byte));
end; 


{!DOCREF} {
  @method: function TByteArray.Find(Value:Byte): Int32; 
  @desc: Searces for the given value and returns the first position from the left.
}
function TByteArray.Find(Value:Byte): Int32; 
begin
  Result := exp_Find(Self,TByteArray([Value]));
end;


{!DOCREF} {
  @method: function TByteArray.Find(Sequence:TByteArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TByteArray.Find(Sequence:TByteArray): Int32; overload;
begin
  Result := exp_Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TByteArray.FindAll(Value:Byte): TIntArray; 
  @desc: Searces for the given value and returns all the position where it was found.
}
function TByteArray.FindAll(Value:Byte): TIntArray; 
begin
  Result := exp_FindAll(Self,TByteArray([value]));
end;


{!DOCREF} {
  @method: function TByteArray.FindAll(Sequence:TByteArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TByteArray.FindAll(Sequence:TByteArray): TIntArray; overload;
begin
  Result := exp_FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TByteArray.Contains(val:Byte): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function TByteArray.Contains(val:Byte): Boolean;
begin
  Result := Self.Find(val) <> -1;
end;


{!DOCREF} {
  @method: function TByteArray.Count(val:Byte): Int32;
  @desc: Counts all the occurances of val
}
function TByteArray.Count(val:Byte): Int32;
begin
  Result := Length(Self.FindAll(val));
end;


{!DOCREF} {
  @method: procedure TByteArray.Sort(key:TSortKey=sort_Default);
  @desc: 
    Sorts the array
    Supports the keys: c'sort_Default'
}
procedure TByteArray.Sort(key:TSortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTBA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TByteArray.Sorted(key:TSortKey=sort_Default): TByteArray;
  @desc: 
    Sorts and returns a copy of the array.
    Supports the keys: c'sort_Default'
}
function TByteArray.Sorted(Key:TSortKey=sort_Default): TByteArray;
begin
  Result := Copy(Self);
  case key of
    sort_default: se.SortTBA(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;



{!DOCREF} {
  @method: function TByteArray.Reversed(): TByteArray;
  @desc:  
    Creates a reversed copy of the array
}
function TByteArray.Reversed(): TByteArray;
begin
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure TByteArray.Reverse();
  @desc: Reverses the array
}
procedure TByteArray.Reverse();
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
  @method: function TByteArray.Sum(): Int32;
  @desc: Adds up the TIA and returns the sum
}
function TByteArray.Sum(): Int32;
begin
  Result := exp_SumPtr(PChar(Self),SizeOf(Byte),Length(Self),True);
end;


{!DOCREF} {
  @method: function TByteArray.Sum64(): Int64;
  @desc: Adds up the TBA and returns the sum
}
function TByteArray.Sum64(): Int64;
begin
  Result := exp_SumPtr(PChar(Self),SizeOf(Byte),Length(Self),True);
end;


{!DOCREF} {
  @method: function TByteArray.Mean(): Extended;
  @desc:Returns the mean value of the array. Use round, trunc or floor to get an c'Int' value.
}
function TByteArray.Mean(): Extended;
begin
  Result := Self.Sum64() / Length(Self);
end;



{!DOCREF} {
  @method: function TByteArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TByteArray.Stdev(): Extended;
var
  i:Int32;
  avg:Extended;
  square:TExtArray;
begin
  avg := Self.Mean();
  SetLength(square,Length(Self));
  for i:=0 to High(self) do Square[i] := Sqr(Self[i] - avg);
  Result := sqrt(square.Mean());
end;


{!DOCREF} {
  @method: function TByteArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TByteArray.Variance(): Extended;
var
  avg:Extended;
  i:Int32;
begin
  avg := Self.Mean();
  for i:=0 to High(Self) do
    Result := Result + Sqr(Self[i] - avg);
  Result := Result / length(self);
end; 



{!DOCREF} {
  @method: function TByteArray.Mode(): Byte;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TByteArray.Mode(): Byte;
var
  arr:TByteArray;
  cur:Byte;
  i,hits,best: Int32;
begin
  arr := self.sorted();
  cur := arr[0];
  hits := 1;
  best := 0;
  for i:=1 to High(Arr) do
  begin
    if (cur <> arr[i]) then
    begin
      if (hits > best) then
      begin
        best := hits;
        Result := cur;
      end;
      hits := 0;
      cur := Arr[I];
    end;
    Inc(hits);
  end;
  if (hits > best) then Result := cur;
end;



{!DOCREF} {
  @method: function TByteArray.VarMin(): Byte;
  @desc: Returns the minimum value in the array
}
function TByteArray.VarMin(): Byte;
var _:Byte;
begin
  se.MinMaxTBA(Self,Result,_);
end;



{!DOCREF} {
  @method: function TByteArray.VarMax(): Byte;
  @desc: Returns the maximum value in the array
}
function TByteArray.VarMax(): Byte;
var _:Byte;
begin
  se.MinMaxTBA(Self,_,Result);
end;



{!DOCREF} {
  @method: function TByteArray.ArgMin(): Int32;
  @desc: Returns the index containing the smallest element in the array.
}
function TByteArray.ArgMin(): Int32;
var 
  mat:T2DByteArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMin(mat).x;
end;


{!DOCREF} {
  @method: function TByteArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the smallest element in the array.
}
function TByteArray.ArgMin(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TByteMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMin(n), Result, _);
end;


{!DOCREF} {
  @method: function TByteArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TByteArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DByteArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMin(mat,B).x;
end;



{!DOCREF} {
  @method: function TByteArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TByteArray.ArgMax(): Int32;
var 
  mat:T2DByteArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMax(mat).x;
end;


{!DOCREF} {
  @method: function TByteArray.ArgMax(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the largest element in the array.
}
function TByteArray.ArgMax(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TByteMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMax(n), Result, _);
end;


{!DOCREF} {
  @method: function TByteArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TByteArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DByteArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMax(mat,B).x;
end;

















