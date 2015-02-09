{!DOCTOPIC}{ 
  Type » TExtArray
}


{!DOCREF} {
  @method: function TExtArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TExtArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TExtArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TExtArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TExtArray.Append(const Value:Extended);
  @desc: Add another item to the array
}
procedure TExtArray.Append(const Value:Extended);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
end;


{!DOCREF} {
  @method: procedure TExtArray.Insert(idx:Int32; Value:Extended);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TExtArray.Insert(idx:Int32; Value:Extended);
var l:Int32;
begin
  l := Length(Self);
  if (idx < 0) then
    idx := se.modulo(idx,l);

  if (l <= idx) then begin
    self.append(value);
    Exit();
  end;

  SetLength(Self, l+1);
  MemMove(Self[idx], self[idx+1], (L-Idx)*SizeOf(Extended));
  Self[idx] := value;
end; 


{!DOCREF} {
  @method: procedure TExtArray.Del(idx:Int32);
  @desc: Removes the element at the given index c'idx'
}
procedure TExtArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(Extended));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TExtArray.Remove(Value:Extended);
  @desc: Removes the first element from left which is equal to c'Value'
}
procedure TExtArray.Remove(Value:Extended);
begin
  Self.Del( se.Find(Self,Value) );
end;


{!DOCREF} {
  @method: function TExtArray.Pop(): Extended;
  @desc: Removes and returns the last item in the array
}
function TExtArray.Pop(): Extended;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TExtArray.PopLeft(): Extended;
  @desc: Removes and returns the first item in the array
}
function TExtArray.PopLeft(): Extended;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(Extended)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TExtArray.Slice(Start,Stop:Int64; Step:Int32=1): TExtArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TExtArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): TExtArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
end;


{!DOCREF} {
  @method: procedure TExtArray.Extend(Arr:TExtArray);
  @desc: Extends the array with an array
}
procedure TExtArray.Extend(Arr:TExtArray);
var L:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  MemMove(Arr[0],Self[L],Length(Arr)*SizeOf(Extended));
end; 


{!DOCREF} {
  @method: function TExtArray.Find(Value:Extended): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TExtArray.Find(Value:Extended): Int32;
begin
  Result := se.Find(Self,Value);
end;


{!DOCREF} {
  @method: function TExtArray.Find(Sequence:TExtArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TExtArray.Find(Sequence:TExtArray): Int32; overload;
begin
  Result := se.Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TExtArray.FindAll(Value:Extended): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TExtArray.FindAll(Value:Extended): TIntArray;
begin
  Result := se.FindAll(Self,Value);
end;


{!DOCREF} {
  @method: function TExtArray.FindAll(Sequence:TExtArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TExtArray.FindAll(Sequence:TExtArray): TIntArray; overload;
begin
  Result := se.FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TExtArray.Contains(value:Extended): Boolean;
  @desc: Checks if the arr contains the given value `value`
}
function TExtArray.Contains(value:Extended): Boolean;
begin
  Result := se.Find(Self,value) <> -1;
end;


{!DOCREF} {
  @method: function TExtArray.Count(value:Extended): Int32;
  @desc: Counts all the occurances of the given value `value`
}
function TExtArray.Count(value:Extended): Int32;
begin
  Result := Length(se.FindAll(self, value));
end;


{!DOCREF} {
  @method: procedure TExtArray.Sort(key:ESortKey=sort_Default);
  @desc: 
    Sorts the array
    Supported keys: c'sort_Default'
}
procedure TExtArray.Sort(key:ESortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTEA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TExtArray.Sorted(key:ESortKey=sort_Default): TExtArray;
  @desc: 
    Sorts and returns a copy of the array.
    Supported keys: c'sort_Default'
}
function TExtArray.Sorted(Key:ESortKey=sort_Default): TExtArray;
begin
  Result := Copy(Self);
  case key of
    sort_default: se.SortTEA(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TExtArray.Reversed(): TExtArray;
  @desc:  
    Creates a reversed copy of the array
  
}
function TExtArray.Reversed(): TExtArray;
begin
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure TExtArray.Reverse();
  @desc: Reverses the array
}
procedure TExtArray.Reverse();
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
  @method: function TExtArray.Sum(): Extended;
  @desc: Adds up the TEA and returns the sum
}
function TExtArray.Sum(): Extended;
begin
  Result := se.Sum(self);
end;



{!DOCREF} {
  @method: function TExtArray.Mean(): Extended;
  @desc:Returns the mean value of the array
}
function TExtArray.Mean(): Extended;
begin
  Result := se.Mean(Self);
end;



{!DOCREF} {
  @method: function TExtArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TExtArray.Stdev(): Extended;
begin
  Result := se.Stdev(Self);
end;


{!DOCREF} {
  @method: function TExtArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TExtArray.Variance(): Extended;
begin
  Result := se.Variance(Self);
end;


{!DOCREF} {
  @method: function TExtArray.Mode(Eps:Extended=0.0000001): Extended;
  @desc:
    Returns the sample mode of the array, which is the [u]most frequently occurring value[/u] in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TExtArray.Mode(): Extended;
begin
  Result := se.Mode(Self);
end;



{!DOCREF} {
  @method: function TExtArray.VarMin(): Extended;
  @desc: Returns the minimum value in the array
}
function TExtArray.VarMin(): Extended;
begin
  Result := se.Min(Self);
end;



{!DOCREF} {
  @method: function TExtArray.VarMax(): Extended;
  @desc: Returns the maximum value in the array
}
function TExtArray.VarMax(): Extended;
begin
  Result := se.Max(Self);
end;



{!DOCREF} {
  @method: function TExtArray.ArgMin(): Int32;
  @desc: Returns the index containing the smallest element in the array.
}
function TExtArray.ArgMin(): Int32;
var 
  mat:T2DExtArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := se.ArgMin(mat).x;
end;


{!DOCREF} {
  @method: function TExtArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the smallest element in the array.
}
function TExtArray.ArgMin(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TExtMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMin(n), Result, _);
end;


{!DOCREF} {
  @method: function TExtArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TExtArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DExtArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := se.ArgMin(mat,B).x;
end;



{!DOCREF} {
  @method: function TExtArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TExtArray.ArgMax(): Int32;
var 
  mat:T2DExtArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := se.ArgMax(mat).x;
end;


{!DOCREF} {
  @method: function TExtArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the largest element in the array.
}
function TExtArray.ArgMax(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TExtMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMax(n), Result, _);
end;


{!DOCREF} {
  @method: function TExtArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TExtArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DExtArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := se.ArgMax(mat,B).x;
end;
