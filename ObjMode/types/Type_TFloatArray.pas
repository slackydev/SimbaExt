{!DOCTOPIC}{ 
  Type » TFloatArray
}


{!DOCREF} {
  @method: function TFloatArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TFloatArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TFloatArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TFloatArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TFloatArray.Append(const Value:Single);
  @desc: Add another item to the array
}
procedure TFloatArray.Append(const Value:Single);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
end;


{!DOCREF} {
  @method: procedure TFloatArray.Insert(idx:Int32; Value:Single);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TFloatArray.Insert(idx:Int32; Value:Single);
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
  MemMove(Self[idx], self[idx+1], (L-Idx)*SizeOf(Single));
  Self[idx] := value;
end; 


{!DOCREF} {
  @method: procedure TFloatArray.Del(idx:Int32);
  @desc: Removes the element at the given index c'idx'
}
procedure TFloatArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(Single));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TFloatArray.Remove(Value:Single);
  @desc: Removes the first element from left which is equal to c'Value'
}
procedure TFloatArray.Remove(Value:Single);
begin
  Self.Del( se.Find(Self,Value) );
end;


{!DOCREF} {
  @method: function TFloatArray.Pop(): Single;
  @desc: Removes and returns the last item in the array
}
function TFloatArray.Pop(): Single;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TFloatArray.PopLeft(): Single;
  @desc: Removes and returns the first item in the array
}
function TFloatArray.PopLeft(): Single;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(Single)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TFloatArray.Slice(Start,Stop:Int64; Step:Int32=1): TFloatArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)[br]
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.[br]
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TFloatArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): TFloatArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
end;


{!DOCREF} {
  @method: procedure TFloatArray.Extend(Arr:TFloatArray);
  @desc: Extends the array with an array
}
procedure TFloatArray.Extend(Arr:TFloatArray);
var L:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  MemMove(Arr[0],Self[L],Length(Arr)*SizeOf(Single));
end; 


{!DOCREF} {
  @method: function TFloatArray.Find(Value:Single): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TFloatArray.Find(Value:Single): Int32;
begin
  Result := se.Find(Self,Value);
end;


{!DOCREF} {
  @method: function TFloatArray.Find(Sequence:TFloatArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TFloatArray.Find(Sequence:TFloatArray): Int32; overload;
begin
  Result := se.Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TFloatArray.FindAll(Value:Single): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TFloatArray.FindAll(Value:Single): TIntArray;
begin
  Result := se.FindAll(Self,Value);
end;


{!DOCREF} {
  @method: function TFloatArray.FindAll(Sequence:TFloatArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TFloatArray.FindAll(Sequence:TFloatArray): TIntArray; overload;
begin
  Result := se.FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TFloatArray.Contains(value:Single): Boolean;
  @desc: Checks if the arr contains the given value `value`
}
function TFloatArray.Contains(value:Single): Boolean;
begin
  Result := se.Find(Self,value) <> -1;
end;


{!DOCREF} {
  @method: function TFloatArray.Count(value:Single): Int32;
  @desc: Counts all the occurances of the given value `value`
}
function TFloatArray.Count(value:Single): Int32;
begin
  Result := Length(se.FindAll(self, value));
end;


{!DOCREF} {
  @method: procedure TFloatArray.Sort(key:ESortKey=sort_Default);
  @desc: 
    Sorts the array
    Supported keys: c'sort_Default'
}
procedure TFloatArray.Sort(key:ESortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTFA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TFloatArray.Sorted(key:ESortKey=sort_Default): TFloatArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default'
}
function TFloatArray.Sorted(Key:ESortKey=sort_Default): TFloatArray;
begin
  Result := Copy(Self);
  case key of
    sort_default: se.SortTFA(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TFloatArray.Reversed(): TFloatArray;
  @desc:  
    Creates a reversed copy of the array
  
}
function TFloatArray.Reversed(): TFloatArray;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure TFloatArray.Reverse();
  @desc: Reverses the array
}
procedure TFloatArray.Reverse();
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
  @method: function TFloatArray.Sum(): Extended;
  @desc: Adds up the array and returns the sum
}
function TFloatArray.Sum(): Extended;
begin
  Result := se.Sum(Self);
end;


{!DOCREF} {
  @method: function TFloatArray.Mean(): Single;
  @desc:Returns the mean value of the array
}
function TFloatArray.Mean(): Single;
begin
  Result := se.Mean(Self);
end;


{!DOCREF} {
  @method: function TFloatArray.Stdev(): Single;
  @desc: Returns the standard deviation of the array
}
function TFloatArray.Stdev(): Single;
begin
  Result := se.Stdev(Self);
end;

{!DOCREF} {
  @method: function TFloatArray.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TFloatArray.Variance(): Double;
begin
  Result := se.Variance(Self);
end; 


{!DOCREF} {
  @method: function TFloatArray.Mode(): Single;
  @desc:
    Returns the sample mode of the array, which is the [u]most frequently occurring value[/u] in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TFloatArray.Mode(): Single;
begin
  Result := se.Mode(Self);
end;


{!DOCREF} {
  @method: function TFloatArray.VarMin(): Single;
  @desc: Returns the minimum value in the array
}
function TFloatArray.VarMin(): Single;
begin
  Result := se.Min(Self);
end;



{!DOCREF} {
  @method: function TFloatArray.VarMax(): Single;
  @desc: Returns the maximum value in the array
}
function TFloatArray.VarMax(): Single;
begin
  Result := se.Max(Self);
end;



{!DOCREF} {
  @method: function TFloatArray.ArgMin(): Int32;
  @desc: Returns the index containing the smallest element in the array.
}
function TFloatArray.ArgMin(): Int32;
var 
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := se.ArgMin(mat).x;
end;


{!DOCREF} {
  @method: function TFloatArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the smallest element in the array.
}
function TFloatArray.ArgMin(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMin(n), Result, _);
end;


{!DOCREF} {
  @method: function TFloatArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TFloatArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := se.ArgMin(mat,B).x;
end;



{!DOCREF} {
  @method: function TFloatArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TFloatArray.ArgMax(): Int32;
var 
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := se.ArgMax(mat).x;
end;


{!DOCREF} {
  @method: function TFloatArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the largest element in the array.
}
function TFloatArray.ArgMax(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMax(n), Result, _);
end;


{!DOCREF} {
  @method: function TFloatArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TFloatArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TFloatMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := se.ArgMax(mat,B).x;
end;




