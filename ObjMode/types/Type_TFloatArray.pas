{!DOCTOPIC}{ 
  Type » TFloatArray
}

{!DOCREF} {
  @method: function TFloatArray.Clone(): TFloatArray;
  @desc: Returns a copy of the array
}
function TFloatArray.Clone(): TFloatArray;
begin
  Result := Copy(Self);
end;


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
    idx := math.modulo(idx,l);

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
  Self.Del( Self.Find(Value) );
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
  @method: function TFloatArray.Slice(Start,Stop: Int32; Step:Int32=1): TFloatArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)[br]
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.[br]
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TFloatArray.Slice(Start,Stop: Int32; Step:Int32=1): TFloatArray;
begin
  if Step = 0 then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except end;
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
  Result := exp_Find(Self,[Value]);
end;


{!DOCREF} {
  @method: function TFloatArray.Find(Sequence:TFloatArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TFloatArray.Find(Sequence:TFloatArray): Int32; overload;
begin
  Result := exp_Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TFloatArray.FindAll(Value:Single): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TFloatArray.FindAll(Value:Single): TIntArray;
begin
  Result := exp_FindAll(Self,[value]);
end;


{!DOCREF} {
  @method: function TFloatArray.FindAll(Sequence:TFloatArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TFloatArray.FindAll(Sequence:TFloatArray): TIntArray; overload;
begin
  Result := exp_FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TFloatArray.Contains(val:Single): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function TFloatArray.Contains(val:Single): Boolean;
begin
  Result := Self.Find(val) <> -1;
end;


{!DOCREF} {
  @method: function TFloatArray.Count(val:Single): Int32;
  @desc: Counts all the occurances of the given value c'val'
}
function TFloatArray.Count(val:Single): Int32;
begin
  Result := Length(Self.FindAll(val));
end;


{!DOCREF} {
  @method: procedure TFloatArray.Sort(key:TSortKey=sort_Default);
  @desc: 
    Sorts the array
    Supported keys: c'sort_Default'
}
procedure TFloatArray.Sort(key:TSortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTFA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TFloatArray.Sorted(key:TSortKey=sort_Default): TFloatArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default'
}
function TFloatArray.Sorted(Key:TSortKey=sort_Default): TFloatArray;
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
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure TFloatArray.Reverse();
  @desc: Reverses the array
}
procedure TFloatArray.Reverse();
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
  @method: function TFloatArray.Sum(): Double;
  @desc: Adds up the array and returns the sum
}
function TFloatArray.Sum(): Double;
begin
  Result := exp_SumFPtr(PChar(Self),SizeOf(Single),Length(Self));
end;


{!DOCREF} {
  @method: function TFloatArray.Mean(): Single;
  @desc:Returns the mean value of the array
}
function TFloatArray.Mean(): Single;
begin
  Result := Self.Sum() / Length(Self);
end;


{!DOCREF} {
  @method: function TFloatArray.Stdev(): Single;
  @desc: Returns the standard deviation of the array
}
function TFloatArray.Stdev(): Single;
var
  i:Int32;
  avg:Single;
  square:TFloatArray;
begin
  avg := Self.Mean();
  SetLength(square,Length(Self));
  for i:=0 to High(self) do Square[i] := Sqr(Self[i] - avg);
  Result := sqrt(square.Mean());
end;

{!DOCREF} {
  @method: function TFloatArray.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TFloatArray.Variance(): Double;
var
  avg:Single;
  i:Int32;
begin
  avg := Self.Mean();
  for i:=0 to High(Self) do
    Result := Result + Sqr(Self[i] - avg);
  Result := Result / length(self);
end; 


{!DOCREF} {
  @method: function TFloatArray.Mode(Eps:Single=0.000001): Single;
  @desc:
    Returns the sample mode of the array, which is the [u]most frequently occurring value[/u] in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
    Takes an extra parameter c'Eps', can be used to allow some tolerance in the floating point comparison.
}
function TFloatArray.Mode(Eps:Single=0.0000001): Single;
var
  arr:TFloatArray;
  i,hits,best: Int32;
  cur:Single;
begin
  arr := self.sorted();
  cur := arr[0];
  hits := 1;
  best := 0;
  for i:=1 to High(Arr) do
  begin
    if (arr[i]-cur > eps) then //arr[i] <> cur
    begin
      if (hits > best) then
      begin
        best := hits;
        Result := (Cur+Arr[i-1]) / 2; //Eps fix
      end;
      hits := 0;
      cur := Arr[I];
    end;
    Inc(hits);
  end;
  if (hits > best) then Result := cur;
end;


{!DOCREF} {
  @method: function TFloatArray.VarMin(): Single;
  @desc: Returns the minimum value in the array
}
function TFloatArray.VarMin(): Single;
var lo,hi:Extended;
begin
  exp_MinMaxFPtr(Pointer(self), 4, length(self), lo,hi);
  Result := Lo;
end;



{!DOCREF} {
  @method: function TFloatArray.VarMax(): Single;
  @desc: Returns the maximum value in the array
}
function TFloatArray.VarMax(): Single;
var lo,hi:Extended;
begin
  exp_MinMaxFPtr(Pointer(self), 4, length(self), lo,hi);
  Result := Hi;
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
  Result := exp_ArgMin(mat).x;
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
  Result := exp_ArgMin(mat,B).x;
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
  Result := exp_ArgMax(mat).x;
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
  Result := exp_ArgMax(mat,B).x;
end;
