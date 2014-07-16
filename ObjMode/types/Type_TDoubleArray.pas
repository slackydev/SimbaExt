{!DOCTOPIC}{ 
  Type » TDoubleArray
}


{!DOCREF} {
  @method: function TDoubleArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TDoubleArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TDoubleArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TDoubleArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Append(const Value:Double);
  @desc: Add another item to the array
}
procedure TDoubleArray.Append(const Value:Double);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Insert(idx:Int32; Value:Double);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TDoubleArray.Insert(idx:Int32; Value:Double);
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
  MemMove(Self[idx], self[idx+1], (L-Idx)*SizeOf(Double));
  Self[idx] := value;
end; 


{!DOCREF} {
  @method: procedure TDoubleArray.Del(idx:Int32);
  @desc: Removes the element at the given index c'idx'
}
procedure TDoubleArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(Double));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Remove(Value:Double);
  @desc: Removes the first element from left which is equal to c'Value'
}
procedure TDoubleArray.Remove(Value:Double);
begin
  Self.Del( Self.Find(Value) );
end;


{!DOCREF} {
  @method: function TDoubleArray.Pop(): Double;
  @desc: Removes and returns the last item in the array
}
function TDoubleArray.Pop(): Double;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TDoubleArray.PopLeft(): Double;
  @desc: Removes and returns the first item in the array
}
function TDoubleArray.PopLeft(): Double;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(Double)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TDoubleArray.Slice(Start,Stop: Int32; Step:Int32=1): TDoubleArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TDoubleArray.Slice(Start:Int64=DefVar64; Stop: Int64=DefVar64; Step:Int64=1): TDoubleArray;
begin
  if (Start = DefVar64) then
    if Step < 0 then Start := -1
    else Start := 0;       
  if (Stop = DefVar64) then 
    if Step > 0 then Stop := -1
    else Stop := 0;
  
  if Step = 0 then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except SetLength(Result,0) end;
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Extend(Arr:TDoubleArray);
  @desc: Extends the array with an array
}
procedure TDoubleArray.Extend(Arr:TDoubleArray);
var L:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  MemMove(Arr[0],Self[L],Length(Arr)*SizeOf(Double));
end; 


{!DOCREF} {
  @method: function TDoubleArray.Find(Value:Double): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TDoubleArray.Find(Value:Double): Int32;
begin
  Result := exp_Find(Self,[Value]);
end;


{!DOCREF} {
  @method: function TDoubleArray.Find(Sequence:TDoubleArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TDoubleArray.Find(Sequence:TDoubleArray): Int32; overload;
begin
  Result := exp_Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TDoubleArray.FindAll(Value:Double): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TDoubleArray.FindAll(Value:Double): TIntArray;
begin
  Result := exp_FindAll(Self,[value]);
end;


{!DOCREF} {
  @method: function TDoubleArray.FindAll(Sequence:TDoubleArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TDoubleArray.FindAll(Sequence:TDoubleArray): TIntArray; overload;
begin
  Result := exp_FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TDoubleArray.Contains(val:Double): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function TDoubleArray.Contains(val:Double): Boolean;
begin
  Result := Self.Find(val) <> -1;
end;


{!DOCREF} {
  @method: function TDoubleArray.Count(val:Double): Int32;
  @desc: Counts all the occurances of the given value c'val'
}
function TDoubleArray.Count(val:Double): Int32;
begin
  Result := Length(Self.FindAll(val));
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Sort(key:TSortKey=sort_Default);
  @desc: 
    Sorts the array
    Supported keys: c'sort_Default'
}
procedure TDoubleArray.Sort(key:TSortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTDA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TDoubleArray.Sorted(key:TSortKey=sort_Default): TDoubleArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supported keys: c'sort_Default'
}
function TDoubleArray.Sorted(Key:TSortKey=sort_Default): TDoubleArray;
begin
  Result := Copy(Self);
  case key of
    sort_default: se.SortTDA(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TDoubleArray.Reversed(): TDoubleArray;
  @desc:  
    Creates a reversed copy of the array
  
}
function TDoubleArray.Reversed(): TDoubleArray;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure TDoubleArray.Reverse();
  @desc: Reverses the array
}
procedure TDoubleArray.Reverse();
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
  @method: function TDoubleArray.Sum(): Double;
  @desc: Adds up the array and returns the sum
}
function TDoubleArray.Sum(): Double;
begin
  Result := exp_SumFPtr(PChar(Self),SizeOf(Double),Length(Self));
end;


{!DOCREF} {
  @method: function TDoubleArray.Sum64(): Double;
  @desc: Adds up the array and returns the sum
}
function TDoubleArray.Sum64(): Double;
begin
  Result := exp_SumFPtr(PChar(Self),SizeOf(Double),Length(Self));
end;


{!DOCREF} {
  @method: function TDoubleArray.Mean(): Double;
  @desc:Returns the mean value of the array
}
function TDoubleArray.Mean(): Double;
begin
  Result := Self.Sum() / Length(Self);
end;



{!DOCREF} {
  @method: function TDoubleArray.Stdev(): Double;
  @desc: Returns the standard deviation of the array
}
function TDoubleArray.Stdev(): Double;
var
  i:Int32;
  avg:Double;
  square:TDoubleArray;
begin
  avg := Self.Mean();
  SetLength(square,Length(Self));
  for i:=0 to High(self) do Square[i] := Sqr(Self[i] - avg);
  Result := sqrt(square.Mean());
end;

{!DOCREF} {
  @method: function TDoubleArray.Variance(): Double;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TDoubleArray.Variance(): Double;
var
  avg:Double;
  i:Int32;
begin
  avg := Self.Mean();
  for i:=0 to High(Self) do
    Result := Result + Sqr(Self[i] - avg);
  Result := Result / length(self);
end; 


{!DOCREF} {
  @method: function TDoubleArray.Mode(Eps:Double=0.000001): Double;
  @desc:
    Returns the sample mode of the array, which is the [u]most frequently occurring value[/u] in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
    Takes an extra parameter c'Eps', can be used to allow some tolerance in the floating point comparison.
}
function TDoubleArray.Mode(Eps:Double=0.0000001): Double;
var
  arr:TDoubleArray;
  i,hits,best: Int32;
  cur:Double;
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
  @method: function TDoubleArray.VarMin(): Double;
  @desc: Returns the minimum value in the array
}
function TDoubleArray.VarMin(): Double;
var lo,hi:Extended;
begin
  exp_MinMaxFPtr(Pointer(self), 4, length(self), lo,hi);
  Result := Lo;
end;



{!DOCREF} {
  @method: function TDoubleArray.VarMax(): Double;
  @desc: Returns the maximum value in the array
}
function TDoubleArray.VarMax(): Double;
var lo,hi:Extended;
begin
  exp_MinMaxFPtr(Pointer(self), 4, length(self), lo,hi);
  Result := Hi;
end;



{!DOCREF} {
  @method: function TDoubleArray.ArgMin(): Int32;
  @desc: Returns the index containing the smallest element in the array.
}
function TDoubleArray.ArgMin(): Int32;
var 
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMin(mat).x;
end;


{!DOCREF} {
  @method: function TDoubleArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the smallest element in the array.
}
function TDoubleArray.ArgMin(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMin(n), Result, _);
end;


{!DOCREF} {
  @method: function TDoubleArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TDoubleArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMin(mat,B).x;
end;



{!DOCREF} {
  @method: function TDoubleArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TDoubleArray.ArgMax(): Int32;
var 
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMax(mat).x;
end;


{!DOCREF} {
  @method: function TDoubleArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the largest element in the array.
}
function TDoubleArray.ArgMax(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMax(n), Result, _);
end;


{!DOCREF} {
  @method: function TDoubleArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TDoubleArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TDoubleMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMax(mat,B).x;
end;
