{!DOCTOPIC}{ 
  Type � TIntArray
}


{!DOCREF} {
  @method: function TIntArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TIntArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TIntArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TIntArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TIntArray.Append(const Value:Int32);
  @desc: Add another item to the array
}
{$IFNDEF SRL6}
procedure TIntArray.Append(const Value:Int32);
{$ELSE}
procedure TIntArray.Append(const Value:Int32); override;
{$ENDIF}
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
end;


{!DOCREF} {
  @method: procedure TIntArray.Insert(idx:Int32; Value:Int32);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TIntArray.Insert(idx:Int32; Value:Int32);
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
  MemMove(Self[idx], self[idx+1], (L-Idx)*SizeOf(Int32));
  Self[idx] := value;
end; 


{!DOCREF} {
  @method: procedure TIntArray.Del(idx:Int32);
  @desc: Removes the element at the given index c'idx'
}
procedure TIntArray.Del(idx:Int32);
var i,l:Int32;
begin
  l := Length(Self);
  if (l <= idx) or (idx < 0) then 
    Exit();
  if (L-1 <> idx) then
    MemMove(Self[idx+1], self[idx], (L-Idx)*SizeOf(Int32));
  SetLength(Self, l-1);
end;


{!DOCREF} {
  @method: procedure TIntArray.Remove(Value:Int32);
  @desc: Removes the first element from left which is equal to c'Value'
}
procedure TIntArray.Remove(Value:Int32);
begin
  Self.Del( Self.Find(Value) );
end;


{!DOCREF} {
  @method: function TIntArray.Pop(): Int32;
  @desc: Removes and returns the last item in the array
}
function TIntArray.Pop(): Int32;
var H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TIntArray.PopLeft(): Int32;
  @desc: Removes and returns the first item in the array
}
function TIntArray.PopLeft(): Int32;
begin
  Result := Self[0];
  MemMove(Self[1], Self[0], SizeOf(Int32)*Length(Self));
  SetLength(Self, High(self));
end;


{!DOCREF} {
  @method: function TIntArray.Slice(Start,Stop: Int32; Step:Int32=1): TIntArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to `step` past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [b]Examples:[/b]
    [code=pascal]
    TIA := [0,1,2,3,4,5,6,7,8,9];
    TIA.Slice(,,-1)    = [9,8,7,6,5,4,3,2,1,0]  //Copies from 9 downto 0, with a step-size of 1.
    TIA.Slice(,,-2)    = [9,7,5,3,1]            //Copies from 9 downto 0, with a step-size of 2.
    TIA.Slice(3,7)     = [3,4,5,6,7]            //Copies from 2 to 7
    TIA.Slice(,-2)     = [0,1,2,3,4,5,6,7,8]    //Copies from 1 to Len-2
    [/code]

    [note]Don't pass positive `Step`, combined with `Start > Stop`, that is undefined[/note]
}
function TIntArray.Slice(Start:Int64=DefVar64; Stop: Int64=DefVar64; Step:Int64=1): TIntArray;
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
  @method: procedure TIntArray.Extend(Arr:TIntArray);
  @desc: Extends the array with an array
}
procedure TIntArray.Extend(Arr:TIntArray);
var L:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  MemMove(Arr[0],Self[L],Length(Arr)*SizeOf(Int32));
end;  


{!DOCREF} {
  @method: function TIntArray.Find(Value:Int32): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TIntArray.Find(Value:Int32): Int32;
begin
  Result := exp_Find(Self,[Value]);
end;


{!DOCREF} {
  @method: function TIntArray.Find(Sequence:TIntArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TIntArray.Find(Sequence:TIntArray): Int32; overload;
begin
  Result := exp_Find(Self,Sequence);
end;



{!DOCREF} {
  @method: function TIntArray.FindAll(Value:Int32): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TIntArray.FindAll(Value:Int32): TIntArray; overload;
begin
  Result := exp_FindAll(Self,[value]);
end;


{!DOCREF} {
  @method: function TIntArray.FindAll(Sequence:TIntArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TIntArray.FindAll(Sequence:TIntArray): TIntArray; overload;
begin
  Result := exp_FindAll(Self,sequence);
end;



{!DOCREF} {
  @method: function TIntArray.Contains(val:Int32): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function TIntArray.Contains(val:Int32): Boolean;
begin
  Result := Self.Find(val) <> -1;
end;


{!DOCREF} {
  @method: function TIntArray.Count(val:Int32): Int32;
  @desc: Counts all the occurances of the given value c'val'
}
function TIntArray.Count(val:Int32): Int32;
begin
  Result := Length(Self.FindAll(val));
end;



{!DOCREF} {
  @method: procedure TIntArray.Sort(key:TSortKey=sort_Default);
  @desc: 
    Sorts the array
    Supports the keys: c'sort_Default'
}
procedure TIntArray.Sort(key:TSortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTIA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TIntArray.Sorted(key:TSortKey=sort_Default): TIntArray;
  @desc: 
    Returns a new sorted array from the input array.
    Supports the keys: c'sort_Default'
}
function TIntArray.Sorted(Key:TSortKey=sort_Default): TIntArray;
begin
  Result := Copy(Self);
  case key of
    sort_default: se.SortTIA(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TIntArray.Reversed(): TIntArray;
  @desc:  
    Creates a reversed copy of the array
}
function TIntArray.Reversed(): TIntArray;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure TIntArray.Reverse();
  @desc: Reverses the array
}
procedure TIntArray.Reverse();
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
  @method: function TIntArray.Sum(): Int64;
  @desc: Adds up the TIA and returns the sum
}
{$IFNDEF SRL6}
function TIntArray.Sum(): Int64;
{$ELSE}
function TIntArray.Sum(): Int32; override;
{$ENDIF}
begin
  Result := exp_SumPtr(PChar(Self),SizeOf(Int32),Length(Self),False);
end;


{!DOCREF} {
  @method: function TIntArray.Mean(): Extended;
  @desc:Returns the mean value of the array. Use round, trunc or floor to get an c'Int' value.
}
function TIntArray.Mean(): Extended;
begin
  Result := Self.Sum() / Length(Self);
end;



{!DOCREF} {
  @method: function TIntArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TIntArray.Stdev(): Extended;
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
  @method: function TIntArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TIntArray.Variance(): Extended;
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
  @method: function TIntArray.Mode(): Int32;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TIntArray.Mode(): Int32;
var
  arr:TIntArray;
  i,cur,hits,best: Int32;
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
  @method: function TIntArray.VarMin(): Int32;
  @desc: Returns the minimum value in the array
}
function TIntArray.VarMin(): Int32;
var _:Int32;
begin
  se.MinMaxTIA(Self,Result,_);
end;



{!DOCREF} {
  @method: function TIntArray.VarMax(): Int32;
  @desc: Returns the maximum value in the array
}
function TIntArray.VarMax(): Int32;
var _:Int32;
begin
  se.MinMaxTIA(Self,_,Result);
end;



{!DOCREF} {
  @method: function TIntArray.ArgMin(): Int32;
  @desc: Returns the index containing the smallest element in the array.
}
function TIntArray.ArgMin(): Int32;
var 
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMin(mat).x;
end;


{!DOCREF} {
  @method: function TIntArray.ArgMin(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the smallest element in the array.
}
function TIntArray.ArgMin(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMin(n), Result, _);
end;


{!DOCREF} {
  @method: function TIntArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TIntArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMin(mat,B).x;
end;



{!DOCREF} {
  @method: function TIntArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TIntArray.ArgMax(): Int32;
var 
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMax(mat).x;
end;


{!DOCREF} {
  @method: function TIntArray.ArgMax(n:int32): TIntArray; overload;
  @desc: Returns the n-indices containing the largest element in the array.
}
function TIntArray.ArgMax(n:Int32): TIntArray; overload;
var 
  i: Int32;
  _:TIntArray;
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  se.TPASplitAxis(mat.ArgMax(n), Result, _);
end;


{!DOCREF} {
  @method: function TIntArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TIntArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:TIntMatrix;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMax(mat,B).x;
end;

















