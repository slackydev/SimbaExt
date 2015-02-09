{!DOCTOPIC}{ 
  Type � TByteArray
}


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
  @method: procedure TByteArray.Insert(idx:Int32; Value:Byte);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TByteArray.Insert(idx:Int32; Value:Byte);
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
  MemMove(Self[idx], self[idx+1], (L-Idx)*SizeOf(Byte));
  Self[idx] := value;
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
  Self.Del( se.Find(Self,Value) );
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
  @method: function TByteArray.Slice(Start,Stop:Int64; Step:Int32=1): TByteArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TByteArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): TByteArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
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
  Result := se.Find(Self,Value);
end;


{!DOCREF} {
  @method: function TByteArray.Find(Sequence:TByteArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TByteArray.Find(Sequence:TByteArray): Int32; overload;
begin
  Result := se.Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TByteArray.FindAll(Value:Byte): TIntArray; 
  @desc: Searces for the given value and returns all the position where it was found.
}
function TByteArray.FindAll(Value:Byte): TIntArray; 
begin
  Result := se.FindAll(self,value);
end;


{!DOCREF} {
  @method: function TByteArray.FindAll(Sequence:TByteArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TByteArray.FindAll(Sequence:TByteArray): TIntArray; overload;
begin
  Result := se.FindAll(Self,sequence);
end;


{!DOCREF} {
  @method: function TByteArray.Contains(value:Byte): Boolean;
  @desc: Checks if the arr contains the given value `value`
}
function TByteArray.Contains(value:Byte): Boolean;
begin
  Result := se.Find(Self,value) <> -1;
end;


{!DOCREF} {
  @method: function TByteArray.Count(value:Byte): Int32;
  @desc: Counts all the occurances of given value `value` 
}
function TByteArray.Count(value:Byte): Int32;
begin
  Result := Length(se.FindAll(self, value));
end;


{!DOCREF} {
  @method: procedure TByteArray.Sort(key:ESortKey=sort_Default);
  @desc: 
    Sorts the array
    Supports the keys: c'sort_Default'
}
procedure TByteArray.Sort(key:ESortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTBA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TByteArray.Sorted(key:ESortKey=sort_Default): TByteArray;
  @desc: 
    Sorts and returns a copy of the array.
    Supports the keys: c'sort_Default'
}
function TByteArray.Sorted(Key:ESortKey=sort_Default): TByteArray;
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
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure TByteArray.Reverse();
  @desc: Reverses the array
}
procedure TByteArray.Reverse();
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
  @method: function TByteArray.Sum(): Int64;
  @desc: Adds up the TIA and returns the sum
}
function TByteArray.Sum(): Int64;
begin
  Result := se.Sum(Self);
end;


{!DOCREF} {
  @method: function TByteArray.Mean(): Extended;
  @desc:Returns the mean value of the array. Use round, trunc or floor to get an c'Int' value.
}
function TByteArray.Mean(): Extended;
begin
  Result := se.Mean(Self);
end;



{!DOCREF} {
  @method: function TByteArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TByteArray.Stdev(): Extended;
begin
  Result := se.Stdev(Self);
end;


{!DOCREF} {
  @method: function TByteArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TByteArray.Variance(): Extended;
begin
  Result := se.Variance(Self);
end;



{!DOCREF} {
  @method: function TByteArray.Mode(): Byte;
  @desc:
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
}
function TByteArray.Mode(): Byte;
begin
  Result := se.Mode(Self);
end;



{!DOCREF} {
  @method: function TByteArray.VarMin(): Byte;
  @desc: Returns the minimum value in the array
}
function TByteArray.VarMin(): Byte;
var _:Byte;
begin
  Result := se.Min(Self);
end;



{!DOCREF} {
  @method: function TByteArray.VarMax(): Byte;
  @desc: Returns the maximum value in the array
}
function TByteArray.VarMax(): Byte;
var _:Byte;
begin
  Result := se.Max(Self);
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
  Result := se.ArgMin(mat).x;
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
  Result := se.ArgMin(mat,B).x;
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
  Result := se.ArgMax(mat).x;
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
  Result := se.ArgMax(mat,B).x;
end;




{=============================================================================}
// 
//
{=============================================================================}


{!DOCREF} {
  @method: function TByteArray.AsString(): String;
  @desc: It converts the TBA to a string, by calling `chr()` on each index. Result will be forumlated as a propper string, EG:` Result := 'asd'`
}
function TByteArray.AsString(): String;
var i:Int32;
begin
  SetLength(Result,Length(Self));
  for i:=0 to High(Self) do
    Result[i+1] := chr(Self[i]);
end;


{!DOCREF} {
  @method: procedure TByteArray.FromString(str:String);
  @desc: It converts the string to a TBA, by calling `ord()` on each index.
}
procedure TByteArray.FromString(str:String);
var i:Int32;
begin
  SetLength(Self,Length(Str));
  for i:=1 to Length(Str) do
    Self[i-1] := ord(Str[i]);
end;










