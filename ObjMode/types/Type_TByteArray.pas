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
  @method: function TByteArray.Slice(Start,Stop: Int32): TByteArray;
  @desc: Returns a slice of the array
}
function TByteArray.Slice(Start,Stop: Int32): TByteArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
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
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure TByteArray.Reverse();
  @desc: Reverses the array
}
procedure TByteArray.Reverse();
var
  i,Hi,Mid:Int32;
  tmp:Byte;
begin
  Hi := High(Self);
  if (Hi < 0) then Exit;
  Mid := Hi div 2;
  for i := 0 to Mid do begin
    tmp := Self[Hi-i];
    Self[Hi-i] := Self[i];
    Self[i] := tmp;
  end;
end;


{!DOCREF} {
  @method: function TByteArray.Sum(): Int32;
  @desc: Adds up the TIA and returns the sum
}
function TByteArray.Sum(): Int32;
begin
  Result := se.SumTBA(Self);
end;


{!DOCREF} {
  @method: function TByteArray.Sum64(): Int64;
  @desc: Adds up the TIA and returns the sum
}
function TByteArray.Sum64(): Int64;
begin
  Result := se.SumTBA(Self);
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
  @method: function TByteArray.Min(): Byte;
  @desc: Returns the minimum value in the array
}
function TByteArray.Min(): Byte;
var _:Byte;
begin
  se.MinMaxTBA(Self,Result,_);
end;



{!DOCREF} {
  @method: function TByteArray.Max(): Byte;
  @desc: Returns the maximum value in the array
}
function TByteArray.Max(): Byte;
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
  Result := exp_ArgMinB(mat).x;
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
  Result := exp_ArgMinExB(mat,B).x;
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
  Result := exp_ArgMaxB(mat).x;
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
  Result := exp_ArgMaxExB(mat,B).x;
end;

















