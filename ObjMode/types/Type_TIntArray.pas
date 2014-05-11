{!DOCTOPIC}{ 
  Type » TIntArray
}

{!DOCREF} {
  @method: function TIntArray.Clone(): TIntArray;
  @desc: Returns a copy of the array
}
function TIntArray.Clone(): TIntArray;
begin
  Result := Copy(Self);
end;


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
procedure TIntArray.Append(const Value:Int32);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Value;
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
  @method: function TIntArray.Slice(Start,Stop: Int32): TIntArray;
  @desc: Returns a slice of the array
}
function TIntArray.Slice(Start,Stop: Int32): TIntArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
end;


{!DOCREF} {
  @method: procedure TIntArray.Sort(key:TSortKey=sort_Default);
  @desc: Sorts the array
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
  @desc:  Sorts and returns a copy of the array.
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
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure TIntArray.Reverse();
  @desc: Reverses the array
}
procedure TIntArray.Reverse();
var
  i, Hi, Mid, tmp: Integer;
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
  @method: function TIntArray.Sum(): Int32;
  @desc: Adds up the TIA and returns the sum
}
function TIntArray.Sum(): Int32;
begin
  Result := se.SumTIA(Self);
end;


{!DOCREF} {
  @method: function TIntArray.Sum64(): Int64;
  @desc: Adds up the TIA and returns the sum
}
function TIntArray.Sum64(): Int64;
begin
  Result := se.SumTIA(Self);
end;


{!DOCREF} {
  @method: function TIntArray.Avg(): Extended;
  @desc:Returns the mean value of the array. Use round, trunc or floor to get an c'Int' value.
}
function TIntArray.Avg(): Extended;
begin
  Result := Self.Sum64() / Length(Self);
end;



{!DOCREF} {
  @method: function TIntArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TIntArray.Stdev(): Extended;
var
  i:Int32;
  mean:Extended;
  square:TExtArray;
begin
  mean := Self.Avg();
  SetLength(square,Length(Self));
  for i:=0 to High(self) do Square[i] := Sqr(Self[i] - mean);
  Result := sqrt(square.Avg());
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
  cur := arr[0]-1;
  hits := 0;
  best := 0;
  for i := 0 to High(Arr) do
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
end;



{!DOCREF} {
  @method: function TIntArray.Min(): Int32;
  @desc: Returns the minimum value in the array
}
function TIntArray.Min(): Int32;
var _:Int32;
begin
  se.MinMaxTIA(Self,Result,_);
end;



{!DOCREF} {
  @method: function TIntArray.Max(): Int32;
  @desc: Returns the maximum value in the array
}
function TIntArray.Max(): Int32;
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
  mat:T2DIntArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMinI(mat).x;
end;



{!DOCREF} {
  @method: function TIntArray.ArgMin(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the smallest element in the array within the lower and upper bounds c'lo, hi'.
}
function TIntArray.ArgMin(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DIntArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMinExI(mat,B).x;
end;



{!DOCREF} {
  @method: function TIntArray.ArgMax(): Int32;
  @desc: Returns the index containing the largest element in the array.
}
function TIntArray.ArgMax(): Int32;
var 
  mat:T2DIntArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  Result := exp_ArgMaxI(mat).x;
end;



{!DOCREF} {
  @method: function TIntArray.ArgMax(Lo,Hi:int32): Int32; overload;
  @desc: Returns the index containing the largest element in the array within the lower and upper bounds c'lo, hi'.
}
function TIntArray.ArgMax(lo,hi:Int32): Int32; overload;
var 
  B: TBox;
  mat:T2DIntArray;
begin
  SetLength(Mat,1);
  mat[0] := Self;
  B := [lo,0,hi,0];
  Result := exp_ArgMaxExI(mat,B).x;
end;

















