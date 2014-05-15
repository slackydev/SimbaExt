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
  @method: function TIntArray.Slice(Start,Stop: Int32; Step:Int32=1): TIntArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.

    [b]Examples:[/b]
    [code=pascal]
    TIA := [0,1,2,3,4,5,6,7,8,9];
    TIA.Slice(9,0,-1)  = [9,8,7,6,5,4,3,2,1,0]  //Copies from 9 downto 0, with a step-size of 1.
    TIA.Slice(9,0,-2)  = [9,7,5,3,1]            //Copies from 9 downto 0, with a step-size of 2.
    TIA.Slice(3,7,1)   = [3,4,5,6,7]            //Copies from 2 to 7
    TIA.Slice(0,-2,1)  = [0,1,2,3,4,5,6,7,8]    //Copies from 1 to Len-2
    [/code]

    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TIntArray.Slice(Start,Stop: Int32; Step:Int32=1): TIntArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
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
    Sorts and returns a copy of the array.
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
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure TIntArray.Reverse();
  @desc: Reverses the array
}
procedure TIntArray.Reverse();
begin
  Self := Self.Slice(-1,0,-1);
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
  @method: function TIntArray.Mean(): Extended;
  @desc:Returns the mean value of the array. Use round, trunc or floor to get an c'Int' value.
}
function TIntArray.Mean(): Extended;
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

















