{!DOCTOPIC}{ 
  Type » TExtArray
}

{!DOCREF} {
  @method: function TExtArray.Clone(): TExtArray;
  @desc: Returns a copy of the array
}
function TExtArray.Clone(): TExtArray;
begin
  Result := Copy(Self);
end;


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
  @method: function TExtArray.Slice(Start,Stop: Int32; Step:Int32=1): TExtArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TExtArray.Slice(Start,Stop: Int32; Step:Int32=1): TExtArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
end;


{!DOCREF} {
  @method: procedure TExtArray.Sort(key:TSortKey=sort_Default);
  @desc: 
    Sorts the array
    Supported keys: c'sort_Default'
}
procedure TExtArray.Sort(key:TSortKey=sort_Default);
begin
  case key of
    sort_default: se.SortTEA(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TExtArray.Sorted(key:TSortKey=sort_Default): TExtArray;
  @desc: 
    Sorts and returns a copy of the array.
    Supported keys: c'sort_Default'
}
function TExtArray.Sorted(Key:TSortKey=sort_Default): TExtArray;
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


{!DOCREF} {
  @method: function TExtArray.Sum(): Extended;
  @desc: Adds up the TEA and returns the sum
}
function TExtArray.Sum(): Extended;
begin
  Result := se.SumTEA(Self);
end;



{!DOCREF} {
  @method: function TExtArray.Mean(): Extended;
  @desc:Returns the mean value of the array
}
function TExtArray.Mean(): Extended;
begin
  Result := se.SumTEA(Self) / Length(Self);
end;



{!DOCREF} {
  @method: function TExtArray.Stdev(): Extended;
  @desc: Returns the standard deviation of the array
}
function TExtArray.Stdev(): Extended;
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
  @method: function TExtArray.Variance(): Extended;
  @desc: 
    Return the sample variance. 
    Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of the array. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.
}
function TExtArray.Variance(): Extended;
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
  @method: function TExtArray.Mode(Eps:Extended=0.000001): Extended;
  @desc:
    Returns the sample mode of the array, which is the [u]most frequently occurring value[/u] in the array.
    When there are multiple values occurring equally frequently, mode returns the smallest of those values.
    Takes an extra parameter c'Eps', can be used to allow some tolerance in the floating point comparison.
}
function TExtArray.Mode(Eps:Extended=0.0000001): Extended;
var
  arr:TExtArray;
  i,hits,best: Int32;
  cur:Extended;
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
  @method: function TExtArray.Min(): Extended;
  @desc: Returns the minimum value in the array
}
function TExtArray.Min(): Extended;
var _:Extended;
begin
  se.MinMaxTEA(Self,Result,_);
end;



{!DOCREF} {
  @method: function TExtArray.Max(): Extended;
  @desc: Returns the maximum value in the array
}
function TExtArray.Max(): Extended;
var _:Extended;
begin
  se.MinMaxTEA(Self,_,Result);
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
  Result := exp_ArgMinE(mat).x;
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
  Result := exp_ArgMinExE(mat,B).x;
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
  Result := exp_ArgMaxE(mat).x;
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
  Result := exp_ArgMaxExE(mat,B).x;
end;
