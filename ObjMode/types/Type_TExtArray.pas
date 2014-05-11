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
  @method: function TExtArray.Slice(Start,Stop: Int32): TExtArray;
  @desc: Returns a slice of the array
}
function TExtArray.Slice(Start,Stop: Int32): TExtArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
end;


{!DOCREF} {
  @method: procedure TExtArray.Sort(key:TSortKey=sort_Default);
  @desc: Sorts the array
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
  @desc:  Sorts and returns a copy of the array.
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
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure TExtArray.Reverse();
  @desc: Reverses the array
}
procedure TExtArray.Reverse();
var
  i,Hi,Mid: Integer;
  tmp:Extended;
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
  @method: function TExtArray.Sum(): Extended;
  @desc: Adds up the TEA and returns the sum
}
function TExtArray.Sum(): Extended;
begin
  Result := se.SumTEA(Self);
end;



{!DOCREF} {
  @method: function TExtArray.Avg(): Extended;
  @desc:Returns the mean value of the array
}
function TExtArray.Avg(): Extended;
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
  mean:Extended;
  square:TExtArray;
begin
  mean := Self.Avg();
  SetLength(square,Length(Self));
  for i:=0 to High(self) do Square[i] := Sqr(Self[i] - mean);
  Result := sqrt(square.Avg());
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
  cur := arr[0] - 1.0;
  hits := 0;
  best := 0;
  for i := 0 to High(Arr) do
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
