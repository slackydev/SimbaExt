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
  @method: procedure TIntArray.Append(const Str:Int32);
  @desc: Add another item to the array
}
procedure TIntArray.Append(const Str:Int32);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Str;
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
