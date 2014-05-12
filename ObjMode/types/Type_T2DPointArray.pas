{!DOCTOPIC}{ 
  Type » T2DPointArray
}

{!DOCREF} {
  @method: function T2DPointArray.Clone(): T2DPointArray;
  @desc: Returns a copy of the array
}
function T2DPointArray.Clone(): T2DPointArray;
begin
  Result := CopyATPA(Self);
end;


{!DOCREF} {
  @method: function T2DPointArray.Len(): Int32;
  @desc: Returns the length of the ATPA. Same as c'Length(ATPA)'
}
function T2DPointArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function T2DPointArray.IsEmpty(): Boolean;
  @desc: Returns True if the ATPA is empty. Same as c'Length(ATPA) = 0'
}
function T2DPointArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure T2DPointArray.Append(const TPA:TPointArray);
  @desc: Add another TPA to the ATPA
}
procedure T2DPointArray.Append(const TPA:TPointArray);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := TPA;
end;


{!DOCREF} {
  @method: function T2DPointArray.Pop(): TPointArray;
  @desc: Removes and returns the last item in the array
}
function T2DPointArray.Pop(): TPointArray;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function T2DPointArray.Slice(Start,Stop: Int32): T2DPointArray;
  @desc: Returns a slice of the array
}
function T2DPointArray.Slice(Start,Stop: Int32): T2DPointArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); //hum hum
end;


{!DOCREF} {
  @method: function T2DPointArray.Merge(): TPointArray;
  @desc: Merges all the groups in the ATPA, and return the TPA
}
function T2DPointArray.Merge(): TPointArray;
begin
  Result := MergeATPA(Self);
end;


{!DOCREF} {
  @method: function T2DPointArray.Sorted(Key:TSortKey=sort_Default): T2DPointArray;
  @desc: 
    Sorts a copy of the ATPA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
function T2DPointArray.Sorted(Key:TSortKey=sort_Default): T2DPointArray;
begin
  Result := Self.Clone();
  case Key of
    sort_Default, sort_Length: se.SortATPAByLength(Result);
    sort_Mean: se.SortATPAByMean(Result);
    sort_First: se.SortATPAByFirst(Result);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function T2DPointArray.Sorted(Index:Integer): T2DPointArray; overload;
  @desc: Sorts a copy of the ATPA by the given index in each group
}
function T2DPointArray.Sorted(Index:Integer): T2DPointArray; overload;
begin
  Result := Self.Clone();
  se.SortATPAByIndex(Result, Index);
end;


{!DOCREF} {
  @method: procedure T2DPointArray.Sort(Key:TSortKey=sort_Default);
  @desc: 
    Sorts the ATPA with the given key, returns a copy
    Supported keys: c'sort_Default, sort_Length, sort_Mean, sort_First'
}
procedure T2DPointArray.Sort(Key:TSortKey=sort_Default);
begin
  case Key of
    sort_Default, sort_Length: se.SortATPAByLength(Self);
    sort_Mean: se.SortATPAByMean(Self);
    sort_First: se.SortATPAByFirst(Self);
  else
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: procedure T2DPointArray.Sort(Index:Integer); overload;
  @desc: 
    Sorts the ATPA by the given index in each group. If the group is not that large it will be set to the last item in that group.
    Negative 'index' will result in counting from right to left: High(Arr[i]) - index
  
}
procedure T2DPointArray.Sort(Index:Integer); overload;
begin
  se.SortATPAByIndex(Self, Index);
end;



{!DOCREF} {
  @method: function T2DPointArray.Reversed(): T2DPointArray;
  @desc:  
    Creates a reversed copy of the array
  
}
function T2DPointArray.Reversed(): T2DPointArray;
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure T2DPointArray.Reverse();
  @desc:  
    Reverses the array  
  
}
procedure T2DPointArray.Reverse();
var
  i, Hi, Mid: Integer;
  tmp:TPointArray;
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