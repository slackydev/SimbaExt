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
  @method: function T2DPointArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DPointArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function T2DPointArray.Slice(Start,Stop: Int32; Step:Int32=1): T2DPointArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
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
begin
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure T2DPointArray.Reverse();
  @desc:  
    Reverses the array  
}
procedure T2DPointArray.Reverse();
begin
  Self := Self.Slice(-1,0,-1);
end;