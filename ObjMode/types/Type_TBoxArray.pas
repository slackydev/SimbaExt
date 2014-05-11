{!DOCTOPIC}{ 
  Type » TBoxArray
}

{!DOCREF} {
  @method: function TBoxArray.Clone(): TBoxArray;
  @desc: Returns a copy of the array
}
function TBoxArray.Clone(): TBoxArray;
var
  i:Int32;
begin
  Result := Copy(Self);
end;


{!DOCREF} {
  @method: function TBoxArray.Len(): Int32;
  @desc: Returns the length of the array. Same as 'Length(arr)'
}
function TBoxArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TBoxArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as 'Length(arr) = 0'
}
function TBoxArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TBoxArray.Append(const B:TBox);
  @desc: Add another string to the array
}
procedure TBoxArray.Append(const B:TBox);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := B;
end;


{!DOCREF} {
  @method: function TBoxArray.Pop(): TBox;
  @desc: Removes and returns the last item in the array
}
function TBoxArray.Pop(): TBox;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TBoxArray.Slice(Start, Stop: Int32): TBoxArray;
  @desc: Returns a slice of the array
}
function TBoxArray.Slice(Start,Stop: Int32): TBoxArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
end;


{!DOCREF} {
  @method: procedure TBoxArray.Sort(key:TSortKey=sort_Default);
  @desc: Sorts the array
}
procedure TBoxArray.Sort(key:TSortKey=sort_Default);
begin
  //case key of
  //  sort_default, sort_lex: se.SortTSA(Self,IgnoreCase);
  //  sort_logical: se.SortTSANatural(Self);
  //else 
  //  WriteLn('TSortKey not supported');
  //end;
  WriteLn('TBoxArray sorting is not supported yet');
end;


{!DOCREF} {
  @method: function TStringArray.Sorted(key:TSortKey=sort_Default; IgnoreCase:Boolean=False): TStringArray;
  @desc: Sorts and returns a copy of the array.
}
function TBoxArray.Sorted(key:TSortKey=sort_Default): TStringArray;
begin
  //Result := Self.Clone();
  //case key of
  //  sort_default, sort_lex: se.SortTSA(Result,IgnoreCase);
  //  sort_logical: se.SortTSANatural(Result);
  //else 
  //  WriteLn('TSortKey not supported');
  //end;
  WriteLn('TBoxArray sorting is not supported yet');
end;




{!DOCREF} {
  @method: function TBoxArray.Reversed(): TBoxArray;
  @desc: Creates a reversed copy of the array
}
function TBoxArray.Reversed(): TBoxArray;
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure TBoxArray.Reverse();
  @desc:  Reverses the array 
}
procedure TBoxArray.Reverse();
var
  i, Hi, Mid: Integer;
  tmp: TBox ;
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
