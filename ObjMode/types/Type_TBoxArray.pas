{!DOCTOPIC}{ 
  Type » TBoxArray
}

{!DOCREF} {
  @method: function TBoxArray.Clone(): TBoxArray;
  @desc: Returns a copy of the array
}
function TBoxArray.Clone(): TBoxArray;
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
  @method: function TBoxArray.Slice(Start,Stop: Int32; Step:Int32=1): TBoxArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on length(..)
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TBoxArray.Slice(Start,Stop: Int32; Step:Int32=1): TBoxArray;
begin
  if Step = 0 then Exit;
  try exp_slice(Self, Start,Stop,Step,Result);
  except end;
end;


{!DOCREF} {
  @method: procedure TBoxArray.Sort(key:TSortKey=sort_Default);
  @desc: Sorts the array [not supported]
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
  @desc: Sorts and returns a copy of the array [not supported]
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
begin
  Result := Self.Slice(-1,0,-1);
end;


{!DOCREF} {
  @method: procedure TBoxArray.Reverse();
  @desc:  Reverses the array 
}
procedure TBoxArray.Reverse();
begin
  Self := Self.Slice(-1,0,-1);
end;
