{!DOCTOPIC}{ 
  Type » TStringArray
}

{!DOCREF} {
  @method: function TStringArray.Clone(): TStringArray;
  @desc: Returns a copy of the array
}
function TStringArray.Clone(): TStringArray;
var
  i:Int32;
begin
  SetLength(Result, Length(Self));
  for i:=0 to High(Self) do
    Result[i] := Copy(Self[i]);
end;


{!DOCREF} {
  @method: function TStringArray.Len(): Int32;
  @desc: Returns the length of the array. Same as c'Length(arr)'
}
function TStringArray.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function TStringArray.IsEmpty(): Boolean;
  @desc: Returns True if the array is empty. Same as c'Length(arr) = 0'
}
function TStringArray.IsEmpty(): Boolean;
begin
  Result := Length(Self) = 0;
end;


{!DOCREF} {
  @method: procedure TStrngArray.Append(const Str:String);
  @desc: Add another string to the array
}
procedure TStringArray.Append(const Str:String);
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Str;
end;


{!DOCREF} {
  @method: function TStringArray.Pop(): String;
  @desc: Removes and returns the last item in the array
}
function TStringArray.Pop(): String;
var
  H:Int32;
begin
  H := high(Self);
  Result := Self[H];
  SetLength(Self, H);
end;


{!DOCREF} {
  @method: function TStringArray.Slice(Start, Stop: Int32): TStringArray;
  @desc: Returns a slice of the array
}
function TStringArray.Slice(Start,Stop: Int32): TStringArray;
begin
  if Stop <= -1 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop); 
end;


{!DOCREF} {
  @method: function TStringArray.Capital(): TStringArray;
  @desc: Return a copy of the array with each strings first character capitalized and the rest lowercased.
}
function TStringArray.Capital(): TStringArray;
var i:Int32;
begin
  Result := Self.Clone();
  for i:=0 to High(Self) do
    Capitalize(Result[i]);
end;


{!DOCREF} {
  @method: procedure TStringArray.Sort(key:TSortKey=sort_Default; IgnoreCase:Boolean=False);
  @desc: Sorts the array
}
procedure TStringArray.Sort(key:TSortKey=sort_Default; IgnoreCase:Boolean=False);
begin
  case key of
    sort_default, sort_lex: se.SortTSA(Self,IgnoreCase);
    sort_logical: se.SortTSANatural(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TStringArray.Sorted(key:TSortKey=sort_Default; IgnoreCase:Boolean=False): TStringArray;
  @desc:  
    Sorts and returns a copy of the array.
    [note]Partial, key not supported fully yet[/note]
}
function TStringArray.Sorted(key:TSortKey=sort_Default; IgnoreCase:Boolean=False): TStringArray;
begin
  Result := Self.Clone();
  case key of
    sort_default, sort_lex: se.SortTSA(Result,IgnoreCase);
    sort_logical: se.SortTSANatural(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;




{!DOCREF} {
  @method: function TStringArray.Reversed(): TStringArray;
  @desc:  
    Creates a reversed copy of the array
  
}
function TStringArray.Reversed(): TStringArray;
var hi,i:Int32;
begin
  hi := High(Self);
  SetLength(Result, hi+1);
  for i:=0 to hi do
    Result[hi-i] := Self[i];
end;


{!DOCREF} {
  @method: procedure TStringArray.Reverse();
  @desc:  
    Reverses the array  
  
}
procedure TStringArray.Reverse();
var
  i, Hi, Mid: Integer;
  tmp:String;
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
