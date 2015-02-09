{!DOCTOPIC}{ 
  Type � TStringArray
}
{$IFDEF SE_TOSTR}
function ToString(x:Array of AnsiString): String; override;
var i:=0;
begin
  if High(x) = -1 then Exit();
  Result := '['+#39+x[0]+#39;
  for i:=1 to High(x) do Result += ', '+ #39+x[i]+#39;
  Result += ']';
end;

function ToString(x:Array of WideString): String; override;
var i:=0;
begin
  if High(x) = -1 then Exit();
  Result := '['+#39+x[0]+#39;
  for i:=1 to High(x) do Result += ', '+ #39+x[i]+#39;
  Result += ']';
end;
{$ENDIF}

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
procedure TStringArray.Append(const Str:String); {$IFDEF SRL6}override;{$ENDIF}
var
  l:Int32;
begin
  l := Length(Self);
  SetLength(Self, l+1);
  Self[l] := Str;
end;


{!DOCREF} {
  @method: procedure TStringArray.Insert(idx:Int32; Value:String);
  @desc: 
    Inserts a new item `value` in the array at the given position. If position `idx` is greater then the length, 
    it will append the item `value` to the end. If it's less then 0 it will substract the index from the length of the array.[br]
    
    `Arr.Insert(0, x)` inserts at the front of the list, and `Arr.Insert(length(a), x)` is equivalent to `Arr.Append(x)`.
}
procedure TStringArray.Insert(idx:Int32; Value:String);
var i,l:Int32;
begin
  l := Length(Self);
  if (idx < 0) then
    idx := se.modulo(idx,l);

  if (l <= idx) then begin
    self.append(value);
    Exit();
  end;

  SetLength(Self, l+1);
  for i:=l downto idx+1 do
    Self[i] := Self[i-1];
  Self[i] := Value;
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
  @method: function TStringArray.PopLeft(): String;
  @desc: Removes and returns the first item in the array
}
function TStringArray.PopLeft(): String;
begin
  Result := Self[0];
  Self := Self.Slice(1,-1);
end;


{!DOCREF} {
  @method: function TStringArray.Slice(Start,Stop:Int64; Step:Int32=1): TStringArray;
  @desc:
    Slicing similar to slice in Python, tho goes from 'start to and including stop'
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on `length(..)`[br]
    
    If c'Start >= Stop', and c'Step <= -1' it will result in reversed output.[br]
    
    [note]Don't pass positive c'Step', combined with c'Start > Stop', that is undefined[/note]
}
function TStringArray.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): TStringArray;
begin
  if Step = 0 then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(),ERR_NOTICE); end;
end;


{!DOCREF} {
  @method: procedure TStringArray.Extend(Arr:TStringArray);
  @desc: Extends the TSA with another TSA
}
procedure TStringArray.Extend(Arr:TStringArray);
var L,i:Int32;
begin
  L := Length(Self);
  SetLength(Self, Length(Arr) + L);
  for i:=L to High(Self) do
    Self[i] := Copy(Arr[i-L]);
end; 


{!DOCREF} {
  @method: procedure TStringArray.Sort(key:ESortKey=sort_Default; IgnoreCase:Boolean=False);
  @desc: 
    Sorts the array of strings
    Supported keys: c'sort_Default, sort_lex, sort_logical'
}
procedure TStringArray.Sort(key:ESortKey=sort_Default; IgnoreCase:Boolean=False);
begin
  case key of
    sort_default, sort_lex: se.SortTSA(Self,IgnoreCase);
    sort_logical: se.SortTSANatural(Self);
  else 
    WriteLn('TSortKey not supported');
  end;
end;


{!DOCREF} {
  @method: function TStringArray.Sorted(key:ESortKey=sort_Default; IgnoreCase:Boolean=False): TStringArray;
  @desc:  
    Sorts and returns a copy of the array.
    Supports the keys: c'sort_Default, sort_lex, sort_logical'
    [note]Partial, key not supported fully yet[/note]
}
function TStringArray.Sorted(key:ESortKey=sort_Default; IgnoreCase:Boolean=False): TStringArray;
begin
  Result := Self.Slice();
  case key of
    sort_default, sort_lex: se.SortTSA(Result,IgnoreCase);
    sort_logical: se.SortTSANatural(Result);
  else 
    WriteLn('TSortKey not supported');
  end;
end;




{!DOCREF} {
  @method: function StringArray.Find(value:String): Int32;
  @desc: Searces for the given value and returns the first position from the left.
}
function TStringArray.Find(value:String): Int32;
begin
  Result := se.Find(self,value);
end;


{!DOCREF} {
  @method: function TStringArray.Find(Sequence:TStringArray): Int32; overload;
  @desc: Searces for the given sequence and returns the first position from the left.
}
function TStringArray.Find(Sequence:TStringArray): Int32; overload;
begin
  Result := se.Find(Self,Sequence);
end;


{!DOCREF} {
  @method: function TStringArray.FindAll(Value:String): TIntArray;
  @desc: Searces for the given value and returns all the position where it was found.
}
function TStringArray.FindAll(Value:String): TIntArray;
begin
  Result := se.FindAll(Self,Value);
end;


{!DOCREF} {
  @method: function TStringArray.FindAll(Sequence:TStringArray): TIntArray; overload;
  @desc: Searces for the given sequence and returns all the position where it was found.
}
function TStringArray.FindAll(Sequence:TStringArray): TIntArray; overload;
begin
  Result := se.FindAll(Self,Sequence);
end;


{!DOCREF} {
  @method: function TStringArray.Contains(value:String): Boolean;
  @desc: Checks if the array contains the given value `value`.
}
function TStringArray.Contains(value:String): Boolean;
begin
  Result := se.Find(Self,value) <> -1;
end;


{!DOCREF} {
  @method: function TStringArray.Count(value:String): Boolean;
  @desc:   Counts the number of occurances of the given value `value`.
}
function TStringArray.Count(value:String): Boolean;
begin
  Result := Length(se.FindAll(self, value));
end;


{!DOCREF} {
  @method: function TStringArray.Reversed(): TStringArray;
  @desc:   Creates a reversed copy of the array
}
function TStringArray.Reversed(): TStringArray;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure TStringArray.Reverse();
  @desc:  Reverses the array  
}
procedure TStringArray.Reverse();
begin
  Self := Self.Slice(,,-1);
end;


{=============================================================================}
// The functions below this line is not in the standard array functionality
//
// By "standard array functionality" I mean, functions that all standard
// array types should have.
{=============================================================================}


{!DOCREF} {
  @method: function TStringArray.Merge(Sep:String): String;
  @desc: ?
}
function TStringArray.Merge(Sep:String): String;
var i,hi:Int32;
begin
  hi := High(Self);
  if hi = -1 then Exit('');
  for i:=0 to hi-1 do
    Result += Self[i] + Sep;
  Result += Self[hi];
end;



{!DOCREF} {
  @method: function TStringArray.Capital(): TStringArray;
  @desc: Return a copy of the array with each strings first character capitalized and the rest lowercased.
}
function TStringArray.Capital(): TStringArray;
var i:Int32;
begin
  Result := Self.Slice();
  for i:=0 to High(Self) do
    Result[i] := Capitalize(Result[i]);
end;


{!DOCREF} {
  @method: function TStringArray.Lower(): TStringArray;
  @desc: Return a copy of the array with each string lowercased.
}
function TStringArray.Lower(): TStringArray;
var i:Int32;
begin
  Result := Self.Slice();
  for i:=0 to High(Self) do
    Result[i] := LowerCase(Result[i]);
end;


{!DOCREF} {
  @method: function TStringArray.Upper(): TStringArray;
  @desc: Return a copy of the array with each string uppercased.
}
function TStringArray.Upper(): TStringArray;
var i:Int32;
begin
  Result := Self.Slice();
  for i:=0 to High(Self) do
    Result[i] := UpperCase(Result[i]);
end;



{!DOCREF} {
  @method: function TStringArray.Mode(IgnoreCase:Boolean=True): String;
  @desc:  
    Returns the sample mode of the array, which is the most frequently occurring value in the array.
    When there are multiple values occurring equally frequently, mode returns the "smallest" of those values.
    [code=pascal]
    var Arr: TStringArray = ['red', 'blue', 'blue', 'red', 'green', 'red', 'red'];
    begin
      WriteLn(Arr.Mode());
    end.
    [/code]
}
function TStringArray.Mode(IgnoreCase:Boolean=True): String;
var
  arr:TStringArray;
  cur:String;
  i,hits,best: Int32;
begin
  arr := self.sorted(sort_lex,IgnoreCase);
  cur := arr[0];
  hits := 1;
  best := 0;
  for i := 1 to High(Arr) do
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
