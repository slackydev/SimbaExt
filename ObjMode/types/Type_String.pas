(*=============================================================================|
 String functionality
|=============================================================================*)
{!DOCTOPIC} { 
  Type » String
}

{!DOCREF} {
  @method:
    function String.Clone(): String;
  @desc:
    Returns a copy of the array 
}
function String.Clone(): String;
begin
  Result := Copy(Self);
end;


{!DOCREF} {
  @method: function String.Len(): Int32;
  @desc:   Returns the length of the String. Same as `Length(Str)`
}
function String.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function String.StrLen(): Int32;
  @desc:   Returns the null-terinated length of the String.
}
function String.StrLen(): Int32;
var P: PChar;
Begin
  P := @Self[1];
  while (P^ <> #0) do Inc(P);
  Result := Int32(P) - Int32(@Self[1]);
end;


{!DOCREF} {
  @method: function String.Slice(Start,Stop: Int32; Step:Int32=1): String;
  @desc:
    Slicing similar to slice in Python, tho goes from "start to and including stop"
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on c'length(..) + 1'
    
    If `Start >= Stop`, and `Step <= -1` it will result in reversed output.
    
    [note]Don't pass positive `Step`, combined with `Start > Stop`, that is undefined[/note]
}
function String.Slice(Start,Stop: Int32; Step:Int32=1): String;
begin
  if (Step = 0) then Exit;
  try Result := exp_slice(Self, Start,Stop,Step);
  except 
    RaiseException(erOutOfRange,'Must be in range of "1..Length(Str)", given: Start: '+ToStr(Start)+', Stop: '+ToStr(Stop)+', Step: '+ToStr(Step));
  end;
end;


{!DOCREF} {
  @method: procedure String.Extend(Str:String);
  @desc: Extends the string with a string
}
procedure String.Extend(Str:String);
begin
  Self := Self + Str;
end; 


{!DOCREF} {
  @method: function String.Pos(Sub:String): Int32;
  @desc:   Return the lowest index in the string where substring `Sub` is located. 0 if not found
}
function String.Pos(Sub:String): Int32;
begin
  Result := se.StrPosL(Sub,Self);
end;


{!DOCREF} {
  @method: function String.rPos(Sub:String): Int32;
  @desc:   Return the highest index in the string where substring `Sub` is located. 0 if not found
}
function String.rPos(Sub:String): Int32;
begin
  Result := se.StrPosR(Sub,Self);
end;


{!DOCREF} {
  @method: function String.PosMulti(Sub:String): TIntArray;
  @desc:   Return all the index in the string where substring `Sub` is located. Empty is not found
}
function String.PosMulti(Sub:String): TIntArray;
begin
  Result := se.StrPosEx(Sub,Self);
end;

{!DOCREF} {
  @method: function String.Find(Value:String): Int32;
  @desc: 
    Searces for the given value and returns the first position from the left.
    [note] Same as `String.Pos(..)` [/note]
}
function String.Find(Value:String): Int32;
begin
  Result := exp_Find(Self,Value);
end;


{!DOCREF} {
  @method: function String.FindAll(Value:String): TIntArray; overload;
  @desc: 
    Searces for the given value and returns all the position where it was found.
    [note]Same Result as `String.PosMulti(..)` but less optimized for the task[/note]
}
function String.FindAll(Value:String): TIntArray; overload;
begin
  Result := exp_FindAll(Self,Value);
end;



{!DOCREF} {
  @method: function String.Contains(val:String): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function String.Contains(val:String): Boolean;
begin
  Result := Self.Find(val) <> 0;
end;


{!DOCREF} {
  @method: function String.Count(val:String): Int32;
  @desc: Counts all the occurances of the given value `val`
}
function String.Count(val:String): Int32;
begin
  Result := Length(Self.PosMulti(val));
end;


{!DOCREF} {
  @method: function String.Strip(const Chars:String=' '): String;
  @desc:
    Return a copy of the string with leading and trailing characters removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the both ends of the string this method is called on.
    [code=pascal]
      >>> WriteLn( '   spacious   '.Strip() );
      'spacious'
      >>> WriteLn( 'www.example.com'.Strip('cmow.') ); 
      'example'
    [/code]
}
function String.Strip(const Chars:String=' '): String;
begin
  Result := exp_StrStrip(Self, Chars);
end;


{!DOCREF} {
  @method: function String.lStrip(const Chars:String=' '): String;
  @desc:
    Return a copy of the string with leading removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the beginning  of the string this method is called on.
    [code=pascal]
      >>> WriteLn( '   spacious   '.lStrip() );
      'spacious   '
      >>> WriteLn( 'www.example.com'.lStrip('cmowz.') ); 
      'example.com'
    [/code]
}
function String.lStrip(const Chars:String=' '): String;
begin
  Result := exp_StrStripL(Self, Chars);
end;


{!DOCREF} {
  @method: function String.rStrip(const Chars:String=' '): String;
  @desc:
    Return a copy of the string with trailing removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the end  of the string this method is called on.
    [code=pascal]
      >>> WriteLn( '   spacious   '.rStrip() );
      '   spacious'
      >>> WriteLn( 'mississippi'.rStrip('ipz') ); 
      'mississ'
    [/code]
}
function String.rStrip(const Chars:String=' '): String;
begin
  Result := exp_StrStripR(Self, Chars);
end;


{!DOCREF} {
  @method: function String.Reversed(): String;
  @desc: Creates a reversed copy of the string

}
function String.Reversed():string;
begin
  Result := Self.Slice(-1,1,-1);
end;


{!DOCREF} {
  @method: procedure String.Reverse();
  @desc: Reverses the string
}
procedure String.Reverse();
begin
  Self := Self.Slice(-1,1,-1);
end;


{!DOCREF} {
  @method: function String.Replace(old, new:String; Flags:TReplaceFlags=[rfReplaceAll]): String;
  @desc:   
    Return a copy of the string with all occurrences of substring old replaced by new.
    [note]Should be a much faster then Simbas c'Replace(...)'[/note]
}
function String.Replace(old, new:String; Flags:TReplaceFlags=[rfReplaceAll]): String;
begin
  Result := se.StrReplace(Self, old, new, Flags);
end;


{!DOCREF} {
  @method: function String.Split(sep:String): TStringArray;
  @desc:
    Return an array of the words in the string, using 'sep' as the delimiter string.
    [note]Should be a tad faster then Simbas c'Explode(...)'[/note]
}
function String.Split(Sep:String=' '): TStringArray;
begin
  Result := se.StrExplode(self,sep);
end;


{!DOCREF} {
  @method: function String.Join(TSA:TStringArray): String;
  @desc:
    Return a string which is the concatenation of the strings in the array 'TSA'. 
    The separator between elements is the string providing this method. 
}
function String.Join(TSA:TStringArray): String;
begin
  Result := Implode(Self, TSA);
end;
//fix for single char evaulation.
function Char.Join(TSA:TStringArray): String;
begin
  Result := Implode(Self, TSA);
end;


{!DOCREF} {
  @method: function String.Mul(x:uInt32): String;
  @desc:   Repeats the string `x` times
}
function String.Mul(x:Int32): String;
var
  i,H: Int32;
begin
  if Length(Self) = 0 then 
    Exit('');
  Result := Self;
  H := Length(Self);
  SetLength(Result, H*x);
  Dec(x);
  for i:=1 to x do
    MemMove(Self[1], Result[1+H*i], H);
end;

function Char.Mul(x:Int32): String;
var
  i,H: Int32;
begin
  Result := Self;
  dec(x);
  for i:=1 to x do
    Result := Result + Self;
end;



{!DOCREF} {
  @method: function String.StartsWith(Prefix:String): Boolean;
  @desc:   Returns True if the string starts with `Prefix`.
}
function String.StartsWith(Prefix:String): Boolean;
var
  i: Int32;
begin
  if Length(Prefix) > Length(Self) then 
    Exit(False);
  Result := True;
  for i:=1 to Length(Prefix) do
    if (Prefix[i] <> Self[i]) then
      Exit(False);
end;


{!DOCREF} {
  @method: function String.EndsWith(Suffix:String): Boolean;
  @desc:   Returns True if the string ends with `Suffix`.
}
function String.EndsWith(Suffix:String): Boolean;
var
  i,l: Int32;
begin
  if Length(Suffix) > Length(Self) then
    Exit(False);
  Result := True;
  l := Length(Self);
  for i:=1 to Length(Suffix) do
    if (Suffix[i] <> Self[l-Length(Suffix)+i]) then
      Exit(False);
end;


{!DOCREF} {
  @method: function String.Capital(): String;
  @desc:   Return a copy of the string with the first character in each word capitalized.
}
function String.Capital(): String;
begin
  Result := Capitalize(Self);
end;


{!DOCREF} {
  @method: function String.Upper(): String;
  @desc:   Return a copy of the string with all the chars converted to uppercase.
}
function String.Upper(): String;
begin
  Result := Uppercase(Self);
end;


{!DOCREF} {
  @method: function String.Lower(): String;
  @desc:   Return a copy of the string with all the chars converted to lowercase.
}
function String.Lower(): String;
begin
  Result := Lowercase(Self);
end;



{!DOCREF} {
  @method: function String.IsAlphaNum(): Boolean;
  @desc:   Return true if all characters in the string are alphabetic or numerical and there is at least one character, false otherwise.
}
function String.IsAlphaNum(): Boolean;
var ptr: PChar; hiptr:UInt32;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  hiptr := UInt32(UInt32(ptr) + Length(self));
  while UInt32(ptr) < hiptr do
    if not (ptr^ in ['0'..'9','a'..'z','A'..'Z']) then
      Exit(False)
    else
      Inc(ptr);
  Result := True;
end;

function Char.IsAlphaNum(): Boolean;
begin
  Result := (Self in ['A'..'Z', 'a'..'z','0'..'9']);
end;


{!DOCREF} {
  @method: function String.IsAlpha(): Boolean;
  @desc:   Return true if all characters in the string are alphabetic and there is at least one character, false otherwise.
}
function String.IsAlpha(): Boolean;
var ptr: PChar; hiptr:UInt32;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  hiptr := UInt32(UInt32(ptr) + Length(self));
  while UInt32(ptr) < hiptr do
    if not (ptr^ in ['A'..'Z', 'a'..'z']) then
      Exit(False)
    else
      Inc(ptr);
  Result := True;
end;

function Char.IsAlpha(): Boolean;
begin
  Result := (Self in ['A'..'Z', 'a'..'z']);
end;


{!DOCREF} {
  @method: function String.IsDigit(): Boolean;
  @desc:   Return true if all characters in the string are digits and there is at least one character, false otherwise.
}
function String.IsDigit(): Boolean;
var ptr: PChar; hiptr:UInt32;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  hiptr := UInt32(UInt32(ptr) + Length(self));
  while UInt32(ptr) < hiptr do
    if not (ptr^ in ['0'..'9']) then
      Exit(False)
    else
      Inc(ptr);
  Result := True;
end;

function Char.IsDigit(): Boolean;
begin
  Result := (Self in ['0'..'9']);
end;



{!DOCREF} {
  @method: function String.IsFloat(): Boolean;
  @desc:   Return true if all characters in the string are digits + "." and there is at least one character, false otherwise.
}
function String.IsFloat(): Boolean;
var ptr: PChar; hiptr:UInt32; i:Int32; dotAdded:Boolean;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  i:=0;
  hiptr := UInt32(UInt32(ptr) + Length(self));
  while UInt32(ptr) < hiptr do
    if not (ptr^ in ['0'..'9']) then
      if (i >= 1) and (ptr^ = '.') and not(dotAdded) then
      begin
        Inc(ptr);
        inc(i);
        dotAdded:=True;
      end else
        Exit(False)
    else begin
      Inc(ptr);
      inc(i);
    end;
  Result := True;
end;

function Char.IsFloat(): Boolean;
begin
  Result := String(Self).IsFloat();
end;



{!DOCREF} {
  @method: function String.GetNumbers(): TIntArray;
  @desc:   Returns all the numbers in the string, does not handle floating point numbers
}
function String.GetNumbers(): TIntArray;
var
  i,j,l:Int32;
  NextNum: Boolean;
  Num: String;
begin
  j := 0;
  num := '';
  L := Length(Self);
  for i:=1 to L do
    if Self[i].IsDigit() then
    begin
      NextNum := False;
      if ( i+1 <= L ) then
        NextNum := Self[i+1].IsDigit();
      Num := Num + Self[i];
      if not(NextNum) then
      begin
        SetLength(Result, j+1);
        Result[j] := StrToInt(num);
        inc(j);
        num := '';
      end;
    end;
end;


{!DOCREF} {
  @method: function String.SplitNum(): TStringArray;
  @desc:   Splits the text in to sentances, and numbers, result will EG be: `['My number is', '123', 'sometimes its', '7.5']`
}
function String.SplitNum(): TStringArray;
var
  i,j,l:Int32;
begin
  L := Length(Self);
  Self := Self + #0#0;
  SetLength(Result, 1);
  j := 0;  i := 1;
  while i <= L do
  begin
    if Self[i].IsDigit() then begin
      Result[j] := Result[j] + Self[i];
      inc(i);
      while (Self[i].IsDigit()) or ((Self[i] = '.') and (Self[i+1].IsDigit())) do
      begin
        Result[j] := Result[j] + Self[i];
        inc(i);
        if (i > L) then break;
      end;
      if (i > L) then break;
      SetLength(Result, length(Result)+1);
      inc(j);
    end else
    begin
      if (Result[j] = '') and (Self[i] = ' ') then 
      begin 
        inc(i)
        if Self[i].IsDigit() then continue;
      end;
      if not(Self[i+1].IsDigit() and (Self[i] = ' ')) then
        Result[j] := Result[j] + Self[i];
      if Self[i+1].IsDigit() then begin
        SetLength(Result, length(Result)+1);
        inc(j);
      end;
    end;
    inc(i);
  end;
  SetLength(Self, L);
end;
