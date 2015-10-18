(*=============================================================================|
 String functionality
|=============================================================================*)
{!DOCTOPIC} { 
  Type » String
}

const STR_WHITESPACE := #9#10#13#32;

{!DOCREF} {
  @method: function String.Len(): Int32;
  @desc:   Returns the length of the String. Same as `Length(Str)`
}
function AnsiString.Len(): Int32;
begin
  Result := Length(Self);
end;


{!DOCREF} {
  @method: function String.StrLen(): Int32;
  @desc:   Returns the null-terinated length of the String.
}
function AnsiString.StrLen(): Int32;
var P: PChar;
Begin
  P := @Self[1];
  while (P^ <> #0) do Inc(P);
  Result := Int32(P) - Int32(@Self[1]);
end;


{!DOCREF} {
  @method: function String.Slice(Start,Stop: Int64; Step:Int32=1): String;
  @desc:
    Slicing similar to slice in Python, tho goes from "start to and including stop"
    Can be used to eg reverse an array, and at the same time allows you to c'step' past items.
    You can give it negative start, and stop, then it will wrap around based on c'length(..) + 1'
    
    If `Start >= Stop`, and `Step <= -1` it will result in reversed output.
    
    [note]Don't pass positive `Step`, combined with `Start > Stop`, that is undefined[/note]
}
function AnsiString.Slice(Start,Stop:Int64=High(Int64); Step:Int32=1): String;
begin
  if (Step = 0) then Exit;
  try Result := se.slice(Self, Start,Stop,Step);
  except RaiseWarning(se.GetException(), ERR_NOTICE); end;
end;


{!DOCREF} {
  @method: procedure String.Extend(Str:String);
  @desc: Extends the string with a string
}
procedure AnsiString.Extend(Str:String);
begin
  Self := Self + Str;
end; 


{!DOCREF} {
  @method: function String.Pos(Sub:String): Int32;
  @desc:   Return the lowest index in the string where substring `Sub` is located. 0 if not found
}
function AnsiString.Pos(Sub:String): Int32;
begin
  Result := se.StrPos(Sub,Self);
end;


{!DOCREF} {
  @method: function String.rPos(Sub:String): Int32;
  @desc:   Return the highest index in the string where substring `Sub` is located. 0 if not found
}
function AnsiString.rPos(Sub:String): Int32;
begin
  Result := se.StrPosR(Sub,Self);
end;


{!DOCREF} {
  @method: function String.PosMulti(Sub:String): TIntArray;
  @desc:   Return all the index in the string where substring `Sub` is located. Empty is not found
}
function AnsiString.PosMulti(Sub:String): TIntArray;
begin
  Result := se.StrPosMulti(Sub,Self);
end;

{!DOCREF} {
  @method: function String.Find(Value:String): Int32;
  @desc: 
    Searces for the given value and returns the first position from the left.
    [note] Same as `String.Pos(..)` [/note]
}
function AnsiString.Find(Value:String): Int32;
begin
  Result := se.StrPos(Value, Self);
end;


{!DOCREF} {
  @method: function String.FindAll(Value:String): TIntArray; overload;
  @desc: 
    Searces for the given value and returns all the position where it was found.
    [note]Same Result as `String.PosMulti(..)` but less optimized for the task[/note]
}
function AnsiString.FindAll(Value:String): TIntArray; overload;
begin
  Result := se.StrPosMulti(Value, Self);
end;



{!DOCREF} {
  @method: function String.Contains(val:String): Boolean;
  @desc: Checks if the arr contains the given value c'val'
}
function AnsiString.Contains(val:String): Boolean;
begin
  Result := se.Find(Self,val) <> 0;
end;


{!DOCREF} {
  @method: function String.Count(Value:String): Int32;
  @desc: Counts all the occurances of the given value `Value`
}
function AnsiString.Count(Value:String): Int32;
begin
  Result := Length(Self.PosMulti(Value));
end;


{!DOCREF} {
  @method: function String.Strip(const Chars:String=STR_WHITESPACE): String;
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
function AnsiString.Strip(const Chars:String=STR_WHITESPACE): String;
begin
  Result := se.StrStrip(Self, Chars);
end;


{!DOCREF} {
  @method: function String.lStrip(const Chars:String=STR_WHITESPACE): String;
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
function AnsiString.lStrip(const Chars:String=STR_WHITESPACE): String;
begin
  Result := se.StrStripL(Self, Chars);
end;


{!DOCREF} {
  @method: function String.rStrip(const Chars:String=STR_WHITESPACE): String;
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
function AnsiString.rStrip(const Chars:String=STR_WHITESPACE): String;
begin
  Result := se.StrStripR(Self, Chars);
end;


{!DOCREF} {
  @method: function String.Reversed(): String;
  @desc: Creates a reversed copy of the string

}
function AnsiString.Reversed():string;
begin
  Result := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: procedure String.Reverse();
  @desc: Reverses the string
}
procedure AnsiString.Reverse();
begin
  Self := Self.Slice(,,-1);
end;


{!DOCREF} {
  @method: function String.Replace(old, new:String; Flags:TReplaceFlags=[rfReplaceAll]): String;
  @desc:   
    Return a copy of the string with all occurrences of substring old replaced by new.
    [note]Should be a much faster then Simbas c'Replace(...)'[/note]
}
function AnsiString.Replace(old, new:String; Flags:TReplaceFlags=[rfReplaceAll]): String;
begin
  Result := se.StrReplace(Self, old, new, Flags);
end;


{!DOCREF} {
  @method: function String.Split(sep:String): TStringArray;
  @desc:
    Return an array of the words in the string, using `sep` as the delimiter string.
    [note]Should be a tad faster then Simbas `Explode(...)`[/note]
}
function AnsiString.Split(Sep:String=' '): TStringArray;
begin
  Result := se.StrExplode(self,sep);
end;


{!DOCREF} {
  @method: function String.Join(TSA:TStringArray): String;
  @desc:
    Return a string which is the concatenation of the strings in the array 'TSA'. 
    The separator between elements is the string providing this method. 
}
function AnsiString.Join(TSA:TStringArray): String;
begin
  Result := Implode(Self, TSA);
end;
//fix for single char evaulation.
function Char.Join(TSA:TStringArray): String;
begin
  Result := Implode(self, TSA);
end;


{!DOCREF} {
  @method: function String.Mul(x:uInt32): String;
  @desc:   Repeats the string `x` times
}
function AnsiString.Mul(x:Int32): String;
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
  i: Int32;
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
function AnsiString.StartsWith(Prefix:String): Boolean;
begin
  Result := Self.Pos(Prefix) = 1;
end;


{!DOCREF} {
  @method: function String.EndsWith(Suffix:String): Boolean;
  @desc:   Returns True if the string ends with `Suffix`.
}
function AnsiString.EndsWith(Suffix:String): Boolean;
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
function AnsiString.Capital(): String;
begin
  Result := Capitalize(Self);
end;


{!DOCREF} {
  @method: function String.Upper(): String;
  @desc:   Return a copy of the string with all the chars converted to uppercase.
}
function AnsiString.Upper(): String;
begin
  Result := Uppercase(Self);
end;


{!DOCREF} {
  @method: function String.Lower(): String;
  @desc:   Return a copy of the string with all the chars converted to lowercase.
}
function AnsiString.Lower(): String;
begin
  Result := Lowercase(Self);
end;



{!DOCREF} {
  @method: function String.IsAlphaNum(): Boolean;
  @desc:   Return true if all characters in the string are alphabetic or numerical and there is at least one character, false otherwise.
}
function AnsiString.IsAlphaNum(): Boolean;
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
function AnsiString.IsAlpha(): Boolean;
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
function AnsiString.IsDigit(): Boolean;
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
function AnsiString.IsFloat(): Boolean;
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
function AnsiString.GetNumbers(): TIntArray;
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
function AnsiString.SplitNum(): TStringArray;
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
        inc(i);
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
