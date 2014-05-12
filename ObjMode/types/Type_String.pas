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
  @desc:   Returns the length of the String. Same as c'Length(Str)'
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
var
  P: PChar;
Begin
  P := @Self[1];
  while (P^ <> #0) do Inc(P);
  Result := Integer(P) - Integer(@Self[1]);
end;


{!DOCREF} {
  @method: function String.Slice(Start, Stop: Int32): String;
  @desc:   Returns a slice of the string
}
function String.Slice(Start,Stop: Int32): String;
begin
  if Start = 0 then Start := 1
  else if Stop <= 0 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop-Start);
end;

{!DOCREF} {
  @method: function String.StartsWith(Prefix:String): Boolean;
  @desc:   Returns True if the string starts with c'Prefix'.
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
  @desc:   Returns True if the string ends with c'Suffix'.
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
  @desc:   Return a copy of the string with its first character capitalized and the rest lowercased.
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
  @method: function String.Pos(Sub:String): Int32;
  @desc:   Return the lowest index in the string where substring c'Sub' is located. -1 if not found
}
function String.Pos(Sub:String): Int32;
begin
  Result := se.StrPosL(Sub,Self);
end;


{!DOCREF} {
  @method: function String.rPos(Sub:String): Int32;
  @desc:   Return the highest index in the string where substring c'Sub' is located. -1 if not found
}
function String.rPos(Sub:String): Int32;
begin
  Result := se.StrPosR(Sub,Self);
end;


{!DOCREF} {
  @method: function String.PosMulti(Sub:String): TIntArray;
  @desc:   Return all the index in the string where substring c'Sub' is located. Empty is not found
}
function String.PosMulti(Sub:String): TIntArray;
begin
  Result := se.StrPosEx(Sub,Self);
end;


{!DOCREF} {
  @method: function String.IsAlphaNum(): Boolean;
  @desc:   Return true if all characters in the string are alphabetic or numerical and there is at least one character, false otherwise.
}
function String.IsAlphaNum(): Boolean;
var ptr: PChar;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  while ptr^ <> #0 do
  begin
    if not (ptr^ in ['A'..'Z', 'a'..'z','0'..'9']) then 
      Exit(False);
    Inc(ptr);
  end;
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
var ptr: PChar;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  while ptr^ <> #0 do
  begin
    if not (ptr^ in ['A'..'Z', 'a'..'z']) then 
      Exit(False);
    Inc(ptr);
  end;
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
var ptr: PChar;
begin
  if Length(Self) = 0 then Exit(False);
  ptr := PChar(Self);
  while ptr^ <> #0 do
  begin
    if not (ptr^ in ['0'..'9']) then 
      Exit(False);
    Inc(ptr);
  end;
  Result := True;
end;

function Char.IsDigit(): Boolean;
begin
  Result := (Self in ['0'..'9']);
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
  @method: function String.Replace(old, new:String; Flags:TReplaceFlags): String;
  @desc:   Return a copy of the string with all occurrences of substring old replaced by new.
}
function String.Replace(old, new:String; Flags:TReplaceFlags): String;
begin
  Result := se.StrReplace(Self, old, new, Flags);
end;


{!DOCREF} {
  @method: function String.Split(sep:String): TStringArray;
  @desc: 
    Return an array of the words in the string, using 'sep' as the delimiter string.
    [note]Should be notable faster then Simbas c'Explode(...)' whenever the string is more then a few sentences[/note]
}
function String.Split(Sep:String): TStringArray;
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




