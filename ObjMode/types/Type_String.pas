(*=============================================================================|
 String functionality
|=============================================================================*)
{#DOCUMENT} {
  [method]function String.StartsWith(Prefix:String): Boolean;[/method]
  [desc]Returns True if the string starts with Prefix.[/desc]
}{#END}
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


{#DOCUMENT} {
  [method]function String.EndsWith(Suffix:String): Boolean;[/method]
  [desc]Returns True if the string ends with Suffix.[/desc]
}{#END}
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


{#DOCUMENT} {
  [method]function String.Capital(): String;[/method]
  [desc]Return a copy of the string with its first character capitalized and the rest lowercased.[/desc]
}{#END}
function String.Capital(): String;
begin
  Result := Capitalize(Self);
end;


{#DOCUMENT} {
  [method]function String.Upper(): String;[/method]
  [desc]Return a copy of the string with all the chars converted to uppercase.[/desc]
}{#END}
function String.Upper(): String;
begin
  Result := Uppercase(Self);
end;


{#DOCUMENT} {
  [method]function String.Lower(): String;[/method]
  [desc]Return a copy of the string with all the chars converted to lowercase.[/desc]
}{#END}
function String.Lower(): String;
begin
  Result := Lowercase(Self);
end;


{#DOCUMENT} {
  [method]function String.Slice(Start, Stop: Int32): String;[/method]
  [desc]Returns a slice of the string[/desc]
}{#END}
function String.Slice(Start,Stop: Int32): String;
begin
  if Start = 0 then Start := 1
  else if Stop <= 0 then Stop := Length(Self)+Stop;
  Result := Copy(Self, Start, Stop);
end;


{#DOCUMENT} {
  [method]function String.Pos(Sub:String): Int32;[/method]
  [desc]Return the lowest index in the string where substring sub is located. -1 if not found[/desc]
}{#END}
function String.Pos(Sub:String): Int32;
begin
  Result := se.StrPosL(Sub,Self);
end;


{#DOCUMENT} {
  [method]function String.rPos(Sub:String): Int32;[/method]
  [desc]Return the highest index in the string where substring sub is located. -1 if not found[/desc]
}{#END}
function String.rPos(Sub:String): Int32;
begin
  Result := se.StrPosR(Sub,Self);
end;


{#DOCUMENT} {
  [method]function String.PosMulti(Sub:String): TIntArray;[/method]
  [desc]Return all the index in the string where substring sub is located. Empty is not found[/desc]
}{#END}
function String.PosMulti(Sub:String): TIntArray;
begin
  Result := se.StrPosEx(Sub,Self);
end;


{#DOCUMENT} {
  [method]function String.IsAlphaNum(): Boolean;[/method]
  [desc]Return true if all characters in the string are alphabetic or numerical and there is at least one character, false otherwise.[/desc]
}{#END}
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


{#DOCUMENT} {
  [method]function String.IsAlpha(): Boolean;[/method]
  [desc]Return true if all characters in the string are alphabetic and there is at least one character, false otherwise.[/desc]
}{#END}
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


{#DOCUMENT} {
  [method]function String.IsDigit(): Boolean;[/method]
  [desc]Return true if all characters in the string are digits and there is at least one character, false otherwise.[/desc]
}{#END}
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



{#DOCUMENT} {
  [method]function String.GetNumbers(): TIntArray;[/method]
  [desc]Returns all the numbers in the string, does not handle floating point numbers[/desc]
}{#END}
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


{#DOCUMENT} {
  [method]function String.Strip(): String;[/method]
  [desc]Strips all white-spaces at the beginning and ending of a string.[/desc]
}{#END}
function String.Strip(): String;
begin
  Result := Trim(Self);
end;


{#DOCUMENT} {
  [method]function String.Strip(): String;[/method]
  [desc]Strips all white-spaces at the beginning and ending of a string.[/desc]
}{#END}
function String.lStrip(): String;
begin
  Result := TrimLeft(Self);
end;


{#DOCUMENT} {
  [method]function String.rStrip(): String;[/method]
  [desc]Strips all white-spaces at the beginning and ending of a string.[/desc]
}{#END}
function String.rStrip(): String;
begin
  Result := TrimRight(Self);
end;


{#DOCUMENT} {
  [method]function String.Replace(old, new:String; Flags:TReplaceFlags): String;[/method]
  [desc]Return a copy of the string with all occurrences of substring old replaced by new.[/desc]
}{#END}
function String.Replace(old, new:String; Flags:TReplaceFlags): String;
begin
  Result := se.StrReplace(Self, old, new, Flags);
end;


{#DOCUMENT} {
  [method]function String.Split(sep:String): TStringArray;[/method]
  [desc]
    Return an array of the words in the string, using 'sep' as the delimiter string.
    [Note]Lightning fast![/note]
  [/desc]
}{#END}
function String.Split(Sep:String): TStringArray;
begin
  Result := se.StrExplode(self,sep);
end;


{#DOCUMENT} {
  [method]function String.Join(TSA:TStringArray): String;[/method]
  [desc]
  Return a string which is the concatenation of the strings in the array 'TSA'. 
  The separator between elements is the string providing this method. 
  [/desc]
}{#END}
function String.Join(TSA:TStringArray): String;
begin
  Result := Implode(Self, TSA);
end;
//fix for single char evaulation.
function Char.Join(TSA:TStringArray): String;
begin
  Result := Implode(Self, TSA);
end;




