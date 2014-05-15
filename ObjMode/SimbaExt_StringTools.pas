{!DOCTOPIC}{ 
  StringTools
}


{!DOCREF} {
  @method: function se.StrStrip(const Text:String; Chars:String=' '): String;
  @desc: 
    Return a copy of the string with leading and trailing characters removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the both ends of Text.
}
function SimbaExt.StrStrip(const Text:String; Chars:String=' '): String;
begin
  Result := exp_StrStrip(Text, Chars);
end;


{!DOCREF} {
  @method: function se.StrStripL(const Text:String; Chars:String=' '): String;
  @desc: 
    Return a copy of the string with leading removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the beginning of Text.
}
function SimbaExt.StrStripL(const Text:String; Chars:String=' '): String;
begin
  Result := exp_StrStripL(Text, Chars);
end;


{!DOCREF} {
  @method: function se.StrStripR(const Text:String; Chars:String=' '): String;
  @desc: 
    Return a copy of the string with trailing removed. If chars is omitted, whitespace characters are removed. 
    If chars is given, the characters in the string will be stripped from the end of Text.
}
function SimbaExt.StrStripR(const Text:String; Chars:String=' '): String;
begin
  Result := exp_StrStripR(Text, Chars);
end; 


{!DOCREF} {
  @method: function se.StrPosEx(const SubStr, Text:String): TIntegerArray;
  @desc: Same as 'String.PosMulti(..)', returns all the positions where the substring was found.
}
function SimbaExt.StrPosEx(const SubStr, Text:String): TIntegerArray;
begin
  exp_StrPosEx(SubStr, Text, Result);
end;


{!DOCREF} {
  @method: function se.StrPosL(const SubStr, Text: String): Int32;
  @desc: Same as 'String.Pos(..)', returns the first position where the substring was found, returns 0 if not.
}
function SimbaExt.StrPosL(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosL(SubStr, Text);
end;


{!DOCREF} {
  @method: function se.StrPosL(const SubStr, Text: String): Int32;
  @desc: Same as 'String.rPos(..)', returns the first position from right where the substring was found, returns 0 if not.
}
function SimbaExt.StrPosR(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosR(SubStr, Text);
end;


{!DOCREF} {
  @method: function se.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
  @desc:
    This function returns a string or an array with all occurrences of c'SubStr' in c'Text' replaced with the given replace value 'RepStr'.
    This version is a lot faster then Simbas c'Replace', at least when we are working with more then just a sentance or three.
}
function SimbaExt.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
begin
  Result := exp_StrReplace(Text, SubStr, RepStr, Flags);
end;


{!DOCREF} {
  @method: function se.StrExplode(const Text, Sep: String): TStringArray;
  @desc: 
    Returns an array of strings, each of which is a substring of c'Text' formed by splitting it on boundaries formed by the string separator c'sep'.
    Should be a bit faster then simbas c'Explode(...)'.
}
function SimbaExt.StrExplode(const Text, Sep: String): TStringArray;
begin
  exp_StrExplode(Text, Sep, Result);
end;
