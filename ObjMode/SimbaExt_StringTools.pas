{!DOCTOPIC}{ 
  StringTools
}

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
  @desc: Same as 'String.Pos(..)', returns the first position where the substring was found.
}
function SimbaExt.StrPosL(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosL(SubStr, Text);
end;


{!DOCREF} {
  @method: function se.StrPosL(const SubStr, Text: String): Int32;
  @desc: Same as 'String.rPos(..)', returns the first position from right where the substring was found.
}
function SimbaExt.StrPosR(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosR(SubStr, Text);
end;


{!DOCREF} {
  @method: function se.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
  @desc: 
    This function returns a string or an array with all occurrences of 'SubStr' in 'Text' replaced with the given replace value 'RepStr'.
    This version is a lot faster then the one in Simba, at least when we are working with large amount of text.
}
function SimbaExt.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
begin
  Result := exp_StrReplace(Text, SubStr, RepStr, Flags);
end;


{!DOCREF} {
  @method: function se.StrExplode(const Text, Sep: String): TStringArray;
  @desc: Returns an array of strings, each of which is a substring of 'Text' formed by splitting it on boundaries formed by the string separator 'sep'.
}
function SimbaExt.StrExplode(const Text, Sep: String): TStringArray;
begin
  exp_StrExplode(Text, Sep, Result);
end;