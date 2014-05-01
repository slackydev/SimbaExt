{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function SimbaExt.StrPosEx(const SubStr, Text:String): TIntegerArray;
begin
  exp_StrPosEx(SubStr, Text, Result);
end;

function SimbaExt.StrPosL(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosL(SubStr, Text);
end;

function SimbaExt.StrPosR(const SubStr, Text: String): Int32;
begin
  Result := exp_StrPosR(SubStr, Text);
end;

function SimbaExt.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
begin
  Result := exp_StrReplace(Text, SubStr, RepStr, Flags);
end;

function SimbaExt.StrExplode(const Text, Sep: String): TStringArray;
begin
  exp_StrExplode(Text, Sep, Result);
end;