{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function SimbaExt.StrPosEx(const Text, SubStr:String): TIntegerArray;
begin
  exp_StrPosEx(Text, SubStr, Result);
end;

function SimbaExt.StrPosL(const Text, SubStr: String): Integer;
begin
  Result := exp_StrPosL(Text, SubStr);
end;

function SimbaExt.StrPosR(const Text, SubStr: String): Integer;
begin
  Result := exp_StrPosR(Text, SubStr);
end;

function SimbaExt.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
begin
  Result := exp_StrReplace(Text, SubStr, RepStr, Flags);
end;

function SimbaExt.StrExplode(const Text, Sep: String): TStringArray;
begin
  exp_StrExplode(Text, Sep, Result);
end;