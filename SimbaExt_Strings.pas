{*=========================================================================================|
| Randomiz.pas                                                                             |
|=========================================================================================*}
function XT_StrPosEx(const Text, SubStr:String): TIntegerArray;
begin
  exp_StrPosEx(Text, SubStr, Result);
end;

function XT_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
begin
  Result := exp_StrReplace(Text, SubStr, RepStr, Flags);
end;

function XT_StrExplode(const Text, Sep: String): TStringArray;
begin
  exp_StrExplode(Text, Sep, Result);
end;