{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap all other methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
function exp_StrStrip(var se:SExt; const Text, Chars: String): String; Cdecl;
begin
  Result := StrStrip(Text,Chars);
end;

function exp_StrStripL(var se:SExt; const Text, Chars: String): String; Cdecl;
begin
  Result := StrStripL(Text,Chars);
end;

function exp_StrStripR(var se:SExt; const Text, Chars: String): String; Cdecl;
begin
  Result := StrStripR(Text,Chars);
end;

function exp_StrPosEx(var se:SExt; const SubStr, Text:String): TIntArray; cdecl;
begin
  Result := StrPosEx(SubStr, Text);
end;

function exp_StrPosL(var se:SExt; const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosL(SubStr, Text);
end;

function exp_StrPosR(var se:SExt; const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosR(SubStr, Text);
end;

function exp_StrReplace(var se:SExt; const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String; Cdecl;
begin
  Result := StrReplace(Text, SubStr, RepStr, Flags);
end;

function exp_StrExplode(var se:SExt; const Text, Sep: String): TStringArray; cdecl;
begin
  Result := StrExplode(Text, Sep);
end;
