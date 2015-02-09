{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports string methods (StringTools.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@exp_StrStrip,	'function SimbaExt.StrStrip(const Text, Chars: String): String;');
AddMethod(@exp_StrStripL,	'function SimbaExt.StrStripL(const Text, Chars: String): String;');
AddMethod(@exp_StrStripR,	'function SimbaExt.StrStripR(const Text, Chars: String): String;');
AddMethod(@exp_StrPosEx,	'function SimbaExt.StrPosMulti(const SubStr, Text:String): TIntArray;');
AddMethod(@exp_StrPosL,		'function SimbaExt.StrPos(const SubStr, Text: String): Integer;');
AddMethod(@exp_StrPosR,		'function SimbaExt.StrPosR(const SubStr, Text: String): Integer;');
AddMethod(@exp_StrReplace,	'function SimbaExt.StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;');
AddMethod(@exp_StrExplode,	'function SimbaExt.StrExplode(const Text, Sep: String): TStringArray;');