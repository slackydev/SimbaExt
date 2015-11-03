unit StringTools;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
interface

uses
  SysUtils, CoreTypes;

function StrStrip(const Text, Chars:String): String; 
function StrStripL(const Text, Chars:String): String;
function StrStripR(const Text, Chars:String): String; 

function StrPosEx(const SubStr, Text:String): TIntArray; 
function StrPosL(const SubStr, Text: String): Integer; 
function StrPosR(const SubStr, Text: String): Integer;

function StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
function StrExplode(const Text, Sep: String): TStringArray;

//-----------------------------------------------------------------------
implementation


{*
  Return a copy of the string with leading and trailing characters removed.
*}
function StrStrip(const Text, Chars:String): String;
var Lo,Hi: UInt32;
begin
  Lo:=1;
  Hi:=Length(Text);
  while (Lo <= Hi) and (Pos(Text[Lo],Chars) <> 0) do Inc(Lo);
  while (Hi >= 1)  and (Pos(Text[Hi],Chars) <> 0) do Dec(Hi);
  Result := Copy(Text, Lo, Hi-Lo+1);
end;


{*
  Return a copy of the string with leading characters removed.
*}
function StrStripL(const Text, Chars:String): String;
var Lo,Hi: UInt32;
begin
  Lo:=1;
  Hi:=Length(Text);
  while (Lo <= Hi) and (Pos(Text[Lo],Chars) <> 0) do Inc(Lo);
  Result := Copy(Text, Lo, Hi-Lo+1);
end;  


{*
  Return a copy of the string with trailing characters removed.
*}
function StrStripR(const Text, Chars:String): String;
var Lo,Hi: UInt32;
begin
  Lo:=1;
  Hi:=Length(Text);
  while (Hi >= 1) and (Pos(Text[Hi],Chars) <> 0) do Dec(Hi);
  Result := Copy(Text, Lo, Hi-Lo+1);
end;  



{*
  Returns all positions of the given pattern/substring.
*}
function StrPosEx(const SubStr, Text:String): TIntArray;
var
  HitPos,LenSub,h,q,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit;
  HitPos := 1;
  h := 0;
  q := 1;
  SetLength(Result, q);
  for i:=1 to Length(Text) do
    if Text[i] <> SubStr[HitPos] then
      HitPos := 1
    else begin
      if (HitPos <> LenSub) then
        Inc(HitPos)
      else begin
        if q <= h then
        begin
          q := q+q;
          SetLength(Result, q);
        end;
        Result[h] := (i - HitPos) + 1;
        Inc(h);
        HitPos := 1;
      end;  
    end;
  SetLength(Result, h);
end;



{*
 Returns first position of the given pattern/substring from left.
*}
function StrPosL(const SubStr, Text: String): Integer;
var
  HitPos,LenSub,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit(0);
  HitPos := 1;
  for i:=1 to Length(Text) do
  begin
    if Text[i] = SubStr[HitPos] then
    begin
      if (HitPos = LenSub) then
        Exit(i - LenSub + 1);
      Inc(HitPos);
    end else
      HitPos := 1;
  end;
  Exit(0);
end;


{*
 Returns first position of the given pattern/substring from right.
*}
function StrPosR(const SubStr, Text: String): Integer;
var
  HitPos,LenSub,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit(0);
  HitPos := LenSub;
  for i:=Length(Text) downto 1 do
  begin
    if Text[i] = SubStr[HitPos] then
    begin
      if (HitPos = 1) then 
        Exit(i);
      Dec(HitPos);
    end else
      HitPos := LenSub;
  end;
  Exit(0);
end;



{*
 Return a copy of string `Text` with all occurrences of `substr` replaced by `RepStr`.
*}
function StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
var
  Hi,HiSub,HiRep,i,j,k: Integer;
  Prev,Curr:Integer;
  Subs: TIntArray;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit;

  HiRep := Length(RepStr);
  HiSub := Length(SubStr);

  k := 1;
  Prev := 1;
  case (rfReplaceAll in Flags) and True of
  True:
    begin
      //If substring and replacement string is only 1 char, then it's simpler:
      if (HiRep = 1) and (HiSub = 1) then begin
        Result := Copy(Text,1,hi);
        for i:=1 to Length(text) do
          if Text[i] = SubStr[1] then
            Result[i] := Text[i];
        Exit();
      end;

      case (rfIgnoreCase in flags) of
        True: Subs := StrPosEx(LowerCase(SubStr), LowerCase(Text));
        False:Subs := StrPosEx(SubStr, Text);
      end;
      SetLength(Result, Hi + (Length(Subs) * (HiRep-HiSub)));
      if Length(Subs) = 0 then Exit(Copy(Text, 1,Hi));
      for i:=0 to High(Subs) do
      begin
        Curr := Subs[i];
        j := (Curr-Prev);
        Move(Text[Prev], Result[k], j);
        k := k + j;
        Move(RepStr[1], Result[k], HiRep);
        k := k+HiRep;
        Prev := Curr + HiSub;
      end;
      Move(Text[Prev], Result[k], Hi-Prev+1);
    end;
    
  False:
    begin
      SetLength(Result, Hi + (HiRep-HiSub));
      case (rfIgnoreCase in flags) of
        True: Curr := StrPosL(LowerCase(SubStr), LowerCase(Text));
        False:Curr := StrPosL(SubStr, Text);
      end;
      if Curr = 0 then Exit(Copy(Text, 1,Hi));

      Move(Text[1], Result[1], Curr-1);
      Move(RepStr[1], Result[Curr], HiRep);
      Move(Text[(Curr+HiSub)], Result[Curr+HiRep], Hi-(Curr+HiSub)+1);
      SetLength(Result, Hi+(HiRep-HiSub));
    end;
  end;
end;



{*
 StrExplode lets you take a string and blow it up into smaller pieces, at each
 occurance of the given seperator `Sep`. 
*}
function StrExplode(const Text, Sep: String): TStringArray;
var
  Subs:TIntArray;
  Hi,i,Curr,Prev,HiSep: UInt32;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit;

  Subs := StrPosEx(Sep, Text);
  if Length(Subs) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Copy(Text, 1,Hi);
    Exit;
  end;
  HiSep := Length(Sep);
  Prev := 1;
  SetLength(Result, Length(Subs));
  for i:=0 to High(Subs) do
  begin
    Curr := Subs[i];
    Result[i] := Copy(Text, Prev, (Curr-Prev));
    Prev := Curr+HiSep;
  end;
  if Prev <= Hi then
  begin
    SetLength(Result, Length(Subs)+1);
    Result[Length(Subs)] := Copy(Text, Prev, Hi);
  end;
end;



end.


