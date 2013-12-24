unit XT_Strings;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses
  SysUtils, XT_Types;

function StrPosEx(const Text, SubStr:String): TIntArray;
function StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
function StrExplode(const Text, Sep: String): TStrArray;


//-----------------------------------------------------------------------
implementation

{*
 Returns all positions of the given pattern/substring.
*}
function StrPosEx(const Text: String; const SubStr:String): TIntArray;
var
  hits,maxhits,h,q,i: Integer;
begin
  MaxHits := Length(SubStr);
  Hits := 1;
  h := 0;
  q := 1;
  SetLength(Result, q);
  for i:=1 to Length(Text) do
  begin
    if Text[i] = SubStr[Hits] then
    begin
      Inc(Hits);
      if (Hits > MaxHits) then
      begin
        if q <= h  then
        begin
          q := q+q;
          SetLength(Result, q);
        end;
        Result[h] := (i - Hits) + 1;
        Inc(h);
        Hits := 1;
      end;
    end else
      Hits := 1;
  end;
  SetLength(Result, h);
end;



{*
 Returns first position of the given pattern/substring.
*}
function StrPos(const Text: String; const SubStr:String): Integer;
var
  hits,maxhits,i: Integer;
begin
  MaxHits := Length(SubStr);
  Hits := 1;
  for i:=1 to Length(Text) do
  begin
    if Text[i] = SubStr[Hits] then
    begin
      Inc(Hits);
      if (Hits > MaxHits) then
        Exit((i - Hits) + 1);
    end else
      Hits := 1;
  end;
  Exit(-1);
end;



{*
 Fast string replace.
*}
function StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;
var
  Hi,HiSub,HiRep,i,j,k: Integer;
  Prev,Curr:Integer;
  Subs: TIntArray;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit;
  case (rfIgnoreCase in flags) of
    True: Subs := StrPosEx(LowerCase(Text), LowerCase(SubStr));
    False:Subs := StrPosEx(Text, SubStr);
  end;

  if Length(Subs) = 0 then
    Exit(Copy(Text, 1,Hi));

  HiRep := Length(RepStr);
  HiSub := Length(SubStr);

  SetLength(Result, Hi + (Length(Subs) * (HiRep-HiSub)));
  k := 1;
  Prev := 1;
  case (rfReplaceAll in Flags) of
  True:
    begin
      for i:=0 to High(Subs) do
      begin
        Curr := Subs[i];
        j := (Curr-Prev) + 1;
        Move(Text[Prev], Result[k], j);
        k := k + j;
        Move(RepStr[1], Result[k], HiRep);
        k := k+HiRep;
        Prev := Curr + HiSub + 1;
      end;
      Move(Text[Prev], Result[k], Hi-Prev+1);
    end;
  False:
    begin
      Curr := Subs[0];
      j := (Curr-Prev) + 1;
      Move(Text[Prev], Result[k], j);
      k := k + j;
      Move(RepStr[1], Result[k], HiRep);
      k := k+HiRep;
      Prev := Curr + HiSub + 1;
      Move(Text[Prev], Result[k], Hi-Prev+1);
      SetLength(Result, k+(Hi-Prev)+1);
    end;
  end;
end;



function StrExplode(const Text, Sep: String): TStrArray;
var
  Subs:TIntArray;
  Hi,i,Curr,Prev,HiSep: Integer;
  S: String;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit;

  Subs := StrPosEx(Text, Sep);
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
    Curr := Subs[i]+1;
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
