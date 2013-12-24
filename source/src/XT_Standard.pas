unit XT_Standard;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

interface

uses
  XT_Types;

procedure ExchI(var A,B:Integer); Inline;
procedure ExchE(var A,B:Extended); Inline;
procedure ExchS(var A,B:Single); Inline;
procedure ExchBt(var A,B:Byte); Inline;
procedure ExchPt(var A,B:TPoint); Inline;


//-----------------------------------------------------------------------
implementation


procedure ExchI(var A,B:Integer); Inline;
var t:Integer;
begin t := A;  A := B;  B := t; end;

procedure ExchE(var A,B:Extended); Inline;
var t:Extended;
begin t := A;  A := B;  B := t; end;

procedure ExchS(var A,B:Single); Inline;
var t:Single;
begin t := A;  A := B;  B := t; end;

procedure ExchBt(var A,B:Byte); Inline;
var t:Byte;
begin t := A;  A := B;  B := t; end;

procedure ExchPt(var A,B:TPoint); Inline;
var t:TPoint;
begin t := A;  A := B;  B := t; end;


end.
