unit CoreMisc;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

interface

uses
  CoreTypes;

  
//Swapping / exchanging
procedure ExchI(var A,B:Integer); Inline;
procedure ExchE(var A,B:Extended); Inline;
procedure ExchS(var A,B:Single); Inline;
procedure ExchBt(var A,B:Byte); Inline;
procedure ExchPt(var A,B:TPoint); Inline;

//Safe move functionallity
procedure Move(const InArr:AnsiString; var DestArr:AnsiString; source, dest, size:Integer); overload;
procedure Move(const InArr:TIntArray; var DestArr:TIntArray; source, dest, size:Integer); overload;
procedure Move(const InArr:TExtArray; var DestArr:TExtArray; source, dest, size:Integer); overload;
procedure Move(const InArr:TPointArray; var DestArr:TPointArray; source, dest, size:Integer); overload;


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




(* 
  Move functionallity (safe) and routines related to it
*)
function ChopSize(D,S,L: Integer): Integer; Inline;
begin
  Result := S;
  if ((D + S) >= L) then
     Result := (L - D) + 1;
end;

procedure Move(const InArr:AnsiString; var DestArr:AnsiString; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  size := ChopSize(Dest,Source,Length(DestArr));
  Move(InArr[source], DestArr[dest], size);
end;


procedure Move(const InArr:TIntArray; var DestArr:TIntArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  size := ChopSize(Dest,Source,Length(DestArr)) * SizeOf(Integer);
  Move(InArr[source], DestArr[dest], size);
end;


procedure Move(const InArr:TExtArray; var DestArr:TExtArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  size := ChopSize(Dest,Source,Length(DestArr)) * SizeOf(Extended);
  Move(InArr[source], DestArr[dest], size);
end;


procedure Move(const InArr:TPointArray; var DestArr:TPointArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  size := ChopSize(Dest,Source,Length(DestArr)) * SizeOf(TPoint);
  Move(InArr[source], DestArr[dest], size);
end;



end.
