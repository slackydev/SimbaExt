unit CoreMisc;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

interface

uses
  CoreTypes;

//Increse, and Decrease but with a result (Similar to i++, and i-- in C)
function Inc(var i: Integer): Integer; Inline; overload; //i++
function Inc(var i: uInt32): uInt32; Inline;  overload; //i++
function Inc(var i: Int64): Int64; Inline;  overload; //i++

function Dec(var i: Integer): Integer; Inline; overload; //i--
function Dec(var i: uInt32): uInt32; Inline; overload; //i--
function Dec(var i: Int64): Int64; Inline; overload; //i--
  
  
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

 
(* 
  Increase of the value (value+1), returning the current value
*)
function Inc(var i: Integer): Integer; Inline; //i++
begin Result := i;  i := i+1; end;

function Inc(var i: uInt32): uInt32; Inline; //i++
begin Result := i;  i := i+1; end;

function Inc(var i: Int64): Int64; Inline; //i++
begin Result := i;  i := i+1; end;


(* 
  Decrease the value (value+1), returning the current value
*)
function Dec(var i: Integer): Integer; Inline; //i--
begin Result := i;  i := i-1; end;
 
function Dec(var i: uInt32): uInt32; Inline; //i--
begin Result := i;  i := i-1; end;

function Dec(var i: Int64): Int64; Inline; //i--
begin Result := i;  i := i-1; end;
 
 

(* 
  Swapping values
*)
procedure ExchI(var A,B:Integer); Inline;
var t: Integer;
begin t := A;  A := B;  B := t; end;

procedure ExchE(var A,B:Extended); Inline;
var t: Extended;
begin t := A;  A := B;  B := t; end;

procedure ExchS(var A,B:Single); Inline;
var t:Single;
begin t := A;  A := B;  B := t; end;

procedure ExchBt(var A,B:Byte); Inline;
var t: Byte;
begin t := A;  A := B;  B := t; end;

procedure ExchPt(var A,B:TPoint); Inline;
var t: TPoint;
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
  Move(InArr[source], DestArr[dest], size);
end;


procedure Move(const InArr:TIntArray; var DestArr:TIntArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  Move(InArr[source], DestArr[dest], size* SizeOf(Integer));
end;


procedure Move(const InArr:TExtArray; var DestArr:TExtArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  Move(InArr[source], DestArr[dest], size * SizeOf(Extended));
end;


procedure Move(const InArr:TPointArray; var DestArr:TPointArray; source, dest, size:Integer); overload;
var L:Integer;
begin
  L := Length(InArr);
  if (L = 0) or (source >= L) then Exit;
  Move(InArr[source], DestArr[dest], size * SizeOf(TPoint));
end;



end.
