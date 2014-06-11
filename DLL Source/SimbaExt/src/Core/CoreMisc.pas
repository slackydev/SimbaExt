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
procedure Exch(var A,B:uInt8); Inline; overload;
procedure Exch(var A,B:uInt16); Inline; overload;
procedure Exch(var A,B:uInt32); Inline; overload;
procedure Exch(var A,B:uInt64); Inline; overload;

procedure Exch(var A,B:Int8); Inline; overload;
procedure Exch(var A,B:Int16); Inline; overload;
procedure Exch(var A,B:Int32); Inline; overload;
procedure Exch(var A,B:Int64); Inline; overload;

procedure Exch(var A,B:Extended); Inline; overload;
procedure Exch(var A,B:Double); Inline; overload;
procedure Exch(var A,B:Single); Inline; overload;

procedure Exch(var A,B:TPoint); Inline; overload;
procedure Exch(var A,B:TBox); Inline; overload;


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
  Excahnging values between A and B 
*)
procedure Exch(var A,B:uInt8); Inline; overload;
var t: uInt8; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt16); Inline; overload;
var t: uInt16; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt32); Inline; overload;
var t: uInt32; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt64); Inline; overload;
var t: uInt64; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:Int8); Inline; overload;
var t: Int8; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int16); Inline; overload;
var t: Int16; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int32); Inline; overload;
var t: Int32; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int64); Inline; overload;
var t: Int64; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:Extended); Inline; overload;
var t: Extended; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Double); Inline; overload;
var t: Double; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Single); Inline; overload;
var t: Single; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:TPoint); Inline; overload;
var t: TPoint; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:TBox); Inline; overload;
var t: TBox; begin t := A;  A := B;  B := t; end;



(* 
  Move functionallity (safe) and routines related to it
  !! Deprecated !!
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
