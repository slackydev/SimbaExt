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
function Asc(var i: Int32): Int32; Inline; overload; //i++
function Asc(var i: uInt32): uInt32; Inline;  overload; //i++
function Asc(var i: Int64): Int64; Inline;  overload; //i++
function Asc(var i: uInt64): uInt64; Inline;  overload; //i++

function Desc(var i: Int32): Int32; Inline; overload; //i--
function Desc(var i: uInt32): uInt32; Inline; overload; //i--
function Desc(var i: Int64): Int64; Inline; overload; //i--
function Desc(var i: uInt64): uInt64; Inline; overload; //i--

  
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


// Excahnging values: only if a > b
procedure ExchMin(var a,b:UInt8); Inline; Overload;
procedure ExchMin(var a,b:UInt16); Inline; Overload;
procedure ExchMin(var a,b:UInt32); Inline; Overload;
procedure ExchMin(var a,b:UInt64); Inline; Overload;

procedure ExchMin(var a,b:Int8); Inline; Overload;
procedure ExchMin(var a,b:Int16); Inline; Overload;
procedure ExchMin(var a,b:Int32); Inline; Overload;
procedure ExchMin(var a,b:Int64); Inline; Overload;

procedure ExchMin(var a,b:Single); Inline; Overload;
procedure ExchMin(var a,b:Double); Inline; Overload;
procedure ExchMin(var a,b:Extended); Inline; Overload;


//Safe move functionallity
//procedure Move(const InArr:AnsiString; var DestArr:AnsiString; source, dest, size:Integer); overload;
//procedure Move(const InArr:TIntArray; var DestArr:TIntArray; source, dest, size:Integer); overload;
//procedure Move(const InArr:TExtArray; var DestArr:TExtArray; source, dest, size:Integer); overload;
//procedure Move(const InArr:TPointArray; var DestArr:TPointArray; source, dest, size:Integer); overload;



//-----------------------------------------------------------------------
implementation

 
(* 
  Increase of the value (value+1), returning the current value (i++)
*)
function Asc(var i: Int32): Int32; Inline;
begin Result:=i;  i+=1; end;

function Asc(var i: uInt32): uInt32; Inline;
begin Result:=i;  i+=1; end;

function Asc(var i: Int64): Int64; Inline;
begin Result:=i; i+=1; end;

function Asc(var i: uInt64): uInt64; Inline;
begin Result:=i;  i+=1; end;

(* 
  Decrease the value (value+1), returning the current value (i--)
*)
function Desc(var i: Int32): Int32; Inline;
begin Result:=i;  i-=1; end;
 
function Desc(var i: uInt32): uInt32; Inline;
begin Result:=i;  i-=1; end;

function Desc(var i: Int64): Int64; Inline;
begin Result:=i;  i-=1; end;
 
function Desc(var i: uInt64): uInt64; Inline;
begin Result:=i;  i-=1; end;
 

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
  Excahnging values between A and B, but only if a > b.
*)
procedure ExchMin(var a,b:UInt8); Inline; Overload;
var t:UInt8; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:UInt16); Inline; Overload;
var t:UInt16; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:UInt32); Inline; Overload;
var t:UInt32; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:UInt64); Inline; Overload;
var t:UInt64; begin if (a > b) then begin t := a; a := b; b := t; end; end;

procedure ExchMin(var a,b:Int8); Inline; Overload;
var t:Int8; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:Int16); Inline; Overload;
var t:Int16; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:Int32); Inline; Overload;
var t:Int32; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:Int64); Inline; Overload;
var t:Int64; begin if (a > b) then begin t := a; a := b; b := t; end; end;

procedure ExchMin(var a,b:Single); Inline; Overload;
var t:Single; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:Double); Inline; Overload;
var t:Double; begin if (a > b) then begin t := a; a := b; b := t; end; end;
procedure ExchMin(var a,b:Extended); Inline; Overload;
var t:Extended; begin if (a > b) then begin t := a; a := b; b := t; end; end;


(* 
  Move functionallity (safe) and routines related to it
  !! Deprecated !!
*)
(*
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
*)


end.
