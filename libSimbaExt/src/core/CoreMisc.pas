unit CoreMisc;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  CoreTypes;

type
  TDataType = record
    vSize:Int32;
    vType:(dtSigned, dtUnsigned, dtFloat);

    function Epsilon(): Extended; inline;
    function Compare(x,y: Pointer): Int8; inline;
    procedure Swap(x,y: Pointer); inline;
    function VarCast(x: Pointer): Int64; inline;
    function VarCastF(x: Pointer): Extended; inline;
  end;


// Post-Increse and Post-Decrease but with a result (Similar to i++, and i-- in C)
function Asc(var i: Int32): Int32; Inline; overload; //i++
function Asc(var i: uInt32): uInt32; Inline;  overload; //i++
function Asc(var i: Int64): Int64; Inline;  overload; //i++
function Asc(var i: uInt64): uInt64; Inline;  overload; //i++

function Desc(var i: Int32): Int32; Inline; overload; //i--
function Desc(var i: uInt32): uInt32; Inline; overload; //i--
function Desc(var i: Int64): Int64; Inline; overload; //i--
function Desc(var i: uInt64): uInt64; Inline; overload; //i--


// Swapping / exchanging
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


// Excahnging values: only if condition is true
procedure ExchIf(cond:Boolean; var a,b:UInt8); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:UInt16); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:UInt32); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:UInt64); Inline; overload;

procedure ExchIf(cond:Boolean; var a,b:Int8); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:Int16); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:Int32); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:Int64); Inline; overload;

procedure ExchIf(cond:Boolean; var a,b:Single); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:Double); Inline; overload;
procedure ExchIf(cond:Boolean; var a,b:Extended); Inline; overload;


// Return the median of the 3 values
function MedianOfThree(a,b,c:uInt8): uInt8; Inline; overload;
function MedianOfThree(a,b,c:Int8): Int8; Inline; overload;
function MedianOfThree(a,b,c:Int32): Int32; Inline; overload;
function MedianOfThree(a,b,c:Single): Single; Inline; overload;
function MedianOfThree(a,b,c:Double): Double; Inline; overload;
function MedianOfThree(a,b,c:Extended): Extended; Inline; overload;


// Return the median of the 5 values
function MedianOfFive(a,b,c,d,e:uInt8): uInt8; Inline; overload;
function MedianOfFive(a,b,c,d,e:Int8): Int8; Inline; overload;
function MedianOfFive(a,b,c,d,e:Int32): Int32; Inline; overload;
function MedianOfFive(a,b,c,d,e:Single): Single; Inline; overload;
function MedianOfFive(a,b,c,d,e:Double): Double; Inline; overload;
function MedianOfFive(a,b,c,d,e:Extended): Extended; Inline; overload;


//-----------------------------------------------------------------------
implementation
uses Math;


//-------------------------------------------------------------------------- 
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
 

//--------------------------------------------------------------------------
(* 
  Excahnging values between A and B 
*)
procedure Exch(var A,B:uInt8); Inline; 
var t: uInt8; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt16); Inline; 
var t: uInt16; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt32); Inline; 
var t: uInt32; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:uInt64); Inline; 
var t: uInt64; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:Int8); Inline; 
var t: Int8; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int16); Inline; 
var t: Int16; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int32); Inline; 
var t: Int32; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Int64); Inline; 
var t: Int64; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:Extended); Inline; 
var t: Extended; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Double); Inline; 
var t: Double; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:Single); Inline; 
var t: Single; begin t := A;  A := B;  B := t; end;

procedure Exch(var A,B:TPoint); Inline; 
var t: TPoint; begin t := A;  A := B;  B := t; end;
procedure Exch(var A,B:TBox); Inline; 
var t: TBox; begin t := A;  A := B;  B := t; end;


//--------------------------------------------------------------------------
(* 
  Excahnging values between A and B, but only if condition `cond` is True.
*)
procedure ExchIf(cond:Boolean; var a,b:UInt8); Inline; 
var t:UInt8; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:UInt16); Inline; 
var t:UInt16; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:UInt32); Inline; 
var t:UInt32; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:UInt64); Inline; 
var t:UInt64; begin if cond then begin t := a; a := b; b := t; end; end;

procedure ExchIf(cond:Boolean; var a,b:Int8); Inline; 
var t:Int8; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:Int16); Inline; 
var t:Int16; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:Int32); Inline; 
var t:Int32; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:Int64); Inline; 
var t:Int64; begin if cond then begin t := a; a := b; b := t; end; end;

procedure ExchIf(cond:Boolean; var a,b:Single); Inline; 
var t:Single; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:Double); Inline; 
var t:Double; begin if cond then begin t := a; a := b; b := t; end; end;
procedure ExchIf(cond:Boolean; var a,b:Extended); Inline; 
var t:Extended; begin if cond then begin t := a; a := b; b := t; end; end;


//--------------------------------------------------------------------------
(* 
  Returns the median of the 3 given values
*)
function MedianOfThree(a,b,c:uInt8): uInt8;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end; 

function MedianOfThree(a,b,c:Int8): Int8;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end;

function MedianOfThree(a,b,c:Int32): Int32;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end;

function MedianOfThree(a,b,c:Single): Single;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end;

function MedianOfThree(a,b,c:Double): Double;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end;

function MedianOfThree(a,b,c:Extended): Extended;
begin
  if(a < b) then
    if (a >= c) then Exit(a)
    else if (b < c) then Exit(b);
  if (a < c) then Exit(a)
  else if (b >= c) then Exit(b);
  Exit(c);
end;


//--------------------------------------------------------------------------
(* 
  Returns the median of the 5 given values
*)
function MedianOfFive(a,b,c,d,e:uInt8): uInt8;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Int8): Int8;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Int32): Int32;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Single): Single;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Double): Double;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;

function MedianOfFive(a,b,c,d,e:Extended): Extended;
begin
  ExchIf(a<b, a,b);
  ExchIf(c<d, c,d);
  if (c < a) then begin Exch(b,d); c := a; end;
  a := e;
  ExchIf(a<b, a,b);
  if (a < c) then begin Exch(b,d); a := c; end;
  Result := Min(d, a);
end;







//--------------------------------------------------------------------------

function TDataType.Compare(x,y: Pointer): Int8;
begin
  case ((Int32(vType) shl 5) xor vSize) of
    68: Exit( Sign(PFloat32(x)^ - PFloat32(y)^) );
    72: Exit( Sign(PFloat64(x)^ - PFloat64(y)^) );
    74: Exit( Sign(PFloat80(x)^ - PFloat80(y)^) );
    33: Exit( Sign(PUInt8(x)^ - PUInt8(y)^) );
    34: Exit( Sign(PUInt16(x)^ - PUInt16(y)^) );
    36: Exit( Sign(PUInt32(x)^ - PUInt32(y)^) );
    40: Exit( Sign(Int64(PUInt64(x)^ - PUInt64(y)^)) );
    1 : Exit( Sign(PInt8(x)^ - PInt8(y)^) );
    2 : Exit( Sign(PInt16(x)^ - PInt16(y)^) );
    4 : Exit( Sign(PInt32(x)^ - PInt32(y)^) );
    8 : Exit( Sign(PInt64(x)^ - PInt64(y)^) );
  end;
end;


procedure TDataType.Swap(x,y: Pointer);
begin
  case ((Int32(vType) shl 5) xor vSize) of
    68: Exch( PFloat32(x)^, PFloat32(y)^ );
    72: Exch( PFloat64(x)^, PFloat64(y)^ );
    74: Exch( PFloat80(x)^, PFloat80(y)^ );
    33: Exch( PUInt8(x)^, PUInt8(y)^ );
    34: Exch( PUInt16(x)^, PUInt16(y)^ );
    36: Exch( PUInt32(x)^, PUInt32(y)^ );
    40: Exch( PUInt64(x)^, PUInt64(y)^ );
    1 : Exch( PInt8(x)^, PInt8(y)^ );
    2 : Exch( PInt16(x)^, PInt16(y)^ );
    4 : Exch( PInt32(x)^, PInt32(y)^ );
    8 : Exch( PInt64(x)^, PInt64(y)^ );
  end;
end;


function TDataType.VarCast(x: Pointer): Int64;
begin
  case ((Int32(vType) shl 5) xor vSize) of
    68: Result := Trunc(PFloat32(x)^);
    72: Result := Trunc(PFloat64(x)^);
    74: Result := Trunc(PFloat80(x)^);
    33: Result := PUInt8(x)^;
    34: Result := PUInt16(x)^;
    36: Result := PUInt32(x)^;
    40: Result := PUInt64(x)^;
    1 : Result := PInt8(x)^;
    2 : Result := PInt16(x)^;
    4 : Result := PInt32(x)^;
    8 : Result := PInt64(x)^;
  end;
end;


function TDataType.VarCastF(x: Pointer): Extended;
begin
  case ((Int32(vType) shl 5) xor vSize) of
    68: Result := PFloat32(x)^;
    72: Result := PFloat64(x)^;
    74: Result := PFloat80(x)^;
    33: Result := PUInt8(x)^;
    34: Result := PUInt16(x)^;
    36: Result := PUInt32(x)^;
    40: Result := PUInt64(x)^;
    1 : Result := PInt8(x)^;
    2 : Result := PInt16(x)^;
    4 : Result := PInt32(x)^;
    8 : Result := PInt64(x)^;
  end;
end;



function TDataType.Epsilon(): Extended;
begin
  case ((Int32(vType) shl 5) xor vSize) of
    68: Result := 1.1921e-07;
    72: Result := 2.220446e-16;
    74: Result := 1.08e-19;
  else
    Result := 0;
  end;
end;



end.
