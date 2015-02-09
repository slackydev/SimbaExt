Unit Colordist;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
     
interface

(* color constants *)
const
  DELTAE_MAX_DIFF: Single  = 141.5;

  LAB_MAX_SQR_DIFF: Single = 85029.30362;					{ MAX_L^2 + MAX_A^2 + MAX_B^2 }
  LAB_MAX_ABS_DIFF: Single = 486.9517823;					{ MAX_L + MAX_A + MAX_B }
  LAB_MAX_EUC_DIFF: Single = 291.5978457;					{ √LAB_MAX_SQR_DIFF }

  HSV_MAX_SQR_DIFF: Single = 52400.00000;					{ MAX_H^2 + MAX_S^2 + MAX_V^2 }
  HSV_MAX_EUC_DIFF: Single = 228.9104629;					{ √HSV_MAX_SQR_DIFF }

  RGB_MAX_EUC_DIFF: Single = 441.6729560;
  XYZ_MAX_EUC_DIFF: Single = 441.6729560;


function Distance_RGB(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_RGB_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_RGB_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;

function Distance_HSV(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_HSV_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_HSV_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;

function Distance_XYZ(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_XYZ_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_XYZ_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;

function Distance_LAB(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_LAB_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_LAB_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;

function Distance_DeltaE(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
function Distance_DeltaE_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;


implementation

uses 
  ColorMath, Math;

//----| RGB |-----------------------------------------------------------------//
function Distance_RGB(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorRGB;
begin
  C1 := PColorRGB(Color1)^;
  C2 := ColorToRGB(Color2);
  Result := Sqrt(Sqr(C1.R-C2.R) + Sqr(C1.G-C2.G) + Sqr(C1.B-C2.B));
end;

function Distance_RGB_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorRGB;
begin
  C1 := PColorRGB(Color1)^;
  C2 := ColorToRGB(Color2);
  Result := Sqr(C1.R-C2.R) + Sqr(C1.G-C2.G) + Sqr(C1.B-C2.B);
end;

function Distance_RGB_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var 
  C1,C2:ColorRGB;
begin
  C1 := PColorRGB(Color1)^;
  C2 := ColorToRGB(Color2);
  Result := 1 - Sqrt(Sqr(C1.R-C2.R) + Sqr(C1.G-C2.G) + Sqr(C1.B-C2.B)) / RGB_MAX_EUC_DIFF;
end;




//----| HSV |-----------------------------------------------------------------//
function Distance_HSV(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var
  C1,C2:ColorHSV;
  deltaH:Single;
begin
  C1 := PColorHSV(Color1)^;
  C2 := ColorToHSV(Color2);

  deltaH := Abs(C1.H - C2.H);
  if deltaH >= 180 then deltaH := 360 - deltaH;
  Result := Sqrt(Sqr(deltaH) + Sqr(C1.S-C2.S) + Sqr(C1.V-C2.V));
end;

function Distance_HSV_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var
  C1,C2:ColorHSV;
  deltaH:Single;
begin
  C1 := PColorHSV(Color1)^;
  C2 := ColorToHSV(Color2);

  deltaH := Abs(C1.H - C2.H);
  if deltaH >= 180 then deltaH := 360 - deltaH;
  Result := Sqr(deltaH) + Sqr(C1.S-C2.S) + Sqr(C1.V-C2.V);
end;

function Distance_HSV_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var
  C1,C2:ColorHSV;
  deltaH:Single;
begin
  C1 := PColorHSV(Color1)^;
  C2 := ColorToHSV(Color2);

  deltaH := Abs(C1.H - C2.H);
  if deltaH >= 180 then deltaH := 360 - deltaH;
  Result := 1 - Sqrt(Sqr(deltaH) + Sqr(C1.S-C2.S) + Sqr(C1.V-C2.V)) / HSV_MAX_EUC_DIFF;
end;



//----| XYZ |-----------------------------------------------------------------//
function Distance_XYZ(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorXYZ;
begin
  C1 := PColorXYZ(Color1)^;
  C2 := ColorToXYZ(Color2);
  Result := Sqrt(Sqr(C1.X-C2.X) + Sqr(C1.Y-C2.Y) + Sqr(C1.Z-C2.Z));
end;

function Distance_XYZ_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorXYZ;
begin
  C1 := PColorXYZ(Color1)^;
  C2 := ColorToXYZ(Color2);
  Result := Sqr(C1.X-C2.X) + Sqr(C1.Y-C2.Y) + Sqr(C1.Z-C2.Z);
end;

function Distance_XYZ_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorXYZ;
begin
  C1 := PColorXYZ(Color1)^;
  C2 := ColorToXYZ(Color2);
  Result := 1 - Sqrt(Sqr(C1.X-C2.X) + Sqr(C1.Y-C2.Y) + Sqr(C1.Z-C2.Z)) / XYZ_MAX_EUC_DIFF;
end;


//----| LAB |-----------------------------------------------------------------//
function Distance_LAB(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorLAB;
begin
  C1 := PColorLAB(Color1)^;
  C2 := ColorToLAB(Color2);
  Result := Sqrt(Sqr(C1.L-C2.L)+Sqr(C1.A-C2.A)+Sqr(C1.B-C2.B));
end;

function Distance_LAB_Sqrd(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorLAB;
begin
  C1 := PColorLAB(Color1)^;
  C2 := ColorToLAB(Color2);
  Result := Sqr(C1.L-C2.L) + Sqr(C1.A-C2.A) + Sqr(C1.B-C2.B);
end;

function Distance_LAB_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorLAB;
begin
  C1 := PColorLAB(Color1)^;
  C2 := ColorToLAB(Color2);
  Result := 1 - Sqrt(Sqr(C1.L-C2.L)+Sqr(C1.A-C2.A)+Sqr(C1.B-C2.B)) / LAB_MAX_EUC_DIFF;
end;


//----| LCh (using LAB-color) |-----------------------------------------------//
function Distance_DeltaE(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorLAB;
begin
  C1 := PColorLAB(Color1)^;
  C2 := ColorToLAB(Color2);
  Result := DeltaE(C1,C2);
end;

function Distance_DeltaE_Normed(Color1:Pointer; Color2:Integer): Single; inline; cdecl;
var C1,C2:ColorLAB;
begin
  C1 := PColorLAB(Color1)^;
  C2 := ColorToLAB(Color2);
  Result := 1 - DeltaE(C1,C2) / DELTAE_MAX_DIFF;
end;


end.
