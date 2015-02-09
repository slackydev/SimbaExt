Unit Colormath;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
{.$DEFINE CAlign}

interface
uses
  CoreMath, Math, SysUtils;

type
  ColorXYZ  = {$IFNDEF CAlign}packed {$ENDIF}record X,Y,Z: Single; end;
  ColorLAB  = {$IFNDEF CAlign}packed {$ENDIF}record L,A,B: Single; end;
  ColorLCH  = {$IFNDEF CAlign}packed {$ENDIF}record L,C,H: Single; end;
  ColorHSV  = {$IFNDEF CAlign}packed {$ENDIF}record H,S,V: Single; end;
  ColorHSL  = {$IFNDEF CAlign}packed {$ENDIF}record H,S,L: Single; end;
  ColorRGB  = {$IFNDEF CAlign}packed {$ENDIF}record R,G,B: Byte;   end;
  ColorRGBA = {$IFNDEF CAlign}packed {$ENDIF}record R,G,B,A: Byte; end;
  
  PColorXYZ  = ^ColorXYZ;
  PColorLAB  = ^ColorLAB;
  PColorLCH  = ^ColorLCH;
  PColorHSV  = ^ColorHSV;
  PColorHSL  = ^ColorHSL;
  PColorRGB  = ^ColorRGB;
  PColorRGBA = ^ColorRGBA;


const
  LAB_TOTAL_L: Single = 100.000;						{0 to 100}
  LAB_TOTAL_A: Single = 184.449;						{-86.19496155 to 98.2534256}
  LAB_TOTAL_B: Single = 202.504;						{-107.8887482 to 94.6146469}


const
  XYZ_POW_2_4: array[0..255] of Single =
  (
    0.000834,  0.000984,  0.001148,  0.001328,  0.001523,  0.001733,  0.001960,  0.002203,  0.002463,  0.002740,  0.003035,  0.003347,  0.003677,  0.004025,  0.004391,  0.004777,  0.005182,  0.005605,  0.006049,  0.006512,  0.006995,  0.007499,  0.008023,  0.008568,  0.009134,  0.009721,  0.010330,  0.010960,  0.011612,  0.012286,  0.012983,  0.013702,  0.014444,  0.015209,  0.015996,  0.016807,  0.017642,  0.018500,  0.019382,  0.020289,  0.021219,  0.022174,  0.023153,  0.024158,  0.025187,  0.026241,  0.027321,  0.028426,  0.029557,  0.030713,  0.031896,  0.033105,
    0.034340,  0.035601,  0.036889,  0.038204,  0.039546,  0.040915,  0.042311,  0.043735,  0.045186,  0.046665,  0.048172,  0.049707,  0.051269,  0.052861,  0.054480,  0.056128,  0.057805,  0.059511,  0.061246,  0.063010,  0.064803,  0.066626,  0.068478,  0.070360,  0.072272,  0.074214,  0.076185,  0.078187,  0.080220,  0.082283,  0.084376,  0.086500,  0.088656,  0.090842,  0.093059,  0.095307,  0.097587,  0.099899,  0.102242,  0.104616,  0.107023,  0.109462,  0.111932,  0.114435,  0.116971,  0.119538,  0.122139,  0.124772,  0.127438,  0.130136,  0.132868,  0.135633,
    0.138432,  0.141263,  0.144128,  0.147027,  0.149960,  0.152926,  0.155926,  0.158961,  0.162029,  0.165132,  0.168269,  0.171441,  0.174647,  0.177888,  0.181164,  0.184475,  0.187821,  0.191202,  0.194618,  0.198069,  0.201556,  0.205079,  0.208637,  0.212231,  0.215861,  0.219526,  0.223228,  0.226966,  0.230740,  0.234551,  0.238398,  0.242281,  0.246201,  0.250158,  0.254152,  0.258183,  0.262251,  0.266356,  0.270498,  0.274677,  0.278894,  0.283149,  0.287441,  0.291771,  0.296138,  0.300544,  0.304987,  0.309469,  0.313989,  0.318547,  0.323143,  0.327778,
    0.332452,  0.337164,  0.341914,  0.346704,  0.351533,  0.356400,  0.361307,  0.366253,  0.371238,  0.376262,  0.381326,  0.386429,  0.391572,  0.396755,  0.401978,  0.407240,  0.412543,  0.417885,  0.423268,  0.428690,  0.434154,  0.439657,  0.445201,  0.450786,  0.456411,  0.462077,  0.467784,  0.473531,  0.479320,  0.485150,  0.491021,  0.496933,  0.502886,  0.508881,  0.514918,  0.520996,  0.527115,  0.533276,  0.539479,  0.545724,  0.552011,  0.558340,  0.564712,  0.571125,  0.577580,  0.584078,  0.590619,  0.597202,  0.603827,  0.610496,  0.617207,  0.623960,
    0.630757,  0.637597,  0.644480,  0.651406,  0.658375,  0.665387,  0.672443,  0.679542,  0.686685,  0.693872,  0.701102,  0.708376,  0.715694,  0.723055,  0.730461,  0.737910,  0.745404,  0.752942,  0.760525,  0.768151,  0.775822,  0.783538,  0.791298,  0.799103,  0.806952,  0.814847,  0.822786,  0.830770,  0.838799,  0.846873,  0.854993,  0.863157,  0.871367,  0.879622,  0.887923,  0.896269,  0.904661,  0.913099,  0.921582,  0.930111,  0.938686,  0.947307,  0.955973,  0.964686,  0.973445,  0.982251,  0.991102,  1.000000
  );
  
  ONE_DIV_THREE:     Single =  1.0 / 3.0;
  NEG_ONE_DIV_THREE: Single = -1.0 / 3.0;


function ColorToRGB(Color:Integer): ColorRGB; Inline;
function RGBToColor(constref RGB:ColorRGB): Integer; Inline;
function ColorIntensity(Color:Integer): Byte; Inline;
function ColorToGray(Color:Integer): Byte; Inline;
function ColorToXYZ(Color:Integer): ColorXYZ; Inline;
function XYZToLAB(constref XYZ:ColorXYZ): ColorLAB; Inline;
function ColorToLAB(Color:Integer): ColorLAB; Inline;
function LABToLCH(constref LAB:ColorLAB): ColorLCH; Inline;
function ColorToLCH(Color:Integer): ColorLCH; Inline;
function ColorToHSV(Color:Integer): ColorHSV; Inline;
function HSVToRGB(constref HSV:ColorHSV): ColorRGB; Inline;
function HSVToColor(constref HSV:ColorHSV): Integer; Inline;

//color distance / difference
function DeltaE(constref LAB1, LAB2: ColorLAB): Single; Inline;


//--------------------------------------------------
implementation


(*
  Converts an RGB integer representation to seprate R,G,B values
*)
function ColorToRGB(Color:Integer): ColorRGB; Inline;
begin
  Result.R := color and $FF;
  Result.G := color shr 8 and $FF;
  Result.B := color shr 16 and $FF;
end;


(*
  Converts R,G,B values to an integer representation of the color
*)
function RGBToColor(constref RGB:ColorRGB): Integer; Inline;
begin
  Result := RGB.R or RGB.G shl 8 or RGB.B shl 16;
end;


(*
  Average of R,G,B - Can be used to measure intensity.
*)
function ColorIntensity(Color:Integer): Byte; Inline;
begin
  //Result := (1 + (Color and $FF) + (Color shr 8 and $FF) + (Color shr 16 and $FF)) * 341 shr 10;
  Result := ((Color and $FF) + (Color shr 8 and $FF) + (Color shr 16 and $FF)) div 3;
end;


(*
  Convert Color(RGB) to Grayscale / Luma
  Rec. 601: Y' = 0.299 R' + 0.587 G' + 0.114 B'
*)
function ColorToGray(Color:Integer): Byte; Inline;
begin
  Result := (76  * (Color and $FF) +
             150 * (Color shr 8 and $FF) +
             29  * (Color shr 16 and $FF) + 255) shr 8;
end;


(*
  Converts Color(RGB) to CIE-XYZ approximation
  X,Y and Z are in the range 0..255 
*)
function ColorToXYZ(Color:Integer): ColorXYZ; Inline;
var
  R,G,B:Byte;
  vR,vG,vB: Single;
begin
  R := color and $FF;
  G := color shr 8 and $FF;
  B := color shr 16 and $FF;

  if R > 10 then vR := XYZ_POW_2_4[R]
  else vR := (R / 255.0) / 12.92;
  
  if G > 10 then vG := XYZ_POW_2_4[G]
  else vG := (G / 255.0) / 12.92;
  
  if B > 10 then vB := XYZ_POW_2_4[B]
  else vB := (B / 255.0) / 12.92;
  
  vR := vR * 255; //Same range as RGB
  vG := vG * 255;
  vB := vB * 255;
  
  //Observer 0 deg, Illuminant = D65
  Result.X := vR * 0.4338 + vG * 0.3762 + vB * 0.1900;
  Result.Y := vR * 0.2126 + vG * 0.7152 + vB * 0.0722;
  Result.Z := vR * 0.0175 + vG * 0.1092 + vB * 0.8733;
end;


(*
  Converts XYZ to LAB approximation
  Input:
    X,Y and Z in the range 0..255

  Output:
    L range [0..100]
    A range [-86.195..98.253]
    B range [-107.889..94.615]
*)
function XYZToLAB(constref XYZ:ColorXYZ): ColorLAB; Inline;
var X,Y,Z:Single;
begin
  X := XYZ.X / 255;
  Y := XYZ.Y / 255;
  Z := XYZ.Z / 255;
  
  if X > 0.008856 then X := fcbrt(X)
  else X := (7.787 * X) + 0.137931;
  
  if Y > 0.008856 then Y := fcbrt(Y)
  else Y := (7.787 * Y) + 0.137931;
  
  if Z > 0.008856 then Z := fcbrt(Z)
  else Z := (7.787 * Z) + 0.137931;

  Result.L := (116.0 * Y) - 16.0;
  Result.A := 500.0 * (X - Y);
  Result.B := 200.0 * (Y - Z);
end;


(*
  Converts Color(RGB) to CIE-LAB (fast approximation)

  Output:
    L range [0..100]
    A range [-86.195..98.253]
    B range [-107.889..94.615]
*)
function ColorToLAB(Color:Integer): ColorLAB; Inline;
var
  iR,iG,iB:Byte;
  vR,vG,vB,X,Y,Z: Single;
begin
  iR := color and $FF;
  iG := color shr 8 and $FF;
  iB := color shr 16 and $FF;

  if iR > 10 then vR := XYZ_POW_2_4[iR]
  else vR := (iR / 255.0) / 12.92;
  
  if iG > 10 then vG := XYZ_POW_2_4[iG]
  else vG := (iG / 255.0) / 12.92;
  
  if iB > 10 then vB := XYZ_POW_2_4[iB]
  else vB := (iB / 255.0) / 12.92;

  //Observer 0 deg, Illuminant = D65
  X := vR * 0.4338 + vG * 0.3762 + vB * 0.1900;
  Y := vR * 0.2126 + vG * 0.7152 + vB * 0.0722;
  Z := vR * 0.0175 + vG * 0.1092 + vB * 0.8733;

  if X > 0.008856 then X := fcbrt(X)
  else X := (7.787 * X) + 0.137931;
  if Y > 0.008856 then Y := fcbrt(Y)
  else Y := (7.787 * Y) + 0.137931;
  if Z > 0.008856 then Z := fcbrt(Z)
  else Z := (7.787 * Z) + 0.137931;

  Result.L := (116.0 * Y) - 16.0;
  Result.A := 500.0 * (X - Y);
  Result.B := 200.0 * (Y - Z);
end;


(*
  Converts LAB to LCH

  Input (assumed):
    L range [0..100]
    A range [-86.195..98.253]
    B range [-107.889..94.615]

  Output (based on assumed input):
    L range [0..100]
    C range [0..133.865]
    H value is in degrees [0..360]
*)
function LABToLCH(constref LAB:ColorLAB): ColorLCH; Inline;
begin
  Result.L := LAB.L;
  Result.C := Sqrt(LAB.A*LAB.A + LAB.B*LAB.B);
  Result.H := ArcTan2(LAB.B,LAB.A);
  
  if (Result.H > 0) then
    Result.H := (Result.H / PI) * 180
  else 
    Result.H := 360 - (-Result.H / PI) * 180;
end;


(*
  Converts Color(RGB) to LCH
  
  Output:
    L range [0..100]
    C range [0..133.865]
    H value is in degrees [0..360]
*)
function ColorToLCH(Color:Integer): ColorLCH; Inline;
var
  LAB: ColorLAB;
begin
  LAB := ColorToLAB(Color);
  Result.C := Sqrt(LAB.A*LAB.A + LAB.B*LAB.B);
  Result.H := ArcTan2(LAB.B,LAB.A);
  Result.L := LAB.L;
  if (Result.H > 0) then
    Result.H := (Result.H / PI) * 180
  else 
    Result.H := 360 - (-Result.H / PI) * 180;
end;


(*
  Converts Color (RGB) to HSV
  
  Output:
    H value is in degrees [0..360]
    S and V values are percentages [0..100]
*)
function ColorToHSV(Color:Integer): ColorHSV; Inline;
var
  chroma,t: Single;
  R,G,B,K: Single;
begin
  R := (color and $FF) / 255;
  G := (color shr 8 and $FF) / 255;
  B := (color shr 16 and $FF) / 255;
  K := 0.0;

  if (g < b) then
  begin
    t := b; b := g; g := t;
    K := -1.0;
  end;

  if (r < g) then
  begin
    t := r; r := g; g := t;
    K := NEG_ONE_DIV_THREE - K;
  end;

  chroma := r - Min(g, b);
  Result.h := Abs(K + (g - b) / (6.0 * chroma + 1.0e-20)) * 360;
  Result.s := chroma / (r + 1.0e-20)  * 100;
  Result.v := r * 100;
end;


(*
  Converts HSV to RGB

  Input:
    H values is in degrees [0..360]
    S and V values are percentages [0..100]
  
  Output: 
    R,G,B is in range of [0..255]
*)
function HSVToRGB(constref HSV:ColorHSV): ColorRGB; Inline;
var
  h,s,v,i,f,p,q,t,R,G,B:Single;
begin
  H := HSV.H / 360;
  S := HSV.S / 100;
  V := HSV.V / 100;
  R := 0; G := 0; B := 0;
  if (S = 0.0) then
  begin
    Result.R := Trunc(V * 255);
    Result.G := Trunc(V * 255);
    Result.B := Trunc(V * 255);
  end else 
  begin
    i := Trunc(H * 6); 
    f := (H * 6) - i;
    p := V * (1 - S);
    q := V * (1 - S * f);
    t := V * (1 - S * (1 - f)); 
    i := Modulo(i, 6);
    case Trunc(i) of
      0:begin 
          R := v;
          G := t;
          B := p;
        end;
      1:begin 
          R := q;
          G := v;
          B := p;
        end;
      2:begin 
          R := p;
          G := v;
          B := t;
        end;
      3:begin 
          R := p;
          G := q;
          B := v;
        end;
      4:begin 
          R := t;
          G := p;
          B := v;
        end;
      5:begin 
          R := v;
          G := p;
          B := q;
        end;
    end; 

    Result.R := Trunc(R * 255);
    Result.G := Trunc(G * 255);
    Result.B := Trunc(B * 255);
  end;
end;


(*
  Converts HSV to Color(RGB)

  Input:
    H values is in degrees [0..360]
    S and V values are percentages [0..100]

  Output:
    Integer rep of the RGB values
*)
function HSVToColor(constref HSV:ColorHSV): Integer; Inline;
var
  RGB:ColorRGB;
begin
  RGB:=HSVToRGB(HSV);
  Result := RGB.R or RGB.G shl 8 or RGB.B shl 16;
end;






{===| Color difference/distance |==============================================}

(*
  Measure color distance using a modified version of Delta-E 1994
  Fixes some issues found in the original DeltaE-94 algorithm
*)
function DeltaE(constref LAB1, LAB2: ColorLAB): Single; Inline;
var
  chroma,hue,H,C: Single;
begin
  chroma := (Sqrt(LAB1.a*LAB1.a + LAB1.b*LAB1.b) +
             Sqrt(LAB2.a*LAB2.a + LAB2.b*LAB2.b)) / 2;
  hue := Sqrt(Abs((Sqr(LAB1.a-LAB2.a) + Sqr(LAB1.b - LAB2.b)) - Sqr(chroma)));

  C := chroma / (1 + 0.045 * chroma);
  H := hue / (1 + 0.015 * chroma);
  Result := Sqrt(Sqr(LAB1.L-LAB2.L) + C*C + H*H);
end;


end.
