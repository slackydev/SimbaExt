Unit ColorMath;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}

interface
uses
  CoreMath, Math, SysUtils;

const
  XYZ_Pow: array[0..255] of Single =
  (
    0.000834, 0.000984,  0.001148,  0.001328,  0.001523,  0.001733,  0.001960,  0.002203,  0.002463,  0.002740,  0.003035,  0.003347,  0.003677,  0.004025,  0.004391,  0.004777,  0.005182,  0.005605,  0.006049,  0.006512,  0.006995,  0.007499,  0.008023,  0.008568,  0.009134,  0.009721,  0.010330,  0.010960,  0.011612,  0.012286,  0.012983,  0.013702,  0.014444,  0.015209,  0.015996,  0.016807,  0.017642,  0.018500,  0.019382,  0.020289,  0.021219,  0.022174,  0.023153,  0.024158,  0.025187,  0.026241,  0.027321,  0.028426,  0.029557,  0.030713,  0.031896,  0.033105,
    0.034340, 0.035601,  0.036889,  0.038204,  0.039546,  0.040915,  0.042311,  0.043735,  0.045186,  0.046665,  0.048172,  0.049707,  0.051269,  0.052861,  0.054480,  0.056128,  0.057805,  0.059511,  0.061246,  0.063010,  0.064803,  0.066626,  0.068478,  0.070360,  0.072272,  0.074214,  0.076185,  0.078187,  0.080220,  0.082283,  0.084376,  0.086500,  0.088656,  0.090842,  0.093059,  0.095307,  0.097587,  0.099899,  0.102242,  0.104616,  0.107023,  0.109462,  0.111932,  0.114435,  0.116971,  0.119538,  0.122139,  0.124772,  0.127438,  0.130136,  0.132868,  0.135633,
    0.138432, 0.141263,  0.144128,  0.147027,  0.149960,  0.152926,  0.155926,  0.158961,  0.162029,  0.165132,  0.168269,  0.171441,  0.174647,  0.177888,  0.181164,  0.184475,  0.187821,  0.191202,  0.194618,  0.198069,  0.201556,  0.205079,  0.208637,  0.212231,  0.215861,  0.219526,  0.223228,  0.226966,  0.230740,  0.234551,  0.238398,  0.242281,  0.246201,  0.250158,  0.254152,  0.258183,  0.262251,  0.266356,  0.270498,  0.274677,  0.278894,  0.283149,  0.287441,  0.291771,  0.296138,  0.300544,  0.304987,  0.309469,  0.313989,  0.318547,  0.323143,  0.327778,
    0.332452, 0.337164,  0.341914,  0.346704,  0.351533,  0.356400,  0.361307,  0.366253,  0.371238,  0.376262,  0.381326,  0.386429,  0.391572,  0.396755,  0.401978,  0.407240,  0.412543,  0.417885,  0.423268,  0.428690,  0.434154,  0.439657,  0.445201,  0.450786,  0.456411,  0.462077,  0.467784,  0.473531,  0.479320,  0.485150,  0.491021,  0.496933,  0.502886,  0.508881,  0.514918,  0.520996,  0.527115,  0.533276,  0.539479,  0.545724,  0.552011,  0.558340,  0.564712,  0.571125,  0.577580,  0.584078,  0.590619,  0.597202,  0.603827,  0.610496,  0.617207,  0.623960,
    0.630757, 0.637597,  0.644480,  0.651406,  0.658375,  0.665387,  0.672443,  0.679542,  0.686685,  0.693872,  0.701102,  0.708376,  0.715694,  0.723055,  0.730461,  0.737910,  0.745404,  0.752942,  0.760525,  0.768151,  0.775822,  0.783538,  0.791298,  0.799103,  0.806952,  0.814847,  0.822786,  0.830770,  0.838799,  0.846873,  0.854993,  0.863157,  0.871367,  0.879622,  0.887923,  0.896269,  0.904661,  0.913099,  0.921582,  0.930111,  0.938686,  0.947307,  0.955973,  0.964686,  0.973445,  0.982251,  0.991102,  1.000000
  );


procedure ColorToRGB(Color:Integer; out R,G,B:Byte); Inline;
procedure ColorToRGB2(Color:Integer; out R,G,B:Integer); Inline;
function RGBToColor(R,G,B:Byte): Integer; Inline;
function RGBIntToColor(R,G,B:Integer): Integer; Inline;
function ColorIntensity(Color:Integer): Byte; Inline;
function ColorToGray(Color:Integer): Byte; Inline;
procedure ColorToXYZ(Color:Integer; out X,Y,Z:Single); Inline;
procedure ColorToLAB(color:Integer; out L,A,B:Single); Inline;
procedure ColorToLCH(Color:Integer; out L,C,H:Single); Inline;
procedure ColorToHSV(Color:Integer; out H,S,V:Single); Inline;
procedure HSVToRGB(H,S,V:Single; out R,G,B:Integer); Inline;
function LCHDiff(L0,a0,b0, L1,a1,b1: Single): Single; Inline;


//--------------------------------------------------
implementation


// Cuberoot spezialised for quick computation of RGB to LAB
// Ranges are 0.0 to 1.500  +/-
function LABCbrt(x:Single): Single; Inline;
begin
  if x <= 0.0 then Exit(0.0)
  else if x <= 0.0015 then Result:= (0.075 - (0.0004 - x) / 0.017)
  else if x <= 0.012 then Result := (0.188 - (0.0066 - x) / 0.106)
  else if x <= 0.020 then Result := (0.195 - (0.0110 - x) / 0.110)
  else if x <= 0.025 then Result := (0.220 - (0.0140 - x) / 0.130)
  else if x <= 0.035 then Result := (0.290 - (0.0270 - x) / 0.270)
  else if x <= 0.050 then Result := (0.307 - (0.0320 - x) / 0.300)
  else if x <= 0.075 then Result := (0.400 - (0.0640 - x) / 0.480)
  else if x <= 0.125 then Result := (0.450 - (0.0910 - x) / 0.600)
  else if x <= 0.185 then Result := (0.580 - (0.2000 - x) / 1.000)
  else if x <= 0.330 then Result := (0.625 - (0.2440 - x) / 1.175)
  else if x <= 0.470 then Result := (0.725 - (0.3810 - x) / 1.575)
  else if x <= 0.640 then Result := (0.800 - (0.5120 - x) / 1.910)
  else if x <= 0.900 then Result := (0.900 - (0.7300 - x) / 2.430)
  else if x <= 1.500 then Result := (1.000 - (1.0000 - x) / 3.000)
  else Result := sqrt(x);
  // Run 1 step newton to finallize the result.
  Result := Result - (Result*Result*Result - x) / (3.0 * Result * Result);
end;  
    
procedure ColorToRGB(Color:Integer; out R,G,B:Byte); Inline;
begin
  R := (color and $FF);
  G := ((color shr 8) and $FF);
  B := ((color shr 16) and $FF);
end;


procedure ColorToRGB2(Color:Integer; out R,G,B:Integer); Inline;
begin
  R := (color and $FF);
  G := ((color shr 8) and $FF);
  B := ((color shr 16) and $FF);
end;

function RGBToColor(R,G,B:Byte): Integer; Inline;
begin
  Result := R or G shl 8 or B shl 16;
end;

function RGBIntToColor(R,G,B:Integer): Integer; Inline;
begin
  Result := R or G shl 8 or B shl 16;
end;

// Average of R,G,B - Can be used to measure intensity.
function ColorIntensity(Color:Integer): Byte; Inline;
begin
  Result := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
end;

// Grayscale / Luma | CCIR 601 > Y' = 0.299 R' + 0.587 G' + 0.114 B'
function ColorToGray(Color:Integer): Byte; Inline;
begin
  Result := Trunc((0.299 * (Color and $FF)) +
                  (0.587 * ((Color shr 8) and $FF)) +
                  (0.114 * ((Color shr 16) and $FF)));
end;


// Convert from Color to CIE-XYZ approximation
procedure ColorToXYZ(Color:Integer; out X,Y,Z:Single); Inline;
var
  R,G,B:Byte;
  vR,vG,vB: Single;
begin
  R := (color and $FF);
  G := ((color shr 8) and $FF);
  B := ((color shr 16) and $FF);

  if R > 11 then vR := XYZ_Pow[R]
  else vR := (R / 255.0) / 12.92;
  if G > 11 then vG := XYZ_Pow[G]
  else vG := (G / 255.0) / 12.92;
  if B > 11 then vB := XYZ_Pow[B]
  else vB := (B / 255.0) / 12.92;
  
  vR := vR * 255; //Same range as RGB (100 resulted in to small differances for my likings..)
  vG := vG * 255;
  vB := vB * 255;
  
  //Observer 0 deg, Illuminant = D65
  X := vR * 0.4338 + vG * 0.3762 + vB * 0.1900;
  Y := vR * 0.2126 + vG * 0.7152 + vB * 0.0722;
  Z := vR * 0.0175 + vG * 0.1092 + vB * 0.8733;
end;


// Convert from Color to CIE-LAB (fast) approximation
procedure ColorToLAB(Color:Integer; out L,A,B:Single); Inline;
var
  iR,iG,iB:Byte;
  vR,vG,vB,X,Y,Z: Single;
begin
  iR := (color and $FF);
  iG := ((color shr 8) and $FF);
  iB := ((color shr 16) and $FF);

  if iR > 11 then vR := XYZ_Pow[iR]
  else vR := (iR / 255.0) / 12.92;
  if iG > 11 then vG := XYZ_Pow[iG]
  else vG := (iG / 255.0) / 12.92;
  if iB > 11 then vB := XYZ_Pow[iB]
  else vB := (iB / 255.0) / 12.92;

  //Observer 0 deg, Illuminant = D65
  X := vR * 0.4338 + vG * 0.3762 + vB * 0.1900;
  Y := vR * 0.2126 + vG * 0.7152 + vB * 0.0722;
  Z := vR * 0.0175 + vG * 0.1092 + vB * 0.8733;

  if X > 0.008856 then X := LABCbrt(X)
  else X := (7.787 * X) + 0.137931;
  if Y > 0.008856 then Y := LABCbrt(Y)
  else Y := (7.787 * Y) + 0.137931;
  if Z > 0.008856 then Z := LABCbrt(Z)
  else Z := (7.787 * Z) + 0.137931;

  L := (116.0 * Y) - 16.0;
  A := 500.0 * (X - Y);
  B := 200.0 * (Y - Z);
end;


//Duh...
procedure ColorToLCH(Color:Integer; out L,C,H:Single); Inline;
var
  A,B: Single;
begin
  ColorToLAB(Color, L,A,B);
  C := Sqrt(A*A + B*B);
  H := ArcTan2(B,A);
  if (H > 0) then H := (H / 3.1415926) * 180
  else H := 360 - (-H / 3.1415926) * 180;
end;


//Convert RGB to HSV
procedure ColorToHSV(Color:Integer; out H,S,V:Single); Inline;
var
  vMax,vMin,chroma: Single;
  vR,vG,vB: Single;
begin
  vR := (color and $FF) / 255;
  vG := ((color shr 8) and $FF) / 255;
  vB := ((color shr 16) and $FF) / 255;

  vMin := min(min(vR, vG), vB);
  vMax := max(max(vR, vG), vB);
  chroma := vMax - vMin;             // Delta RGB value

  if (chroma <> 0) then
  begin
    if vR = vMax then
    begin
      H := (vG - vB) / chroma;
      if(H < 0.0) then H += 6.0;
    end
    else if (vG = vMax) then
      H := ((vB - vR) / chroma) + 2.0
    else
      H := ((vR - vG) / chroma) + 4.0;

    H := (H / 6.0) * 200;
    S := (Chroma/vMax) * 100;
  end else
  begin
     H := 0.0;
     S := 0.0;
  end;
  V := vMax * 100;
end;


// Convert HSV to RGB
procedure HSVToRGB(H,S,V:Single; out R,G,B:Integer); Inline;
var
  i,f,p,q,t,vr,vg,vb:Single;
begin
  H := H / 200;
  S := S / 100;
  V := V / 100;
  vr := 0; vg := 0; vb := 0;
  if (S = 0.0) then
  begin
    R := Trunc(V * 255);
    G := Trunc(V * 255);
    B := Trunc(V * 255);
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
          vr := v; 
          vg := t; 
          vb := p; 
        end;
      1:begin 
          vr := q; 
          vg := v; 
          vb := p; 
        end;
      2:begin 
          vr := p; 
          vg := v; 
          vb := t; 
        end;
      3:begin 
          vr := p; 
          vg := q; 
          vb := v;    
        end;
      4:begin 
          vr := t; 
          vg := p; 
          vb := v;    
        end;
      5:begin 
          vr := v; 
          vg := p; 
          vb := q; 
        end;
    end; 

    R := Trunc(vr * 255);
    G := Trunc(vg * 255);
    B := Trunc(vb * 255);
  end;
end;




// Measure LAB distance by using Delta-E 1994 which computes difference in LCh-space. 
function LCHDiff(L0,a0,b0, L1,a1,b1: Single): Single; Inline;
var
  C0,C1,dC,dH: Single;
begin
  C0 := Sqrt(a0*a0 + b0*b0);
  C1 := Sqrt(a1*a1 + b1*b1);
  dC := C0 - C1;
  dH := Sqrt(abs((Sqr(a0-a1) + Sqr(b0 - b1)) - Sqr(dC)));

  dC := dC / (1 + (0.045 * C0));
  dH := dH / (1 + (0.015 * C0));
  Result := Sqrt(Sqr(L0-L1) + (dC*dC) + (dH*dH));
end;

end.
