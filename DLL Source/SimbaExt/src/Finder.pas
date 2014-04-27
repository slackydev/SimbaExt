Unit Finder;
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
  CoreTypes, Math, SysUtils;

function MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
function MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
function MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
function MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;

function ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; Cdecl;
function ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntArray; Tol:Integer): Boolean; Cdecl;
function ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;
function ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;


//--------------------------------------------------
implementation

uses
  ColorMath, HashMap, CoreMath, PointList, MatrixMath, MatrixOps;



// Cross correlate a color with an image in RGB-Space
function MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
var
  W,H,X,Y:Integer;
  R,G,B,R1,G1,B1:Byte;
  Diff: Single;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Result, H+1,W+1);
  ColorToRGB(Color, R,G,B);

  // If we want euclidean distance correlation
  if CCMode in [CC_Euclid, CC_EuclidSquared, CC_EuclidNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        ColorToRGB(ImgArr[Y][X], R1,G1,B1);
        Result[y][x] := Sqr(R1-R) + Sqr(G1-G) + Sqr(B1-B);
      end;
    if (CCMode = CC_Euclid) then
      Result := fSqrt(Result)
    else if (CCMode = CC_EuclidNormed) then
    begin
      diff := Sqrt(Sqr(255) * 3);
      Result := 1 - (fSqrt(Result) / diff);
    end;
  end;

  // if we want chebyshev distance correlation
  if CCMode in [CC_Cheb, CC_ChebNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        ColorToRGB(ImgArr[Y][X], R1,G1,B1);
        Result[y][x] := Abs(R1-R) + Abs(G1-G) + Abs(B1-B);
      end;
    if (CCMode = CC_ChebNormed) then
      Result := 1 - (Result / 765.0);
  end;
end;


// Cross correlate a color with an image in XYZ-Space
function MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
var
  W,H,X,Y:Integer;
  X0,Y0,Z0,X1,Y1,Z1:Single;
  diff: Single;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Result, H+1,W+1);
  ColorToXYZ(Color, X0,Y0,Z0);

  // If we want euclidean distance correlation
  if CCMode in [CC_Euclid, CC_EuclidSquared, CC_EuclidNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        ColorToXYZ(ImgArr[Y][X], X1,Y1,Z1);
        Result[y][x] := Sqr(X1-X0) + Sqr(Y1-Y0) + Sqr(Z1-Z0);
      end;
    if (CCMode = CC_Euclid) then
      Result := fSqrt(Result)
    else if (CCMode = CC_EuclidNormed) then
    begin
      diff := Sqrt(Sqr(255) * 3);
      Result := 1 - (fSqrt(Result) / diff);
    end;
  end;

  // if we want chebyshev distance correlation
  if CCMode in [CC_Cheb, CC_ChebNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        ColorToXYZ(ImgArr[Y][X], X1,Y1,Z1);
        Result[y][x] := Abs(X1-X0) + Abs(Y1-Y0) + Abs(Z1-Z0);
      end;
    if (CCMode = CC_ChebNormed) then
      Result := 1 - (Result / 765.0);
  end;
end;



// Cross correlate a color with an image in LAB-Space
function MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
var
  W,H,X,Y:Integer;
  L0,A0,B0,L1,A1,B1:Single;
  Similarity: Single;
  SimMap: TF32Hash;
  LAB: ColorLAB;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Result, H+1,W+1);
  ColorToLAB(Color, L0,A0,B0);

  SimMap := TF32Hash.Create((W+1)*(H+1));

  // If we want euclidean distance correlation
  if CCMode in [CC_Euclid, CC_EuclidSquared, CC_EuclidNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        if not(SimMap.Get(ImgArr[Y][X], Similarity)) then
        begin
          ColorToLAB(ImgArr[Y][X], L1,a1,b1);
          Similarity := Sqr(L1-L0) + Sqr(a1-a0) + Sqr(b1-b0);
          SimMap.Add(ImgArr[Y][X], Similarity);
        end;
        Result[y][x] := Similarity;
      end;
    if (CCMode = CC_Euclid) then
      Result := fSqrt(Result)
    else if (CCMode = CC_EuclidNormed) then
      Result := 1 - (fSqrt(Result) / 265);
  end;

  // if we want chebyshev distance correlation
  if CCMode in [CC_Cheb, CC_ChebNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        if not(SimMap.Get(ImgArr[Y][X], Similarity)) then
        begin
          ColorToLAB(ImgArr[Y][X], L1,a1,b1);
          Similarity := Abs(L1-L0) + Abs(a1-a0) + Abs(b1-b0);
          SimMap.Add(ImgArr[Y][X], Similarity);
        end;
        Result[y][x] := Similarity;
      end;
    if (CCMode = CC_ChebNormed) then
      Result := 1 - (Result / 415.0);
  end;
  SimMap.Destroy;
end;



// Cross correlate a color with an image in LCh-Space
function MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; Cdecl;
var
  W,H,X,Y:Integer;
  L0,a0,b0,L1,a1,b1,similarity:Single;
  SimMap: TF32Hash;
  LAB: ColorLAB;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Result, H+1,W+1);
  if CCMode in [CC_Cheb, CC_ChebNormed, CC_EuclidSquared] then begin
    WriteLn('Error: not supported TCCorrMode use: CC_Euclid|CC_EuclidNormed');
    Exit;
  end;

  SimMap := TF32Hash.Create((W+1)*(H+1));

  ColorToLAB(Color, L0,a0,b0); 
  // If we want euclidean distance correlation
  if CCMode in [CC_Euclid, CC_EuclidNormed] then
  begin
    for Y:=0 to H do
      for X:=0 to W do
      begin
        if not(SimMap.Get(ImgArr[Y][X], Similarity)) then
        begin
          ColorToLAB(ImgArr[Y][X], L1,a1,b1);
          Similarity := LCHDiff(L0,a0,b0, L1,a1,b1);
          SimMap.Add(ImgArr[Y][X], Similarity);
        end;
        Result[y][x] := Similarity;
      end;
    if (CCMode = CC_EuclidNormed) then
    begin
      Result := 1 - (Result / 130);
    end;
  end;
  SimMap.Destroy;
end;



{*
 Internal commands.. no need to export. ..:::. Should actually rewrite.
*}

// Find multiple matches of specified color.
function ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntArray; Tol:Integer): Boolean; Cdecl;
var
  W,H,X,Y,i:Integer;
  R,G,B,R1,G1,B1:Byte;
  TPS: TPointList;
begin
  Result := True;
  W := High(ImgArr[0]);
  H := High(ImgArr);
  TPS.Init;
  
  Tol := Sqr(Tol);
  for Y:=0 to H do
    for X:=0 to W do
    begin
      ColorToRGB(ImgArr[Y][X], R1,G1,B1);
      for i:=0 to High(Colors) do
      begin
        ColorToRGB(Colors[i], R,G,B);
        if ((Sqr(R1 - R) + Sqr(G1 - G) + Sqr(B1 - B)) <= Tol) then
        begin
          TPS.Append(X,Y);
          Break;
        end;
      end;
    end;

  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


  
// Find multiple matches of specified color.
function ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; Cdecl;
var
  W,H,X,Y:Integer;
  R,G,B,R1,G1,B1:Byte;
  TPS: TPointList;
begin
  Result := True;
  W := High(ImgArr[0]);
  H := High(ImgArr);
  TPS.Init;
  ColorToRGB(Color, R,G,B);
  Tol := Sqr(Tol);
  for Y:=0 to H do
    for X:=0 to W do
    begin
      ColorToRGB(ImgArr[Y][X], R1,G1,B1);
      if ((Sqr(R1 - R) + Sqr(G1 - G) + Sqr(B1 - B)) <= Tol) then
        TPS.Append(X,Y);
    end;

  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


// Find multiple matches of specified color.
function ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;
var
  W,H,X,Y:Integer;
  L,C,HH, C1,H1, DeltaHue,FF,EE,DD:Single;
  LAB: ColorLAB;
  LABDict: TLABHash;
  TPS: TPointList;
begin
  Result := True;

  W := High(ImgArr[0]);
  H := High(ImgArr);
  LABDict := TLABHash.Create((W+1)*(H+1));
  TPS.Init;
  ColorToLCH(Color, L,C,HH);
  LightTol := Sqr(LightTol);

  for Y:=0 to H do
    for X:=0 to W do
    begin
      if not(LABDict.Get(ImgArr[Y][X], LAB)) then
      begin
        ColorToLAB(ImgArr[Y][X], FF,EE,DD);
        LAB.A := FF;
        LAB.B := EE;
        LAB.B := DD;
        LABDict.Add(ImgArr[Y][X], LAB);
      end;
      C1 := Sqrt(Sqr(LAB.A) + Sqr(LAB.B));

      //Within Lightness and Chroma? (142 = tolmax)
      if ((Sqr(LAB.L - L) + Sqr(C1 - C)) <= LightTol) then
      begin
        H1 := ArcTan2(LAB.B,LAB.A);
        if (H1 > 0) then H1 := (H1 / 3.1415926536) * 180
        else H1 := 360 - (-H1 / 3.1415926536) * 180;
        DeltaHue := Modulo((H1 - HH + 180), 360) - 180;
        //Within Hue tolerance? (180 = tolmax)
        if (Abs(DeltaHue) <= ColorTol) then
          TPS.Append(X,y);
      end;
    end;

  LABDict.Destroy;
  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


// Find multiple matches of specified color.
function ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol,LightTol:Integer): Boolean; Cdecl;
var
  W,H,X,Y:Integer;
  L,A,B,FF,EE,DD:Single;
  LAB: ColorLAB;
  LABDict: TLABHash;
  TPS: TPointList;
begin
  Result := True;

  W := High(ImgArr[0]);
  H := High(ImgArr);
  LABDict := TLABHash.Create((W+1)*(H+1));
  TPS.Init;
  ColorToLAB(Color, L,A,B);
  ColorTol := Sqr(ColorTol);

  for Y:=0 to H do
    for X:=0 to W do
    begin
      if not(LABDict.Get(ImgArr[Y][X], LAB)) then
      begin
        ColorToLAB(ImgArr[Y][X], FF,EE,DD);
        LAB.L := FF;
        LAB.A := EE;
        LAB.B := DD;
        LABDict.Add(ImgArr[Y][X], LAB);
      end;

      //Within chroma and hue-levels? (142 = tolmax)
      if ((Sqr(LAB.A - A) + Sqr(LAB.B - B)) <= ColorTol) then
        //Within Lightness tolerance? (100 = tolmax)
        if (Abs(L-LAB.L) <= LightTol) then
          TPS.Append(X,Y);
    end;

  LABDict.Destroy;
  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


end.
