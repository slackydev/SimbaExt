Unit XT_Finder;
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
  XT_Types, Math, SysUtils;


function ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; Cdecl;
function ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntArray; Tol:Integer): Boolean; Cdecl;
function ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;
function ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;


//--------------------------------------------------
implementation

uses
  XT_HashTable, XT_ColorMath, XT_Math, XT_TPointList;

  
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
        TPS.AppendXY(X,Y);
    end;

  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


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
          TPS.AppendXY(X,Y);
          Break;
        end;
      end;
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
  LABDict: ColorDict;
  TPS: TPointList;
begin
  Result := True;

  W := High(ImgArr[0]);
  H := High(ImgArr);
  LABDict := ColorDict.Create((W+1)*(H+1));
  TPS.Init;
  ColorToLCH(Color, L,C,HH);
  LightTol := Sqr(LightTol);

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
          TPS.AppendXY(X,y);
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
  LABDict: ColorDict;
  TPS: TPointList;
begin
  Result := True;

  W := High(ImgArr[0]);
  H := High(ImgArr);
  LABDict := ColorDict.Create((W+1)*(H+1));
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
          TPS.AppendXY(X,Y);
    end;

  LABDict.Destroy;
  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


end.
