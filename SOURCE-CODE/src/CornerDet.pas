unit CornerDet;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses
  SysUtils, Math, CoreTypes;

function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist: Integer): TPointArray;


//-----------------------------------------------------------------------
implementation
uses
  PointList, MatrixMath, MatrixTools, PointTools;


(*=============================================================================|
 Convert Matrix to byte values (0-255) aka gray-scale
|=============================================================================*)
function GrayScale(const Mat:T2DIntArray): T2DIntArray;
var
  x,y,W,H,color:Integer;
begin
  W := High(Mat[0]);
  H := High(Mat);
  SetLength(Result, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
    begin
      color := Mat[y][x];
      Result[y][x] := Trunc((0.299 * (Color and $FF)) +
                            (0.587 * ((Color shr 8) and $FF)) +
                            (0.114 * ((Color shr 16) and $FF)));
    end;
end;


(*=============================================================================|
 2D Convolution.
 I can't find any way to speed it up :|
|=============================================================================*)
function Convolve(const Source:T2DIntArray; Mask:T2DFloatArray): T2DIntArray;
var
  W,H,x,y,yy,xx: Integer;
  mW,mH,mid:Integer;
  val: Single;
begin
  W := High(source[0]);
  H := High(source);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    Result[y] := Copy(Source[y], 0, W+1);

  mW := High(mask[0]);
  mH := High(mask);
  mid := (mW+1) div 2;
  W := W-mid;
  H := H-mid;
  for y:=mid to H do
  for x:=mid to W do
  begin
    val := 0;
    for yy:=0 to mH do
      for xx:=0 to mW do
        val += (mask[yy][xx] * Source[y + yy - mid][x + xx - mid]);
    Result[y][x] := Trunc(val);
  end;
end;


(*=============================================================================|
 Sobel operator
|=============================================================================*)
function Sobel(const Mat:T2DIntArray; Axis:Char): T2DIntArray;
var
  xmask, ymask: T2DFloatArray;
begin
  SetLength(xmask, 3,3);
  xmask[0][0] := -1; xmask[0][1] := 0; xmask[0][2] := 1;
  xmask[1][0] := -2; xmask[1][1] := 0; xmask[1][2] := 2;
  xmask[2][0] := -1; xmask[2][1] := 0; xmask[2][2] := 1;
  
  SetLength(ymask, 3,3);
  ymask[0][0] := -1; ymask[0][1] := -2; ymask[0][2] := -1;
  ymask[1][0] :=  0; ymask[1][1] :=  0; ymask[1][2] := 0;
  ymask[2][0] :=  1; ymask[2][1] :=  2; ymask[2][2] := 1;

  if axis = 'y' then Result := convolve(Mat,ymask);
  if axis = 'x' then Result := convolve(Mat,xmask);
end; 


(*=============================================================================|
 Gassuian blur and related functions
|=============================================================================*)
function GaussKernel(KernelRadius:Integer; Sigma:Single): T2DFloatArray;
var
  hkernel:TExtArray;
  Size,i,x,y:Integer;
  sum:Single;
begin
  Size := 2*KernelRadius+1;
  SetLength(hkernel, Size);
  for i:=0 to Size-1 do
    hkernel[i] := Exp(-(Sqr((i-KernelRadius) / Sigma)) / 2.0);

  SetLength(Result, Size, Size);
  sum:=0;
  for y:=0 to Size-1 do
    for x:=0 to Size-1 do
    begin
      Result[y][x] := hkernel[x]*hkernel[y];
      Sum := Sum + Result[y][x];
    end;

  for y := 0 to Size-1 do
    for x := 0 to Size-1 do
      Result[y][x] := Result[y][x] / sum;
end; 

function GaussianBlur(Mat:T2DIntArray; Sigma:Single; KernelSize:Integer): T2DIntArray;
begin
  Result := Convolve(Mat, GaussKernel(KernelSize, Sigma));
end;


(*=============================================================================|
 Computing harris response of grayscale (0-255) matrix.
|=============================================================================*)
function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
var
  blur,imx,imy:T2DIntArray;
  wxx,wyy,wxy:T2DFloatArray;
begin
  blur := GaussianBlur(GrayScale(Mat), GaussDev, KSize);
  imx := Sobel(blur, 'x');
  imy := Sobel(blur, 'y');

  //Wxx := ToFloat(GaussianBlur(_imul(imx,imx), 3.0, 1));
  //Wyy := ToFloat(GaussianBlur(_imul(imy,imy), 3.0, 1));
  //Wxy := ToFloat(GaussianBlur(_imul(imx,imy), 3.0, 1));
  
  Wxx := ToFloat(GaussianBlur(imx * imx, 3.0, 1));
  Wyy := ToFloat(GaussianBlur(imy * imy, 3.0, 1));
  Wxy := ToFloat(GaussianBlur(imx * imy, 3.0, 1));
  
  Result := ((Wxx * Wyy) - Sqr(Wxy)) / (Wxx + Wyy);
  //Result := _div(_sub(_mul(Wxx,Wyy), _mul(Wxy,Wxy)), _add(Wxx,Wyy));
end; 


(*=============================================================================|
 Tries to extract the peak points of the neighborhood covering MinDist.
|=============================================================================*)
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;
var
  w,h,i,j,x,y,k: Integer;
  CurrentMax: Single;
  Response: T2DFloatArray;
begin
  Response := CornerResponse(Mat, GaussDev, KSize);

  W := High(Response[0]);
  H := High(Response);
  SetLength(Result, (W+1)*(H+1));
  Response := NormalizeMat(Response, 0, 1.0);
  k := 0;
  for y:=footprint to H-footprint do
    for x:=footprint to W-footprint do
    begin
      CurrentMax := Response[y][x];
      for i :=-footprint to footprint do
        for j:=-footprint to footprint do
          if (Response[y + i][x + j] > currentMax) then
          begin
            CurrentMax := 0;
            Break;
          end;

      if (CurrentMax > Thresh) then
      begin
        Result[k] := Point(x, y);
        Inc(k);
      end;
    end;
  SetLength(Result, k);
  SetLength(Response, 0);
end;



(*=============================================================================|
 Clusters each group of points and returns the mean point of that group.
|=============================================================================*)
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist: Integer): TPointArray;
var
  W,H,x,y:Integer;
  aMax,aTresh:Single;
  Mat2:T2DFloatArray;
  TPL:TPointList;
  ATPA:T2DPointArray;
begin
  Mat2 := CornerResponse(Mat, GaussDev, KSize);
  aMax := Mat2[0][0];
  W := High(Mat2[0]);
  H := High(Mat2);
  for y:=0 to H do
    for x:=0 to W do
      if Mat2[y][x] > aMax then
        aMax := Mat2[y][x];
  aTresh := aMax * Thresh;

  TPL.Init();
  for y:=0 to H do
    for x:=0 to W do
      if (Mat2[y][x] > aTresh) then
        TPL.Append(x,y);

  ATPA := ClusterTPA(TPL.Clone(), MinDist, True);
  SetLength(Result, Length(ATPA));
  for x:=0 to High(ATPA) do
    Result[x] := TPACenter(ATPA[x], cm_mean, False);
  TPL.Free();
end;


end.
