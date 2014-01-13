unit XT_Corners;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses
  SysUtils, Math, XT_TPointList, XT_Types;

function CornerResponse(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer): T2DExtArray;
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer; Thresh:Extended; MinDist:Integer): TPointArray;
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer; Thresh:Extended; MinDist: Integer): TPointArray;


//-----------------------------------------------------------------------
implementation
uses
  XT_Points;

(*=============================================================================|
 Matrix operations (shortcut functions)
|=============================================================================*)
function MatToExtended(const mat:T2DIntArray): T2DExtArray;
var W,H,x,y:Integer;
begin
  W := high(mat[0]);
  H := high(mat);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := mat[y][x];
end;


function _IMul(const a,b:T2DIntArray): T2DIntArray;
var W,H,x,y:Integer;
begin
  W := high(a[0]);
  H := high(a);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := (a[y][x] * b[y][x]);
end;


function _Mul(const a,b:T2DExtArray): T2DExtArray;
var W,H,x,y:Integer;
begin
  W := high(a[0]);
  H := high(a);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := (a[y][x] * b[y][x]);
end;


function _Div(const a, b:T2DExtArray): T2DExtArray;
var W,H,x,y:Integer;
begin
  W := high(a[0]);
  H := high(a);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
    begin
      if b[y][x] = 0 then
        Result[y][x] := 0
      else
        Result[y][x] := (a[y][x] / b[y][x]);
    end;
end;


function _Add(const a,b:T2DExtArray): T2DExtArray;
var W,H,x,y:Integer;
begin
  W := high(a[0]);
  H := high(a);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := (a[y][x] + b[y][x]);
end;


function _Sub(const a,b:T2DExtArray): T2DExtArray;
var W,H,x,y:Integer;
begin
  W := high(a[0]);
  H := high(a);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := (a[y][x] - b[y][x]);
end;


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
function Convolve(const Source:T2DIntArray; Mask:T2DExtArray): T2DIntArray;
var
  W,H,x,y,yy,xx: Integer;
  mW,mH,mid:Integer;
  val: Extended;
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
  xmask, ymask: T2DExtArray;
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
function GaussKernel(KernelRadius:Integer; Sigma:Extended): T2DExtArray;
var
  hkernel:TExtArray;
  Size,i,x,y:Integer;
  sum:Extended;
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

function GaussianBlur(Mat:T2DIntArray; Sigma:Extended; KernelSize:Integer): T2DIntArray;
begin
  Result := Convolve(Mat, GaussKernel(KernelSize, Sigma));
end;


(*=============================================================================|
 Computing harris response of grayscale (0-255) matrix.
|=============================================================================*)
function CornerResponse(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer): T2DExtArray;
var
  blur,imx,imy:T2DIntArray;
  wxx,wyy,wxy:T2DExtArray;
begin
  blur := GaussianBlur(GrayScale(Mat), GaussDev, KSize);
  imx := Sobel(blur, 'x');
  imy := Sobel(blur, 'y');

  Wxx := MatToExtended(GaussianBlur(_imul(imx,imx), 3.0, 1));
  Wyy := MatToExtended(GaussianBlur(_imul(imy,imy), 3.0, 1));
  Wxy := MatToExtended(GaussianBlur(_imul(imx,imy), 3.0, 1));

  Result := _div(_sub(_mul(Wxx,Wyy), _mul(Wxy,Wxy)), _add(Wxx,Wyy));
end; 


(*=============================================================================|
 Peak extraction (Dirty)...
|=============================================================================*)
function LocalPeaks(const Mat:T2DExtArray; Footprint:Integer; Thresh:Extended): TPointArray;
var
  W,H,x,y,xx,yy,yl,xl,step:Integer;
  vTresh, vMax, lMax:Extended;
  TPL:TPointList;
begin
  W := High(Mat[0]);
  H := High(Mat);

  //Find highest peak
  vMax := 0.0;
  for y:=0 to H do
    for x:=0 to W do
      if Mat[y][x] > vMax then
        vMax := Mat[y][x];
  vTresh := vMax * Thresh;

  TPL.Init();
  Step := Footprint;
  Dec(Footprint);
  y := 0;
  while (y < H) do
  begin
    x := 0;
    yl := Min(y+Footprint,H);
    while (x < W) do
    begin
      xl := Min(x+Footprint, W);
      //Find local peak
      lMax := 0.0;
      for yy:=y to yl do
        for xx:=x to xl do
          if (Mat[yy][xx] > lMax) then
            lMax := Mat[yy][xx];
      //Append all top spikes to result.
      if lMax > vTresh then
        for yy:=y to yl do
          for xx:=x to xl do
            if (Mat[yy][xx] >= lMax) then
              TPL.AppendXY(xx,yy);
      x += Step;
    end;
    y += Step;
  end;

  Result := TPL.Clone();
  TPL.Free();
end;


(*=============================================================================|
 Tries to extract the peak points of the neighborhood covering MinDist.
|=============================================================================*)
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer; Thresh:Extended; MinDist:Integer): TPointArray;
var Mat2:T2DExtArray;
begin
  MinDist := Max(1,MinDist);
  Mat2 := CornerResponse(Mat, GaussDev, KSize);
  Result := LocalPeaks(Mat2, MinDist, Thresh);
end;



(*=============================================================================|
 Clusters each group of points and returns the mean point of that group.
|=============================================================================*)
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Extended; KSize:Integer; Thresh:Extended; MinDist: Integer): TPointArray;
var
  W,H,x,y:Integer;
  aMax,aTresh:Extended;
  Mat2:T2DExtArray;
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
        TPL.appendXY(x,y);

  ATPA := ClusterTPA(TPL.Clone(), MinDist, True);
  SetLength(Result, Length(ATPA));
  for x:=0 to High(ATPA) do
    Result[x] := TPACenter(ATPA[x], cm_mean, False);
  TPL.Free();
end;


end.
