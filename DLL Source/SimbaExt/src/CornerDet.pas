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
  CoreTypes;

function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist: Integer): TPointArray;


//-----------------------------------------------------------------------
implementation
uses
  Windows, SysUtils, Imaging, PointList, MatrixMath, MatrixTools, PointTools, Math;


(*=============================================================================|
 Convert Matrix to byte values (0-255) - values represents pircel intensity
|=============================================================================*)
function Intesity(const Mat:T2DIntArray): T2DIntArray;
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
      //Result[y][x] := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
      Result[y][x] := Round((0.299 * (Color and $FF)) + 
                            (0.587 * ((Color shr 8) and $FF)) + 
                            (0.114 * ((Color shr 16) and $FF)));
    end;
end;


(*=============================================================================|
 2D Convolution
|=============================================================================*)
function SobelConvolve(const Source:T2DIntArray; Mask:T2DIntArray): T2DIntArray;
var
  W,H,x,y: Integer;
begin
  W := High(source[0]);
  H := High(source);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    Move(Source[y][0],Result[y][0], (W+1)*SizeOf(Int32));

  W := W-1;
  H := H-1;
  for y:=1 to H do
    for x:=1 to W do
      Result[y,x] := (
          (mask[0][0] * Source[y-1][x-1]) + (mask[1][0] * Source[y-1][x+0]) + (mask[2][0] * Source[y-1][x+1]) +
          (mask[0][1] * Source[y][x-1])   + (mask[1][1] * Source[y][x])     + (mask[2][1] * Source[y][x+1])   +
          (mask[0][2] * Source[y+1][x-1]) + (mask[1][2] * Source[y+1][x+0]) + (mask[2][2] * Source[y+1][x+1])
      );
end;


(*=============================================================================|
 Sobel operator
|=============================================================================*)
function Sobel(const Mat:T2DIntArray; Axis:Char): T2DIntArray;
var
  xmask, ymask: T2DIntArray;
begin
  SetLength(xmask, 3,3);
  xmask[0,0] := -1; xmask[0,1] := 0; xmask[0,2] := 1;
  xmask[1,0] := -2; xmask[1,1] := 0; xmask[1,2] := 2;
  xmask[2,0] := -1; xmask[2,1] := 0; xmask[2,2] := 1;
  
  SetLength(ymask, 3,3);
  ymask[0,0] := -1; ymask[0,1] := -2; ymask[0,2] := -1;
  ymask[1,0] :=  0; ymask[1,1] :=  0; ymask[1,2] := 0;
  ymask[2,0] :=  1; ymask[2,1] :=  2; ymask[2,2] := 1;

  if axis = 'y' then Result := SobelConvolve(Mat,ymask);
  if axis = 'x' then Result := SobelConvolve(Mat,xmask);
end; 


(*=============================================================================|
 Gassuian blur and related functions
|=============================================================================*)
function GaussianBlur(ImArr:T2DIntArray; Radius:Int32; Sigma:Single): T2DIntArray;
var
  x,y,wid,hei,xx,yy,offset,block:Int32;
  gauss:Single;
  tmp:T2DFloatArray;
  kernel:TFloatArray;

begin
  block := Radius*2;
  Wid := High(ImArr[0]);
  Hei := High(ImArr);
  SetLength(Result, hei+1,wid+1);
  SetLength(tmp, hei+1,wid+1);

  kernel := GaussKernel1D(Radius, Sigma); // Compute our gaussian 1d kernel

  // y direction
  for y:=0 to hei do
    for x:=0 to wid do
    begin
      gauss := 0.0;
      for offset:=0 to block do
      begin
        xx := (x-Radius)+offset;
        if (xx < 0) then xx := 0 else if (xx > wid) then xx := wid;
        gauss += ImArr[y, xx] * kernel[offset];
      end;
      tmp[y,x] := gauss;
    end;

  // x direction
  for y:=0 to hei do
    for x:=0 to wid do
    begin
      gauss := 0.0;
      for offset:=0 to block do
      begin
        yy := (y-Radius)+offset;
        if (yy < 0) then yy := 0 else if (yy > hei) then yy := hei;
        gauss += tmp[yy, x] * kernel[offset];
      end;
      Result[y,x] := Round(gauss);
    end;
end;  


function BoxBlur3(Mat:T2DIntArray): T2DFloatArray;
var W,H,x,y:Int32;
begin
  W := High(Mat[0]);
  H := High(Mat);
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    Move(Mat[y][0],Result[y][0], (W+1)*SizeOf(Int32));
  Dec(W); Dec(H);
  for y:=1 to H do
    for x:=1 to W do
      Result[y][x] := (
               Mat[y-1][x]   + Mat[y+1][x]   + Mat[y][x-1] +
               Mat[y][x+1]   + Mat[y-1][x-1] + Mat[y+1][x+1] +
               Mat[y-1][x+1] + Mat[y+1][x-1] + Mat[y][x]) div 9;
end;


(*=============================================================================|
 Computing harris response of grayscale (0-255) matrix.
|=============================================================================*)
function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
var
  blur,imx,imy:T2DIntArray;
  wxx,wyy,wxy: T2DFloatArray;
begin
  blur := GaussianBlur(Intesity(Mat), KSize, GaussDev);
  imx := Sobel(blur, 'x');
  imy := Sobel(blur, 'y');

  (*Wxx := ToSingle(GaussianBlur(imx*imx, 3.0, 1));
    Wyy := ToSingle(GaussianBlur(imy*imy, 3.0, 1));
    Wxy := ToSingle(GaussianBlur(imx*imy, 3.0, 1));*)
  Wxx := BoxBlur3(imx*imx);
  Wyy := BoxBlur3(imy*imy);
  Wxy := BoxBlur3(imx*imy);

  Result := ((Wxx*Wyy) - (Wxy*Wxy)) / (Wxx+Wyy);
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
        Result[k] := CoreTypes.Point(x, y);
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
    Result[x] := TPACenter(ATPA[x], CA_MEAN, False);
  TPL.Free();
end;


end.
