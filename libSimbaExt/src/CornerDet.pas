unit CornerDet;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 #fixme: CornerRespons is not normalized
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses
  CoreTypes, SysUtils;

function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;
function FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist: Integer): TPointArray;


//-----------------------------------------------------------------------
implementation
uses
  Imaging, PointList, MatrixMath, MatrixOps, PointTools, Math, TimeUtils, ThreadPool;


(*=============================================================================|
 Convert colored image to an image who represents color-intensity (0..255)
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
      color := Mat[y,x];
      //Result[y][x] := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
      Result[y,x] := Round((0.299 * (Color and $FF)) +
                           (0.587 * ((Color shr 8) and $FF)) +
                           (0.114 * ((Color shr 16) and $FF)));
    end;
end;


(*=============================================================================|
 2D Convolution
|=============================================================================*)
procedure SobelConvolve_Thread(Params:PParamArray);
var
  x,y,wid,hei:Int32;
  dest,data,mask:P2DIntArray;
  b:TBox;
begin
  data := Params^[0];
  mask := Params^[1];
  dest := Params^[2];
  b := PBox(Params^[3])^;
  hei := High(data^);
  wid := High(data^[0]);
  if b.x1 = 0 then b.x1+=1;
  if b.y1 = 0 then b.y1+=1;
  if b.x2 >= wid then b.x2 := wid-1;
  if b.y2 >= hei then b.y2 := hei-1;

  for y:=b.y1 to b.y2 do
    for x:=b.x1 to b.x2 do
      dest^[y,x] := (
        (mask^[0][0] * data^[y-1][x-1]) + (mask^[0][1] * data^[y-1][x+0]) + (mask^[0][2] * data^[y-1][x+1]) +
        (mask^[1][0] * data^[y+0][x-1]) + (mask^[1][1] * data^[y+0][x+0]) + (mask^[1][2] * data^[y+0][x+1]) +
        (mask^[2][0] * data^[y+1][x-1]) + (mask^[2][1] * data^[y+1][x+0]) + (mask^[2][2] * data^[y+1][x+1])
      );
end;


(*=============================================================================|
 Sobel operator
|=============================================================================*)
function Sobel(const Mat:T2DIntArray; Axis:Char): T2DIntArray;
var
  mask: T2DIntArray;
  W,H,x,y: Int32;
begin
  SetLength(mask, 3,3);
  if Axis = 'z' then
  begin
    mask[0,0] := -1; mask[0,1] := 0; mask[0,2] := 1;
    mask[1,0] := -2; mask[1,1] := 0; mask[1,2] := 2;
    mask[2,0] := -1; mask[2,1] := 0; mask[2,2] := 1;
  end else begin
    mask[0,0] := -1; mask[0,1] := -2; mask[0,2] := -1;
    mask[1,0] :=  0; mask[1,1] :=  0; mask[1,2] := 0;
    mask[2,0] :=  1; mask[2,1] :=  2; mask[2,2] := 1;
  end;

  W := Length(mat[0]);
  H := Length(mat);
  SetLength(Result, H,W);
  for y:=0 to H-1 do
  begin
    Result[y][0] := mat[y][0];
    Result[y][W-1] := mat[y][W-1];
  end;
  for x:=0 to W-1 do
  begin
    Result[0][x] := mat[0][x];
    Result[H-1][x] := mat[H-1][x];
  end;

  ThreadPool.MatrixFunc(@SobelConvolve_Thread, [@mat, @mask, @result], W,H);
end; 


(*=============================================================================|
 Gaussian blur
|=============================================================================*)
procedure Gaussian_Thread_Y(params:PParamArray);
var
  ofs,xx,x,y,wid,dia,rad:Int32;
  data:P2DIntArray;
  dest:P2DFloatArray;
  mask:PFloatArray;
  gauss:Single;
  b:TBox;
begin
  data := Params^[0];
  mask := Params^[1];
  dest := Params^[2];
  dia := PInt32(Params^[3])^;
  rad := PInt32(Params^[4])^;
  b := PBox(Params^[5])^;
  wid := High(data^[0]);

  for y:=b.y1 to b.y2 do
    for x:=b.x1 to b.x2 do
    begin
      gauss := 0.0;
      for ofs:=0 to dia do
      begin
        xx := (x-rad)+ofs;
        if (xx < 0) then xx := 0 else if (xx > wid) then xx := wid;
        gauss += data^[y, xx] * mask^[ofs];
      end;
      dest^[y,x] := gauss;
    end;
end;


procedure Gaussian_Thread_X(params:PParamArray);
var
  ofs,yy,x,y,hei,dia,rad:Int32;
  data:P2DFloatArray;
  dest:P2DIntArray;
  mask:PFloatArray;
  gauss:Single;
  b:TBox;
begin
  data := Params^[0];
  mask := Params^[1];
  dest := Params^[2];
  dia := PInt32(Params^[3])^;
  rad := PInt32(Params^[4])^;
  b := PBox(Params^[5])^;
  hei := High(data^);

  for y:=b.y1 to b.y2 do
    for x:=b.x1 to b.x2 do
    begin
      gauss := 0.0;
      for ofs:=0 to dia do
      begin
        yy := (y-rad)+ofs;
        if (yy < 0) then yy := 0 else if (yy > hei) then yy := hei;
        gauss += data^[yy, x] * mask^[ofs];
      end;
      dest^[y,x] := round(gauss);
    end;
end;

function GaussianBlur(ImArr:T2DIntArray; Radius:Int32; Sigma:Single): T2DIntArray;
var
  wid,hei,dia:Int32;
  tmpRes:T2DFloatArray;
  kernel:TFloatArray;
begin
  dia := radius*2;
  wid := Length(ImArr[0]);
  hei := Length(ImArr);
  SetLength(Result, hei,wid);
  SetLength(tmpRes, hei,wid);
  kernel := GaussKernel1D(Radius, Sigma);

  ThreadPool.MatrixFunc(@Gaussian_Thread_Y, [@imarr,  @kernel, @tmpRes, @dia, @radius], wid,hei);
  ThreadPool.MatrixFunc(@Gaussian_Thread_X, [@tmpRes, @kernel, @result, @dia, @radius], wid,hei);
end;  


(*=============================================================================|
 Box blur
|=============================================================================*)
procedure BoxBlur3_Thread(Params:PParamArray);
var
  x,y,wid,hei:Int32;
  data:P2DIntArray;
  dest:P2DFloatArray;
  b:TBox;
begin
  data := Params^[0];
  dest := Params^[1];
  b := PBox(Params^[2])^;
  hei := High(data^);
  wid := High(data^[0]);
  if b.x1 = 0 then b.x1+=1;
  if b.y1 = 0 then b.y1+=1;
  if b.x2 >= wid then b.x2 := wid-1;
  if b.y2 >= hei then b.y2 := hei-1;

  for y:=b.y1 to b.y2 do
    for x:=b.x1 to b.x2 do
      dest^[y][x] := (
          data^[y-1][x]   + data^[y+1][x]   + data^[y][x-1] +
          data^[y][x+1]   + data^[y-1][x-1] + data^[y+1][x+1] +
          data^[y-1][x+1] + data^[y+1][x-1] + data^[y][x]
      ) div 9;
end;

function BoxBlur3(mat:T2DIntArray): T2DFloatArray;
var W,H:Int32;
begin
  W := Length(mat[0]);
  H := Length(mat);
  SetLength(result, H,W);
  ThreadPool.MatrixFunc(@BoxBlur3_Thread, [@mat, @result], W,H);
end;


(*=============================================================================|
 Computing harris response of greyscale (0-255) matrix.
|=============================================================================*)
function CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;
var
  blur,imx,imy:T2DIntArray;
  wxx,wyy,wxy: T2DFloatArray;
begin
  blur := GaussianBlur(Intesity(Mat), KSize, GaussDev); //45+15ms

  imx := Sobel(blur, 'x'); //25ms
  imy := Sobel(blur, 'y'); //..

  Wxx := BoxBlur3(imx*imx);  //25ms
  Wyy := BoxBlur3(imy*imy);  //..
  Wxy := BoxBlur3(imx*imy);  //..

  Result := ((Wxx*Wyy) - (Wxy*Wxy)) / (Wxx+Wyy);   //45ms
end; 


(*=============================================================================|
 Tries to extract the peak points of the neighborhood covering MinDist.
|=============================================================================*)
function FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;
var
  w,h,i,j,x,y,k: Int32;
  CurrentMax: Single;
  Response: T2DFloatArray;
begin
  Response := CornerResponse(Mat, GaussDev, KSize);

  W := High(Response[0]);
  H := High(Response);
  SetLength(Result, (W+1)*(H+1));
  Response := Normalize(Response, 0, 1.0);
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
  W,H,x,y:Int32;
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

  ATPA := ClusterTPA(TPL.Finalize, MinDist, True);
  SetLength(Result, Length(ATPA));
  for x:=0 to High(ATPA) do
    Result[x] := TPACenter(ATPA[x], ECA_MEAN);
  TPL.Free();
end;


end.
