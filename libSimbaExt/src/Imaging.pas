Unit Imaging;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro on}
{$inline on}

interface
uses
  SysUtils, Math, CoreTypes, CoreMisc;


procedure ImGetRGB(const Img: T2DIntArray; out R,G,B:T2DIntArray);
function ImMergeRGB(const R,G,B:T2DIntArray): T2DIntArray;
procedure ImGrayscale(const ImgArr:T2DIntArray; var Dest:T2DIntArray);
function GaussKernel(KernelRadius:Int32; Sigma:Single): T2DFloatArray;
function GaussKernel1D(KernelRadius:Int32; Sigma:Single): TFloatArray;
procedure ImBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32);
function ImMedianBlur(const ImgArr: T2DIntArray; Radius:Integer): T2DIntArray; 
function ImBrighten(const ImgArr:T2DIntArray; Amount:Extended): T2DIntArray; 
procedure ImThreshold(var ImgArr:T2DIntArray; Threshold:Byte; Alpha, Beta:Int32);
procedure ImThresholdAdaptive(var ImgArr:T2DIntArray; Alpha, Beta: Int32; Method:EThreshAlgo; C:Int32);
function ImFindContours(const ImgArr:T2DIntArray; Threshold:Int32=127): T2DPointArray;
function ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray; 
function ImSobel(const ImgArr: T2DIntArray): T2DIntArray; 
function ImSobel(const ImgArr: T2DIntArray; Axis:Int8): T2DIntArray; overload;
function ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;
procedure ImGaussBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32; Sigma:Single);
function ImBlend(const Img1, Img2: T2DIntArray; Alpha:Single=0.5): T2DIntArray;
function ImCompareAt(large,small:T2DIntArray; pt:TPoint; tol:Int32): Single;
procedure ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:EResizeAlgo);
function ImSample(ImgArr:T2DIntArray; Scale:Int32): T2DIntArray;
function ImRotate(const Mat:T2DIntArray; Angle:Single; Expand:Boolean; BiLinear:Boolean=True): T2DIntArray;


  
function TestResizeBI(Src:T2DIntArray; NewW,NewH:int32): T2DIntArray; cdecl;

//--------------------------------------------------
implementation

uses
  PointTools, ColorMath, MatrixMath;
  
  
function GetMatrixSize(Mat:T2DIntArray; out W,H:Int32): Boolean; Inline;
begin
  H := Length(Mat);
  if H > 0 then W := Length(Mat[0]) else W := 0;
  Result := H > 0;
end;


function GetMatrixHigh(Mat:T2DIntArray; out W,H:Int32): Boolean; Inline;
begin
  H := High(Mat);
  if H > -1 then W := High(Mat[0]) else W := -1;
  Result := H > -1;
end;
  
  
procedure CheckResizeMatrix(var Mat:T2DIntArray; H,W:Int32); Inline;
begin
  if (Length(Mat) <> H) and (Length(Mat[0]) <> W) then
    SetLength(Mat,H,W);
end;

  
  
{*
 Converts the ImMatrix to separate arrays for each channel (R,G,B).
*} 
procedure ImGetRGB(const Img: T2DIntArray; out R,G,B:T2DIntArray);
var W,H,x,y:Int32;
begin
  if not(GetMatrixSize(Img, W,H)) then Exit();
  SetLength(R, H,W);
  SetLength(G, H,W);
  SetLength(B, H,W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do begin
      R[y][x] := Img[y][x] and $FF;
      G[y][x] := Img[y][x] shr 8 and $FF;
      B[y][x] := Img[y][x] shr 16 and $FF;
    end;
end;


{*
 Takes three channels (R,G,B), one for each color, merges them in to a Image-matrix.
*} 
function ImMergeRGB(const R,G,B:T2DIntArray): T2DIntArray;
var W,H,x,y:Int32;
begin
  if not(GetMatrixSize(R, W,H)) then Exit();
  if not(GetMatrixSize(G, W,H)) then Exit();
  if not(GetMatrixSize(B, W,H)) then Exit();
  SetLength(Result, H, W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
      Result[y][x] := R[y][x] or G[y][x] shl 8 or B[y][x] shl 16;
end;
  

{*
 Converts a colored image to grayscale (using only the first byte as representation)
*} 
procedure ImGrayscale(const ImgArr:T2DIntArray; var Dest:T2DIntArray);
var W,H,x,y:Int32;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();
  CheckResizeMatrix(Dest, H,W);
  dec(W);
  for y:=0 to H-1 do
    for x:=0 to W do
      Dest[y,x] := ColorIntensity(ImgArr[y,x]);
end;
  
  
{*
 Generates a 2D gaussian filter. 
*}
function GaussKernel(KernelRadius:Int32; Sigma:Single): T2DFloatArray;
var
  hkernel:TExtArray;
  Size,i,x,y:Integer;
  sum:Single;
begin
  Size := 2*KernelRadius;
  SetLength(hkernel, Size+1);
  for i:=0 to Size do
    hkernel[i] := Exp(-(Sqr((i-KernelRadius) / Sigma)) / 2.0);

  SetLength(Result, Size+1, Size+1);
  sum:=0;
  for y:=0 to Size do
    for x:=0 to Size do
    begin
      Result[y][x] := hkernel[x]*hkernel[y];
      Sum := Sum + Result[y][x];
    end;

  for y := 0 to Size do
    for x := 0 to Size do
      Result[y][x] := Result[y][x] / sum;
end;


{*
 Generates a 1D gaussian filter.
*}
function GaussKernel1D(KernelRadius:Int32; Sigma:Single): TFloatArray;
var sum:Single; i,size:Int32;
begin
  size := 2*KernelRadius;
  SetLength(Result, size+1);

  sum := 0.0;
  for i:=0 to size do begin
    Result[i] := Exp(-(Sqr((i-KernelRadius) / sigma)) / 2.0);
    sum += Result[i];
  end;
  WriteLn(Sum);
  for i:=0 to size do
    Result[i] /= sum;
end;


{*
 Returns a (box) blurred version of the Matrix/ImgArr.
 Block is the radius of the blur: 1,2,3,4...
*}
procedure ImBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32);
type TFRGB = record R,G,B:Single; end;
var
  x,y,wid,hei,xx,yy,offset,dia,width:Int32;
  ptr:^TFRGB; f:TFRGB;
  tmp:Array of TFRGB;
  kval:Single;
begin
  dia := Radius * 2 + 1;
  kval := 1.0 / dia;
  if not(GetMatrixHigh(ImgArr, Wid,Hei)) then Exit();
  width := wid + 1;

  if Hei <> High(Dest) then {if Dest is not pre-initalized then initalize it}
    SetLength(Dest, hei+1,wid+1);

  SetLength(tmp, (hei+1) * width);

  // y direction
  offset := 0;
  repeat
    ptr := @tmp[0];
    for y:=0 to hei do
      for x:=0 to wid do
      begin
        xx := (x-Radius)+offset;
        if (xx < 0) then xx := 0 else if (xx > wid) then xx := wid;
        ptr^.R += (ImgArr[y,xx] and $FF) * kval;
        ptr^.G += (ImgArr[y,xx] shr 8 and $FF) * kval;
        ptr^.B += (ImgArr[y,xx] shr 16 and $FF) * kval;
        inc(ptr);
      end;
    inc(offset);
  until offset = dia;

  // x direction + result
  for y:=0 to hei do
    for x:=0 to wid do
    begin
      f.R := 0; f.G := 0; f.B := 0;
      ptr := @f;
      offset := 0;
      repeat
        yy := (y-Radius)+offset;
        if (yy < 0) then yy := 0 else if (yy > hei) then yy := hei;
        ptr^.R += tmp[yy*width+x].R * kval;
        ptr^.G += tmp[yy*width+x].G * kval;
        ptr^.B += tmp[yy*width+x].B * kval;
        inc(offset);
      until offset = dia;
      Dest[y,x] := (Round(ptr^.R)) or (Round(ptr^.G) shl 8) or (Round(ptr^.B) shl 16);
    end;
end;


{*
 Filter a matrix/ImgArr with a Median Filter.
 Block is the radius of the filter, 1,2,3,4...
*}
{** __PRIVATE__ **}
procedure __SortRGB(var Arr, Weight: TIntArray); Inline;
var CurIdx, TmpIdx, Hi: Integer;
begin
  Hi := High(Arr);
  for CurIdx := 1 to Hi do
    for TmpIdx := CurIdx downto 1 do
    begin
      if not (Weight[TmpIdx] < Weight[TmpIdx - 1]) then
        Break;
      Exch(Arr[TmpIdx], Arr[TmpIdx - 1]);
      Exch(Weight[TmpIdx], Weight[TmpIdx - 1]);
    end;
end;

function ImMedianBlur(const ImgArr: T2DIntArray; Radius:Integer):T2DIntArray; 
var
  W,H,j,x,y,fx,fy,low,mid,size,color,block:Integer;
  lx,ly,hx,hy:Integer;
  Filter,Colors:TIntArray;
begin
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();
  Block := Radius*2+1;
  Size := Block * Block;
  if (Size<=1) or (Block mod 2 = 0) then Exit;
  SetLength(Result, H+1,W+1);
  SetLength(Filter, Size+1);
  SetLength(Colors, Size+1);
  low := Block div 2;
  mid := Size div 2;
  
  for y:=0 to H do
  begin
    ly := Max(0,y-low);
    hy := Min(H,y+low);
    for x:=0 to W do
    begin
      j := 0;
      lx := Max(0,x-low);
      hx := Min(W,x+low);
      for fy:=ly to hy do
        for fx:=lx to hx do
        begin
          Color := ImgArr[fy][fx];
          Filter[j] := ColorToGray(Color);
          Colors[j] := Color;
          Inc(j);
        end;
      __SortRGB(Colors, Filter);
      Result[y][x] := Colors[mid];
    end;
  end;
  SetLength(Colors, 0);
  SetLength(Filter, 0);
end;


{*
 Brightens the image or darkens if negative "amount" is given.
*}
function ImBrighten(const ImgArr:T2DIntArray; Amount:Extended): T2DIntArray; 
var
  W,H,x,y,R,G,B,AmountL:Integer;
  Color:ColorRGB;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();
  SetLength(Result, H,W);

  Dec(W); 
  Dec(H);
  AmountL := Round(Amount);
  for y:=0 to H do
    for x:=0 to W do
    begin
      Color := ColorToRGB(ImgArr[y][x]);
      R := Color.R + AmountL;
      if (R > 255) then R:=255
      else if (R < 0) then R:=0;

      G := Color.G + AmountL;
      if (G > 255) then G:=255
      else if (G < 0) then G:=0;

      B := Color.B + AmountL;
      if (B > 255) then B:=255
      else if (B < 0) then B:=0;
      Result[y][x] := (R) or (G shl 8) or (B shl 16);
    end;
end;


{*
 Given a threshold this function checks all the colors, and them who goes bellow `Threshold` will be set to `Alpha`
 the colors above or equal to the threshold will be set to `Beta`.
 @params:
    Threshold: Threshold value.
    Alpha: Minvalue for result
    Beta: Maxvalue for result
    Invert: Bellow Mean is set to Beta, rather then Alpha.
*}
procedure ImThreshold(var ImgArr:T2DIntArray; Threshold:Byte; Alpha, Beta: Int32);
var
  W,H,x,y:Integer;
begin
  if Alpha = Beta then Exit;
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();

  for y:=0 to H do
    for x:=0 to W do
    begin
      if ColorIntensity(ImgArr[y,x]) < Threshold then
        ImgArr[y][x] := Alpha
      else 
        ImgArr[y][x] := Beta;
    end;
end;


{*
 This method first finds the Mean of the image, and set the threshold to it. Again: colors bellow the Threshold will be set to `Alpha`
 the colors above or equal to the Mean/Threshold will be set to `Beta`.
 @todo: Test to use a matrix filter to reduce noice of size: 3x3, 5x5, 7x7 etc.. ( What did I mean by that? :P )
 @params:
    Alpha: Minvalue for result
    Beta: Maxvalue for result
    Invert: Bellow Mean is set to Beta, rather then Alpha.
    Method: TM_Mean or TM_MinMax
    C: Substract or add to the mean.
*}
procedure ImThresholdAdaptive(var ImgArr:T2DIntArray; Alpha, Beta: Int32; Method:EThreshAlgo; C:Int32);
var
  W,H,x,y:Int32;
  Color,IMin,IMax: Byte;
  Threshold,Counter: Int64;
begin
  if Alpha = Beta then Exit;
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();

  //Finding the threshold - While at it convert image to grayscale.
  Threshold := 0;
  Case Method of
    //Find the Arithmetic Mean / Average.
    ETA_MEAN:
    begin
      for y:=0 to H do
      begin
        Counter := 0;
        for x:=0 to W do
        begin
          Color := ColorIntensity(ImgArr[y,x]);
          ImgArr[y,x] := Color;
          Counter += Color;
        end;
        Threshold += (Counter div (W+1));
      end;
      Threshold := (Threshold div (H+1)) + C;
    end;

    //Middle of Min- and Max-value
    ETA_MINMAX:
    begin
      IMin := ColorIntensity(ImgArr[0,0]);
      IMax := IMin;
      for y:=0 to H do
        for x:=0 to W do
        begin
          Color := ColorIntensity(ImgArr[y,x]);
          ImgArr[y,x] := Color;
          if Color < IMin then
            IMin := Color
          else if Color > IMax then
            IMax := Color;
        end;
      Threshold := ((IMax+IMin) shr 1) + C;
    end;
  end;

  for y:=0 to H do
    for x:=0 to W do
    begin
      if ImgArr[y,x] < Threshold then
        ImgArr[y,x] := Alpha
      else 
        ImgArr[y,x] := Beta;
    end;
end;


{
  Extracts the contours of the image
  Fix me:
    It's prob better to reduce the number of colors to say 256 (non-adaptive) colors,
    instead of the current where we reduce it to a binary image (2 colors).
    Write an algorithm able to reduce the colors in an image.
}
function ImFindContours(const ImgArr:T2DIntArray; Threshold:Int32=127): T2DPointArray;
var
  W,H,j,i,x,y:Int32;
  TPA:TPointArray;
  Mat:T2DIntArray;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
  SetLength(Mat, H);
  for i:=0 to H-1 do
    Mat[i] := Copy(ImgArr[i],0,W);
  ImThreshold(Mat, Threshold, 255, 0);

  SetLength(TPA, W*H);
  j := 0;
  for y:=1 to H-2 do
    for x:=1 to W-2 do
      if Mat[y,x] = 255 then
      begin
        TPA[j] := Point(x,y);
        Inc(j);
      end;

  SetLength(TPA, j);
  TPA := TPAEdges(TPA);
  Result := ClusterTPA(TPA, 1, True);
  //trace each contour so that the result is ordered.
  for i:=0 to High(Result) do
    Result[i] := TPAOutline(Result[i]);
end; 



{
  Given a matrix that represents an image this algorithm extacts the contrast edge points.
  The result is an Array of TPoint (TPointArray).
  Uses RGB and R,G and B are weighted equally.
}
function ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray; 
var
  X,Y,Width,Height,Len,QSize: Integer;
  C1,C2:ColorRGB;
  Hit:Boolean;
begin
  if not(GetMatrixHigh(ImgArr, Width,Height)) then Exit();
  MinDiff := Sqr(MinDiff) * 3;
  QSize := Min(1000, Width*Height);
  SetLength(Result, QSize+1);
  
  Len := 0;
  for Y:=0 to Height do 
    for X:=0 to Width do
    begin
      Hit := False;
      C1 := ColorToRGB(ImgArr[Y][X]);
      if ((X+1) < Width) then
      begin
        C2 := ColorToRGB(ImgArr[Y][X+1]);
        if Sqr(C1.R-C2.R)+Sqr(C1.G-C2.G)+Sqr(C1.B-C2.B) >= MinDiff then
          Hit := True;
      end;

      if ((Y+1) < Height) and Not(Hit) then 
      begin
        C2 := ColorToRGB(ImgArr[Y+1][X]);
        if Sqr(C1.R-C2.R)+Sqr(C1.G-C2.G)+Sqr(C1.B-C2.B) >= MinDiff then
          Hit := True;
      end;
      
      if Hit then
      begin
        Result[Len] := Point(X,Y);
        Inc(Len);
        if QSize<=Len then
        begin
          QSize := QSize+QSize;
          SetLength(Result, QSize+1);
        end;
        Continue;
      end;
    end;

  SetLength(Result, Len);
end;




{*
 Applies a sobel overator on the image, and returns it (in gray scale).
*}
function ImSobel(const ImgArr: T2DIntArray): T2DIntArray;
var
  x,y,xx,yy,W,H,color,gx,gy:Int32;
  opx,opy: T2DIntArray;
  Gray:T2DByteArray;
begin
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();

  SetLength(Gray, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
      Gray[y,x] := ColorToGray( ImgArr[y,x] );

  SetLength(opx, 3,3);
  opx[0][0] := -1; opx[0][1] := 0; opx[0][2] := 1;
  opx[1][0] := -2; opx[1][1] := 0; opx[1][2] := 2;
  opx[2][0] := -1; opx[2][1] := 0; opx[2][2] := 1;

  SetLength(opy, 3,3);
  opy[0][0] := -1; opy[0][1] := -2; opy[0][2] := -1;
  opy[1][0] :=  0; opy[1][1] :=  0; opy[1][2] := 0;
  opy[2][0] :=  1; opy[2][1] :=  2; opy[2][2] := 1;

  SetLength(Result, H+1,W+1);
  W := W-1;
  H := H-1;
  for y:=1 to H do
    for x:=1 to W do
    begin
      gx := 0;
      gy := 0;
      for yy:=0 to 2 do
        for xx:=0 to 2 do
        begin
          gx := gx + (opx[yy][xx] * Gray[y + yy - 1][x + xx - 1]);
          gy := gy + (opy[yy][xx] * Gray[y + yy - 1][x + xx - 1]);
        end;
      Color := Trunc(Sqrt(gx*gx + gy*gy));
      if (Color < 0) then Color:=0 else if (Color > 255) then Color := 255;
      Result[y][x] := Color or (Color shl 8) or (Color shl 16);
    end;
end; 


function ImSobel(const ImgArr:T2DIntArray; Axis:Int8): T2DIntArray;
var
  W,H,x,y: Integer;
  mask: Array of TS8Array;
  Gray:T2DByteArray;
begin
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();
  SetLength(Gray, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
      Gray[y,x] := ColorToGray( ImgArr[y,x] );

  SetLength(mask, 3,3);
  if Axis = 0 then
  begin
    mask[0][0] := -1; mask[0][1] := 0; mask[0][2] := 1;
    mask[1][0] := -2; mask[1][1] := 0; mask[1][2] := 2;
    mask[2][0] := -1; mask[2][1] := 0; mask[2][2] := 1;
  end else begin
    mask[0][0] := -1; mask[0][1] := -2; mask[0][2] := -1;
    mask[1][0] :=  0; mask[1][1] :=  0; mask[1][2] :=  0;
    mask[2][0] :=  1; mask[2][1] :=  2; mask[2][2] :=  1;
  end;  

  SetLength(Result, H+1,W+1);
  W := W-1;
  H := H-1;
  for y:=1 to H do
    for x:=1 to W do
      Result[y,x] := (1020 + (
          (mask[0][0] * Gray[y-1][x-1]) + (mask[0][1] * Gray[y-1][x+0]) + (mask[0][2] * Gray[y-1][x+1]) +
          (mask[1][0] * Gray[y+0][x-1]) + (mask[1][1] * Gray[y+0][x+0]) + (mask[1][2] * Gray[y+0][x+1]) +
          (mask[2][0] * Gray[y+1][x-1]) + (mask[2][1] * Gray[y+1][x+0]) + (mask[2][2] * Gray[y+1][x+1])
      )) shr 3;
end;


{*
 Performs full convolution of Source, with the given mask (Srouce?mask). 
 Be warned: Mask should not be very large, as that would be really slow to proccess.
*}
function ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;
var
  W,H,x,y,yy,xx,cx,cy,dW,dH: Int32;
  mW,mH,mid:Int32;
  valR,valG,valB: Double;

  procedure ForceInBounds(const x,y, Wid,Hig: Int32; out cx,cy: Int32); Inline;
  begin
    cx := x; cy := y;
    if cx >= Wid then   cx := Wid-1
    else if cx < 0 then cx := 0;
    if cy >= Hig then   cy := Hig-1
    else if cy < 0 then cy := 0;
  end;

begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();
  SetLength(Result, H,W);

  mW := High(mask[0]);
  mH := High(mask);
  mid := (mW+1) div 2;
  dH := H - 1;
  dW := W - 1;
  for y:=0 to dH do
    for x:=0 to dW do
    begin
      valR := 0;
      valG := 0;
      valB := 0;
      for yy:=0 to mH do
        for xx:=0 to mW do
        begin
          ForceInBounds(x+xx-mid, y+yy-mid, W,H, cx,cy);
          valR := valR + (mask[yy,xx] * (ImgArr[cy,cx] and $FF));
          valG := valG + (mask[yy,xx] * (ImgArr[cy,cx] shr 8 and $FF));
          valB := valB + (mask[yy,xx] * (ImgArr[cy,cx] shr 16 and $FF));
        end;
      if valR > 255 then valR := 255 else if valR < 0 then valR := 0;
      if valG > 255 then valG := 255 else if valG < 0 then valG := 0;
      if valB > 255 then valB := 255 else if valB < 0 then valB := 0;
      Result[y][x] := (Round(valR)) or 
                      (Round(valG) shl 8) or 
                      (Round(valB) shl 16);
  end;
end;





{*
  Returns a gaussian blured version of the Matrix/ImgArray.
  @parmas:
    Radius can be any number. 1..11 is usually a normal.
    Sigma is usually set around 1.0-3.0.
*}
procedure ImGaussBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32; Sigma:Single);
type TFRGB = record R,G,B:Single; end;
var
  x,y,wid,hei,xx,yy,s,offset,dia,width:Int32;
  ptr:^TFRGB; f:TFRGB;
  tmp:Array of TFRGB;
  kernel:TFloatArray; 
begin
  if not(GetMatrixHigh(ImgArr, Wid,Hei)) then Exit();
  
  dia := Radius * 2 + 1;
  width := wid + 1;
  s := (hei+1) * width;

  if Hei <> High(Dest) then {if Dest is not pre-initalized then initalize it}
    SetLength(Dest, hei+1,wid+1);

  kernel := GaussKernel1D(Radius, Sigma);
  SetLength(tmp, s);

  // y direction
  offset := 0;
  repeat
    ptr := @tmp[0];
    for y:=0 to hei do
      for x:=0 to wid do
      begin
        xx := (x-Radius)+offset;
        if (xx < 0) then xx := 0 else if (xx > wid) then xx := wid;
        ptr^.R += (ImgArr[y,xx] and $FF) * kernel[offset];
        ptr^.G += ((ImgArr[y,xx] shr 8) and $FF) * kernel[offset];
        ptr^.B += ((ImgArr[y,xx] shr 16) and $FF) * kernel[offset];
        inc(ptr);
      end;
    inc(offset);
  until offset = dia;

  // x direction + result
  for y:=0 to hei do
    for x:=0 to wid do
    begin
      f.R := 0; f.G := 0; f.B := 0;
      ptr := @f;
      offset := 0;
      repeat
        yy := (y-Radius)+offset;
        if (yy < 0) then yy := 0 else if (yy > hei) then yy := hei;
        ptr^.R += tmp[yy*width+x].R * kernel[offset];
        ptr^.G += tmp[yy*width+x].G * kernel[offset];
        ptr^.B += tmp[yy*width+x].B * kernel[offset];
        inc(offset);
      until offset = dia;
      Dest[y,x] := (Round(ptr^.R)) or (Round(ptr^.G) shl 8) or (Round(ptr^.B) shl 16);
    end;
end;



{*
 Blends the two images in to a single image. Both images must be the same size.
*}
function ImBlend(const Img1, Img2: T2DIntArray; Alpha:Single=0.5): T2DIntArray;
var
  R1,G1,B1,R2,G2,B2:T2DIntArray;
  wA,wB:Single;
begin
  if (Length(Img1) <> Length(Img2)) then Exit();
  wA := Min(Max(Alpha, 0), 1.0);
  wB := 1.0-wA;
  ImGetRGB(Img1,R1,G1,B1);
  ImGetRGB(Img2,R2,G2,B2);

  Result := ImMergeRGB(
              ToInt((R1 * wA) + (R2 * wB)),
              ToInt((G1 * wA) + (G2 * wB)),
              ToInt((B1 * wA) + (B2 * wB))
            );
end;


(*
 Counts the number of matches end returns the num of hits in the range 0 to 1.
*)
function ImCompareAt(large,small:T2DIntArray; pt:TPoint; tol:Int32): Single;
var
  x,y,w,h,SAD:Int32;
  c1,c2:TRGB32;
begin
  if not(GetMatrixSize(small, W,H)) then Exit();
  SAD := 0;
  for y:=0 to h-1 do
    for x:=0 to w-1 do
    begin
      c1 := TRGB32(large[y+pt.y, x+pt.x]);
      c2 := TRGB32(small[y, x]);
      if (Abs(c1.R-c2.R) < Tol) and
         (Abs(c1.G-c2.G) < Tol) and
         (Abs(c1.B-c2.B) < Tol) then
        Inc(SAD);
    end;
  Result := SAD / (W*H);
end;



//-- Image resizing ----------------------------------------------------------||

(*
 NEAREST NEIGHBOR
*)
function ResizeMat_NEAREST(const ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,i,j: Integer;
  ratioX,ratioY: Single;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();
  ratioX := (W-1) / NewW;
  ratioY := (H-1) / NewH;
  SetLength(Result, NewH, NewW);
  Dec(NewW);
  for i:=0 to NewH-1 do 
  for j:=0 to NewW do
  begin
    x := Trunc(ratioX * j);
    y := Trunc(ratioY * i);
    Result[i][j] := ImgArr[y][x];
  end;
end;




(*
 BILINEAR: I guess one could call the result decent.. But honestly, for
           upscaling, I almost rather see my self scaling with NN + GaussBlur.
*)
function ResizeMat_BILINEAR(const ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,p0,p1,p2,p3,i,j: Int32;
  ratioX,ratioY,dx,dy: Single;
  R,G,B: Single;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();

  ratioX := (W-1) / NewW;
  ratioY := (H-1) / NewH;
  SetLength(Result, NewH, NewW);
  Dec(NewW);
  for i:=0 to NewH-1 do 
    for j:=0 to NewW do
    begin
      x := Trunc(ratioX * j);
      y := Trunc(ratioY * i);
      dX := ratioX * j - x;
      dY := ratioY * i - y;

      p0 := ImgArr[y][x];
      p1 := ImgArr[y][x+1];
      p2 := ImgArr[y+1][x];
      p3 := ImgArr[y+1][x+1];

      R := (p0 and $FF) * (1-dX) * (1-dY) +
           (p1 and $FF) * (dX * (1-dY)) +
           (p2 and $FF) * (dY * (1-dX)) +
           (p3 and $FF) * (dX * dY);

      G := ((p0 shr 8) and $FF) * (1-dX) * (1-dY) +
           ((p1 shr 8) and $FF) * (dX * (1-dY)) +
           ((p2 shr 8) and $FF) * (dY * (1-dX)) +
           ((p3 shr 8) and $FF) * (dX * dY); 
           
      B := ((p0 shr 16) and $FF) * (1-dX) * (1-dY) +
           ((p1 shr 16) and $FF) * (dX * (1-dY)) +
           ((p2 shr 16) and $FF) * (dY * (1-dX)) +
           ((p3 shr 16) and $FF) * (dX * dY);

      Result[i][j] := Trunc(R) or Trunc(G) shl 8 or Trunc(B) shl 16;
    end;
end;




//Used in bicubic interpolation.
//I could reqrite it to function without this, and gain some speed, but...
function _ImGetColor(const ImgArr:T2DIntArray; W,H, X,Y, C:Integer): Byte; Inline;
begin
  Result := 0;
  if (x > -1) and (x < W) and (y > -1) and (y < H) then
    case C of
      0: Result := ImgArr[y][x] and $FF;
      1: Result := (ImgArr[y][x] shr 8) and $FF;
      2: Result := (ImgArr[y][x] shr 16) and $FF;  
    end; 
end; 

(*
 BICUBIC: This got slower then expected, also worse result then expected...
          Tho I get that it's not faster, no real optimizations are used.
*)
function ResizeMat_BICUBIC(const ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,i,j,k,jj,yy,col: Int32;
  a0,a1,a2,a3,d0,d2,d3:Single;
  ratioX,ratioY,dx,dy: Single;
  C: Array of Single;
  Chan:TByteArray;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();

  ratioX := (W-1) / NewW;
  ratioY := (H-1) / NewH;

  SetLength(Result, NewH, NewW);
  SetLength(C, 4);
  SetLength(Chan, 3);
  Dec(NewH);
  Dec(NewW);
  
  for i:=0 to NewH do 
    for j:=0 to NewW do
    begin
      x := Trunc(ratioX * j);
      y := Trunc(ratioY * i);
      dX := ratioX * j - x;
      dY := ratioY * i - y;
      for k := 0 to 2 do
        for jj:= 0 to 3 do
        begin
          yy := y - 1 + jj;
          a0 := _ImGetColor(ImgArr, W, H, x+0, yy, k);
          d0 := _ImGetColor(ImgArr, W, H, x-1, yy, k) - a0;
          d2 := _ImGetColor(ImgArr, W, H, x+1, yy, k) - a0;
          d3 := _ImGetColor(ImgArr, W, H, x+2, yy, k) - a0;
          a1 := (-1.0 / 3 * d0 + d2 - 1.0 / 6 * d3);
          a2 := (1.0 / 2 * d0 + 1.0 / 2 * d2);
          a3 := (-1.0 / 6 * d0 - 1.0 / 2 * d2 + 1.0 / 6 * d3);
          C[jj] := (a0 + a1 * dx + a2 * dx * dx + a3 * dx * dx * dx);

          d0 := C[0] - C[1];
          d2 := C[2] - C[1];
          d3 := C[3] - C[1];
          a1 := (-1.0 / 3 * d0 + d2 -1.0 / 6 * d3);
          a2 := (1.0 / 2 * d0 + 1.0 / 2 * d2);
          a3 := (-1.0 / 6 * d0 - 1.0 / 2 * d2 + 1.0 / 6 * d3);
          Col := Trunc(C[1] + a1 * dy + a2 * dy * dy + a3 * dy * dy * dy);
          if (Col>255) then Col := 255
          else if (Col<0) then Col := 0;
          Chan[k] := Col;
        end;
      
      Result[i][j] := (Chan[0]) or (Chan[1] shl 8) or (Chan[2] shl 16);
    end;
end;


(*
 Resize a matrix/ImArray
 @Methods: RM_NEAREST, RM_BILINEAR and RM_BICUBIC.
*)
procedure ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:EResizeAlgo);
begin
  case Method of
    ERA_NEAREST: ImgArr := ResizeMat_NEAREST(ImgArr, NewW, NewH);
    ERA_BILINEAR:ImgArr := ResizeMat_BILINEAR(ImgArr, NewW, NewH);
    ERA_BICUBIC: ImgArr := ResizeMat_BICUBIC(ImgArr, NewW, NewH);
  end;
end;





(*
 Resize a matrix/ImArray
 *** TEST ***
 Meh.. it didn't yield that much better result, and its quite hacky (will fail at some stuff).
 - I am keeping it in here to remind my self.
*)
function TestResizeBI(Src:T2DIntArray; NewW,NewH:int32): T2DIntArray; cdecl;
var
  fr,sx,sy,offs,i,x,y,W,H,nW1:Int32;
  a,b:TRGB32;
  rowptr,ptr:PRGB32;
  Tmp: Array of TRGB32;
begin
  if not(GetMatrixSize(Src, W,H)) then Exit();

  //horizontal streching
  i := trunc(65536 * ((W-1) / NewW));
  SetLength(Tmp,H*NewW);
  ptr := @tmp[0];
  nW1 := NewW - 1;
  for y:=0 to H-1 do
  begin
    offs := 0;
    rowptr := @src[y,0];
    for x:=0 to nW1 do
    begin
      sx := offs shr 16;
      fr := offs and $FFFF;
      a := rowptr[sx];
      b := rowptr[sx+1];

      ptr^.R := a.r + ((b.r-a.r) * fr) shr 16;
      ptr^.G := a.g + ((b.g-a.g) * fr) shr 16;
      ptr^.B := a.b + ((b.b-a.b) * fr) shr 16;
      inc(offs,i);
      inc(ptr);
    end;
  end;


  //vertical streching
  SetLength(Result,NewH,NewW);
  i := trunc(65536 * ((H-1) / NewH));
  offs := 0;
  for y:=0 to NewH-1 do
  begin
    sy := (offs shr 16) * NewW;
    fr := offs and $FFFF;
    for x:=0 to nW1 do
    begin
      a := Tmp[sy+x];
      b := Tmp[sy+x+NewW];
      Result[y,x] := (a.b + ((b.b-a.b) * fr) shr 16) or
                     (a.g + ((b.g-a.g) * fr) shr 16) shl 8 or
                     (a.r + ((b.r-a.r) * fr) shr 16) shl 16;
    end;
    offs += i;
  end;
end;


(*
 High quality downsampling algorithm.
*)
function ImSample(ImgArr:T2DIntArray; Scale:Int32): T2DIntArray;
type
  TRGBMatrix = Array of Array of TRGB32;
var
  x,y,ys,W,H,nW,nH,sqscale:Int32;
  mat: TRGBMatrix;
  
  function GetAreaColor(ImgArr:TRGBMatrix; px,py,scale,sqscale:Int32): Int32; inline;
  var
    x,y:Int32;
    R:Int32=0; G:Int32=0; B:Int32=0;
  begin
    for y:=py to py+scale-1 do
      for x:=px to px+scale-1 do
      begin
        R += ImgArr[y,x].R;
        G += ImgArr[y,x].G;
        B += ImgArr[y,x].B;
      end;
    R := R div sqscale;
    G := G div sqscale;
    B := B div sqscale;
    Result := B or G shl 8 or R shl 16;
  end;

begin
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();
  nW := W div Scale;
  nH := H div Scale;
  sqscale := Scale*Scale;
  SetLength(Result, nH,nW);
  mat := TRGBMatrix(ImgArr);
  for y:=0 to nH-1 do
  begin
    ys := y*scale;
    for x:=0 to nW-1 do
      Result[y,x] := GetAreaColor(mat, x*scale, ys, scale, sqscale);
  end;
end;




//-- Image rotatating --------------------------------------------------------||

(*
 Computes the expanded bounds according to the new angle
*)
function __GetNewSizeRotated(W,H:Int32; Angle:Single): TBox;
  function Rotate(p:TPoint; angle:Single; mx,my:Int32): TPoint;
  begin
    Result.X := Round(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
    Result.Y := Round(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y - my));
  end;
var pts: TPointArray;
begin
  SetLength(pts, 4);
  Result := Box($FFFFFF,$FFFFFF,0,0);
  pts[0]:= Rotate(Point(0,h), angle, W div 2, H div 2);
  pts[1]:= Rotate(Point(w,h), angle, W div 2, H div 2);
  pts[2]:= Rotate(Point(w,0), angle, W div 2, H div 2);
  pts[3]:= Rotate(Point(0,0), angle, W div 2, H div 2);
  Result := TPABounds(pts);
end;


(*
 Rotates the bitmap using bilinear interpolation
*)
function __RotateBI(const ImgArr:T2DIntArray; Angle:Single): T2DIntArray;
var
  i,j,R,G,B,mx,my,W,H,fX,fY,cX,cY: Int32;
  rX,rY,dX,dY,cosa,sina:Single;
  p0,p1,p2,p3: TRGB32;
  topR,topG,topB,BtmR,btmG,btmB:Single;
begin
  if not(GetMatrixHigh(ImgArr, W,H)) then Exit();

  SetLength(Result, H, W);
  cosa := Cos(Angle);
  sina := Sin(Angle);
  mX := W div 2;
  mY := H div 2;

  W -= 1;
  H -= 1;
  for i := 0 to H do begin
    for j := 0 to W do begin
      rx := (mx + cosa * (j - mx) - sina * (i - my));
      ry := (my + sina * (j - mx) + cosa * (i - my));

      fX := Trunc(rX);
      fY := Trunc(rY);
      cX := Ceil(rX);
      cY := Ceil(rY);

      if not((fX < 0) or (cX < 0) or (fX > W) or (cX > W) or
             (fY < 0) or (cY < 0) or (fY > H) or (cY > H)) then
      begin
        dx := rX - fX;
        dy := rY - fY;

        p0 := TRGB32(ImgArr[fY, fX]);
        p1 := TRGB32(ImgArr[fY, cX]);
        p2 := TRGB32(ImgArr[cY, fX]);
        p3 := TRGB32(ImgArr[cY, cX]);

        TopR := (1 - dx) * p0.R + dx * p1.R;
        TopG := (1 - dx) * p0.G + dx * p1.G;
        TopB := (1 - dx) * p0.B + dx * p1.B;
        BtmR := (1 - dx) * p2.R + dx * p3.R;
        BtmG := (1 - dx) * p2.G + dx * p3.G;
        BtmB := (1 - dx) * p2.B + dx * p3.B;

        R := Round((1 - dy) * TopR + dy * BtmR);
        G := Round((1 - dy) * TopG + dy * BtmG);
        B := Round((1 - dy) * TopB + dy * BtmB);

        if (R < 0) then R := 0
        else if (R > 255)then R := 255;
        if (G < 0) then G := 0
        else if (G > 255)then G := 255;
        if (B < 0) then B := 0
        else if (B > 255)then B := 255;

        Result[i,j] := B or (G shl 8) or (R shl 16);
      end;
    end;
  end;
end;


(*
 Rotates the bitmap using bilinear interpolation, does expand
*)
function __RotateExpandBI(const ImgArr:T2DIntArray; Angle:Single): T2DIntArray;
var
  i,j,R,G,B,mx,my,W,H,nW,nH,fX,fY,cX,cY: Int32;
  rX,rY,dX,dY,cosa,sina:Single;
  topR,topG,topB,BtmR,btmG,btmB:Single;
  p0,p1,p2,p3: TRGB32;
  NewB:TBox;
begin
  if not(GetMatrixSize(ImgArr, W,H)) then Exit();

  NewB := __GetNewSizeRotated(W,H,Angle);
  nW := NewB.Width;
  nH := NewB.Height;
  mX := nW div 2;
  mY := nH div 2;
  SetLength(Result,nH,nW);
  cosa := Cos(Angle);
  sina := Sin(Angle);
  nW -= 1; nH -= 1;
  for i := 0 to nH do begin
    for j := 0 to nW do begin
      rx := (mx + cosa * (j - mx) - sina * (i - my));
      ry := (my + sina * (j - mx) + cosa * (i - my));

      fX := (Trunc(rX)+ NewB.x1);
      fY := (Trunc(rY)+ NewB.y1);
      cX := (Ceil(rX) + NewB.x1);
      cY := (Ceil(rY) + NewB.y1);

      if not((fX < 0) or (cX < 0) or (fX >= W) or (cX >= W) or
             (fY < 0) or (cY < 0) or (fY >= H) or (cY >= H)) then
      begin
        dx := rX - (fX - NewB.x1);
        dy := rY - (fY - NewB.y1);

        p0 := TRGB32(ImgArr[fY, fX]);
        p1 := TRGB32(ImgArr[fY, cX]);
        p2 := TRGB32(ImgArr[cY, fX]);
        p3 := TRGB32(ImgArr[cY, cX]);

        TopR := (1 - dx) * p0.R + dx * p1.R;
        TopG := (1 - dx) * p0.G + dx * p1.G;
        TopB := (1 - dx) * p0.B + dx * p1.B;
        BtmR := (1 - dx) * p2.R + dx * p3.R;
        BtmG := (1 - dx) * p2.G + dx * p3.G;
        BtmB := (1 - dx) * p2.B + dx * p3.B;

        R := Round((1 - dy) * TopR + dy * BtmR);
        G := Round((1 - dy) * TopG + dy * BtmG);
        B := Round((1 - dy) * TopB + dy * BtmB);

        if (R < 0) then R := 0
        else if (R > 255) then R := 255;
        if (G < 0) then G := 0
        else if (G > 255) then G := 255;
        if (B < 0) then B := 0
        else if (B > 255) then B := 255;

        Result[i,j] := (B or (G shl 8) or (R shl 16));
      end;
    end;
  end;
end;


(*
 Rotates the bitmap using nearest neighbor
*)
function __RotateNN(const Mat:T2DIntArray; Angle:Single): T2DIntArray;
var
  W,H,x,y,mx,my,i,j:Int32;
  cosa,sina:Single;
begin
  if not(GetMatrixHigh(Mat, W,H)) then Exit();
  
  mx := W div 2;
  my := H div 2;
  SetLength(Result, H+1,W+1);
  cosa := cos(angle);
  sina := sin(angle);
  for i:=0 to H do
    for j:=0 to W do
    begin
      x := Round(mx + cosa * (j - mx) - sina * (i - my));
      y := Round(my + sina * (j - mx) + cosa * (i - my));
      if (x >= 0) and (x < W) and (y >= 0) and (y < H) then
        Result[i,j] := Mat[y,x];
    end;
end;


(*
 Rotates the bitmap using nearest neighbor, does expand
*)
function __RotateExpandNN(const Mat:T2DIntArray; Angle:Single): T2DIntArray;
var
  nW,nH,W,H,x,y,mx,my,j,i:Int32;
  NewB:TBox;
  cosa,sina:Single;
begin
  if not(GetMatrixSize(Mat, W,H)) then Exit();

  mx := W div 2;
  my := H div 2;
  NewB := __GetNewSizeRotated(W,H,Angle);
  nW := NewB.Width;
  nH := NewB.Height;
  SetLength(Result, nH,nW);
  cosa := cos(angle);
  sina := sin(angle);

  nw -= 1; nh -= 1;
  for i:=0 to nH do
    for j:=0 to nW do
    begin
      x := Round(mx + cosa * (NewB.x1+j - mx) - sina * (NewB.y1+i - my));
      y := Round(my + sina * (NewB.x1+j - mx) + cosa * (NewB.y1+i - my));
      if (x >= 0) and (x < W) and (y >= 0) and (y < H) then
        Result[i,j] := Mat[y,x];
    end;
end;


function ImRotate(const Mat:T2DIntArray; Angle:Single; Expand:Boolean; Bilinear:Boolean=True): T2DIntArray;
begin
  case Expand of
    True:
      case Bilinear of
        True:  Result := __RotateExpandBI(Mat,Angle);
        False: Result := __RotateExpandNN(Mat,Angle);
      end;
    False:
      case Bilinear of
        True:  Result := __RotateBI(Mat,Angle);
        False: Result := __RotateNN(Mat,Angle);
      end;
  end;
end;







end.
