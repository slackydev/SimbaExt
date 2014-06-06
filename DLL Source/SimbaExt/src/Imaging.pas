Unit Imaging;
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
  Math, CoreTypes, CoreMisc;

procedure ImGetRGB(Img: T2DIntArray; out R,G,B:T2DByteArray);
function ImMergeRGB(R,G,B:T2DByteArray): T2DIntArray;  
function GaussKernel(KernelRadius:Integer; Sigma:Single): T2DFloatArray;
function ImBlurFilter(ImgArr: T2DIntArray; Block:Integer): T2DIntArray; 
function ImMedianFilter(ImgArr: T2DIntArray; Block:Integer): T2DIntArray; 
function ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean): T2DIntArray; 
function ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended): T2DIntArray; 
function ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean): T2DIntArray; 
function ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer): T2DIntArray;
function ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean): T2DPointArray; 
function ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray; 
function ImSobel(const ImgArr: T2DIntArray): T2DIntArray; 
function ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;
function ImGaussBlur(const ImgArr:T2DIntArray; Radius: Integer; Sigma: Single): T2DIntArray;
function ImBlend(Img1, Img2: T2DIntArray; Alpha:Single=0.5): T2DIntArray; //not exported yet..
function ImFilterGray(const ImgArr:T2DIntArray; MinDark, MaxDark:Byte; Replace, Tol:Integer): T2DIntArray;
procedure ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo);
function ImRotate(Mat:T2DIntArray; Angle:Single; Expand:Boolean; BiLinear:Boolean=True): T2DIntArray;

//--------------------------------------------------
implementation

uses
  PointTools, ColorMath, MatrixMath;

 
{*
 Converts the ImMatrix to separate arrays for each channel (R,G,B).
*} 
procedure ImGetRGB(Img: T2DIntArray; out R,G,B:T2DByteArray); 
var W,H,x,y:Int32;
begin
  H := High(Img);
  if H < 0 then Exit;
  W := High(Img[0]);
  SetLength(R, H+1, W+1);
  SetLength(G, H+1, W+1);
  SetLength(B, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do begin
      R[y][x] := Img[y][x] and $FF;
      G[y][x] := Img[y][x] shr 8 and $FF;
      B[y][x] := Img[y][x] shr 16 and $FF;
    end;
end;


{*
 Takes three channels (R,G,B), one for each color, merges them in to a Image-matrix.
*} 
function ImMergeRGB(R,G,B:T2DByteArray): T2DIntArray; 
var W,H,x,y:Int32;
begin
  H := High(R);
  if H < 0 then Exit;
  W := High(R[0]);
  SetLength(Result, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := R[y][x] or G[y][x] shl 8 or B[y][x] shl 16;
end;
  
  
{*
 Generates a gaussian filter. 
*}
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


{*
 Returns a blurred version of the Matrix/ImgArray.
 Block is the radius of the blur: 3,5,7,9...
*}
function ImBlurFilter(ImgArr: T2DIntArray; Block:Integer): T2DIntArray; 
var
  W,H,x,y,mid,fx,fy,size:Integer;
  R,G,B,color,lx,ly,hx,hy:Integer;
begin
  if (Block<=1) or (Block mod 2 = 0) then Exit;
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Result, H+1,W+1);
  mid := Block div 2;
  
  for y:=0 to H do
  begin
    ly := Max(0,y-mid);
    hy := Min(H,y+mid);
    for x:=0 to W do
    begin
      lx := Max(0,x-mid);
      hx := Min(W,x+mid);
      R := 0; G := 0; B := 0;
      Size := 0;
      for fy:=ly to hy do
        for fx:=lx to hx do
        begin
          Color := ImgArr[fy][fx];
          R := R + (Color and $FF);
          G := G + ((Color shr 8) and $FF);
          B := B + ((Color shr 16) and $FF);
          Inc(Size);
        end;
      Result[y][x] := (R div size) or
                      ((G div size) shl 8) or
                      ((B div size) shl 16);
    end;
  end;
end;


{*
 Filter a matrix/ImgArr with a Median Filter.
 Block is the radius of the filter, 3,5,7,9...
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
      ExchI(Arr[TmpIdx], Arr[TmpIdx - 1]);
      ExchI(Weight[TmpIdx], Weight[TmpIdx - 1]);
    end;
end;

function ImMedianFilter(ImgArr: T2DIntArray; Block:Integer):T2DIntArray; 
var
  W,H,j,x,y,fx,fy,low,mid,size,color:Integer;
  lx,ly,hx,hy:Integer;
  Filter,Colors:TIntArray;
begin
  Size := Block * Block;
  if (Size<=1) or (Block mod 2 = 0) then Exit;
  W := High(ImgArr[0]);
  H := High(ImgArr);
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
function ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean): T2DIntArray; 
var
  W,H,x,y,R,G,B,AmountL:Integer;
  cH,cS,cV:Single;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
  SetLength(Result, H,W);
  R := 0;G := 0;B := 0;
  Dec(W); 
  Dec(H);
  case Legacy of
   False:
    //This has shown to be not as stable as I tought.
    for y:=0 to H do
      for x:=0 to W do
      begin
        ColorToHSV(ImgArr[y][x], cH,cS,cV);
        cV := cV+(Amount*100);
        if (cV < 0.0) then cV := 0
        else if (cV > 100) then cV := 100;
        HSVToRGB(cH,cS,cV, R,G,B);
        Result[y][x] := (R) or (G shl 8) or (B shl 16);
      end;
   True:
    begin
      AmountL := Round(Amount);
      for y:=0 to H do
        for x:=0 to W do
        begin
          ColorToRGB2(ImgArr[y][x], R,G,B);
          R := R + AmountL;
          if (R > 255) then R:=255
          else if (R < 0) then R:=0;

          G := G + AmountL;
          if (G > 255) then G:=255
          else if (G < 0) then G:=0;

          B := B + AmountL;
          if (B > 255) then B:=255
          else if (B < 0) then B:=0;
          Result[y][x] := (R) or (G shl 8) or (B shl 16);
        end;
   end;
  end;
end;


{*
 Enhances colors in the image by a given value.
 @params:

   Enhancement: How much to substraact or add to the color.
   C: Based on the "mid"-value (127), if color is bellow then it gets weakened,
      if it's above then it gets enhanced.
*}
function ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended): T2DIntArray; 
var
  W,H,x,y,R,G,B:Integer;
  mid: Single;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
  SetLength(Result, H,W);

  Mid := 127 * C;
  Dec(W);
  Dec(H);
  for y:=0 to H do
    for x:=0 to W do
    begin
      ColorToRGB2(ImgArr[y][x], R,G,B);

      if R > mid then begin
        R := R + Enhancement;
        if (R > 255) then R:=255;
      end else begin
        R := R - Enhancement;
        if (R < 0) then R:=0;
      end;
      
      if G > mid then begin 
        G := G + Enhancement;
        if (G > 255) then G:=255;
      end else begin
        G := G - Enhancement;
        if (G < 0) then G:=0;
      end;
      
      if B > mid then begin
        B := B + Enhancement;
        if (B > 255) then B:=255;
      end else begin
        B := B - Enhancement;
        if (B < 0) then B:=0;
      end;
      
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
function ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta: Byte; Invert:Boolean): T2DIntArray; 
var
  W,H,x,y,i:Integer;
  Tab: Array [0..256] of Byte;
begin
  if Alpha >= Beta then Exit;
  if Alpha > Beta then ExchBt(Alpha, Beta); 

  W := Length(ImgArr[0]);
  H := Length(ImgArr);
  SetLength(Result, H,W);
  
  if Invert then ExchBt(Alpha, Beta); 
  for i:=0 to (Threshold-1) do Tab[i] := Alpha;
  for i:=Threshold to 255 do Tab[i] := Beta;
  Dec(W); 
  Dec(H);
  
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := Tab[ColorToGray(ImgArr[y][x])];
end;


{*
 This function first finds the Mean of the image, and set the threshold to it. Again: colors bellow the Threshold will be set to `Alpha`
 the colors above or equal to the Mean/Threshold will be set to `Beta`.
 @todo: Test to use a matrix filter to reduce noice of size: 3x3, 5x5, 7x7 etc.. ( What did I mean by that? :P )
 @params:
    Alpha: Minvalue for result
    Beta: Maxvalue for result
    Invert: Bellow Mean is set to Beta, rather then Alpha.
    Method: TM_Mean or TM_MinMax
    C: Substract or add to the mean.
*}
function ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer): T2DIntArray;
var
  W,H,x,y,i:Integer;
  Color,IMin,IMax: Byte;
  Threshold,Counter: Integer;
  Temp: T2DByteArray;
  Tab: Array [0..256] of Byte;   
begin
  if Alpha >= Beta then Exit;
  if Alpha > Beta then ExchBt(Alpha, Beta); 
  
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
  SetLength(Result, H,W);
  SetLength(Temp, H,W);
  Dec(W); 
  Dec(H);
  
  //Finding the threshold - While at it convert image to grayscale.
  Threshold := 0;
  Case Method of 
    //Find the Arithmetic Mean / Average.
    TA_MEAN:
    begin
      for y:=0 to H do
      begin
        Counter := 0;
        for x:=0 to W do
        begin
          Color := ColorToGray(ImgArr[y][x]);
          Temp[y][x] := Color;
          Counter := Counter + Color;
        end;
        Threshold := Threshold + (Counter div W);
      end;
      if (C < 0) then Threshold := (Threshold div H) - Abs(C)
      else Threshold := (Threshold div H) + C;
    end;
    
    //Mean of Min and Max values
    TA_MINMAX:
    begin
      IMin := ColorToGray(ImgArr[0][0]);
      IMax := IMin;
      for y:=0 to H do
        for x:=0 to W do
        begin
          Color := ColorToGray(ImgArr[y][x]);
          Temp[y][x] := Color;
          if Color < IMin then
            IMin := Color
          else if Color > IMax then
            IMax := Color;
        end;
      if (C < 0) then Threshold := ((IMax+IMin) shr 1) - Abs(C)
      else Threshold := ((IMax+IMin) shr 1) + C;
    end;
  end;
  
  Threshold := Max(0, Min(Threshold, 255)); //In range 0..255
  if Invert then ExchBt(Alpha, Beta);
  for i:=0 to (Threshold-1) do Tab[i] := Alpha;
  for i:=Threshold to 255 do Tab[i] := Beta;  
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := Tab[Temp[y][x]];
  SetLength(Temp, 0);
end;


{
  ImgArr is treated as a binary array, so 0s will be left alone, and anything above 0 will be checked.
  You can use this with XT_Threshold or XT_ThresholdApdative.
  
  This will probably be changed to something more "proper".
}
function ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean): T2DPointArray; 
var
  W,H,j,i,x,y:Integer;
  TPA:TPointArray;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(TPA, W*H);
  j := 0;
  for y:=1 to H do
    for x:=1 to W do
      if ImgArr[y][x] > 0 then
      begin
        TPA[j].x := x;
        TPA[j].y := y;
        Inc(j);
      end;
  SetLength(TPA, j);
  Result := ClusterTPA(TPA, 1, True);
  SetLength(TPA,0);
  if Outlines then
  begin
    for i:=0 to High(Result) do
      Result[i] := TPAOutline(Result[i]);
  end;
end;


{
  Given a matrix that represents an image this algorithm extacts the contrast edge points.
  The result is an Array of TPoint (TPointArray).
  Uses RGB and R,G and B are weighted equally.
}
function ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray; 
var
  X,Y,Width,Height,Len,QSize: Integer;
  R,G,B,R1,G1,B1:Byte;
  Hit:Boolean;
begin
  Width := High(ImgArr[0]);
  Height := High(ImgArr);
  MinDiff := Sqr(MinDiff) * 3;
  QSize := Min(1000, Width*Height);
  SetLength(Result, QSize+1);
  
  Len := 0;
  for Y:=0 to Height do 
    for X:=0 to Width do
    begin
      Hit := False;
      if ((X+1) < Width) then
      begin
        ColorToRGB(ImgArr[Y][X], R,G,B);
        ColorToRGB(ImgArr[Y][X+1], R1,G1,B1);
        if Sqr(R-R1)+Sqr(G-G1)+Sqr(B-B1) >= MinDiff then Hit := True;
      end;

      if ((Y+1) < Height) and Not(Hit) then 
      begin
        ColorToRGB(ImgArr[Y][X], R,G,B);
        ColorToRGB(ImgArr[Y+1][X], R1,G1,B1);
        if Sqr(R-R1)+Sqr(G-G1)+Sqr(B-B1) >= MinDiff then  Hit := True;
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
  W := High(ImgArr[0]);
  H := High(ImgArr);
  SetLength(Gray, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
    begin
      color := ImgArr[y][x];
      Gray[y][x] := Trunc((0.299 * (Color and $FF)) +
                          (0.587 * ((Color shr 8) and $FF)) +
                          (0.114 * ((Color shr 16) and $FF)));
    end;

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


{*
 Performs full convolution of Source, with the given mask (Srouce?mask). 
 Be warned: Mask should not be very large, as that would be really slow to proccess.
 
 Unlike Convolution-function found in Bitmaps.pas in Simba, this will `wrap around`
 so that each pixel of the source can be convolved. It costs a bit more, but its needed.
*}
function ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;
var
  W,H,x,y,yy,xx,cx,cy,dW,dH: Integer;
  mW,mH,mid:Integer;
  valR,valG,valB: Single;

  procedure ForceInBounds(const x,y, Wid,Hig: Int32; out cx,cy: Int32); Inline;
  begin
    cx := x; cy := y;
    if cx >= Wid then   cx := Wid-1
    else if cx < 0 then cx := 0;
    if cy >= Hig then   cy := Hig-1
    else if cy < 0 then cy := 0;
  end;

begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
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
          valR := valR + (mask[yy][xx] * (ImgArr[cy][cx] and $FF));
          valG := valG + (mask[yy][xx] * ((ImgArr[cy][cx] shr 8) and $FF));
          valB := valB + (mask[yy][xx] * ((ImgArr[cy][cx] shr 16) and $FF));
        end;
      Result[y][x] := (Round(valR)) or 
                      (Round(valG) shl 8) or 
                      (Round(valB) shl 16);
  end;
end;


{*
  Returns a gaussion blurred version of the Matrix/ImgArray.
  @parmas:
    Radius can be any number, but should keep it low, like: 1-9
    Sigma is usually around 1.0-3.0. 
*}
function ImGaussBlur(const ImgArr:T2DIntArray; Radius: Integer; Sigma: Single): T2DIntArray;
begin
  Result := ImConvolve(ImgArr, GaussKernel(Radius,Sigma));
end;


{*
 Blends the two images in to a single image. Both images must be the same size.
*}
function ImBlend(Img1, Img2: T2DIntArray; Alpha:Single=0.5): T2DIntArray;
var
  R1,G1,B1,R2,G2,B2:T2DByteArray;
  wA,wB:Single;
begin
  if (Length(Img1) <> Length(Img2)) then Exit();
  wA := Min(Max(Alpha, 0), 1.0);
  wB := 1.0-wA;
  ImGetRGB(Img1,R1,G1,B1);
  ImGetRGB(Img2,R2,G2,B2);

  Result := ImMergeRGB(
              ToByte((R1 * wA) + (R2 * wB)),
              ToByte((G1 * wA) + (G2 * wB)),
              ToByte((B1 * wA) + (B2 * wB))
            );
end; 


{*
 Replace all colors which are grayish with the given "replace"-color.
 How grayish is determined by tolerance (Tol), and "MinDark, MaxDark" (How dark, and how bright a color is allowed to be)
*}
function ImFilterGray(const ImgArr:T2DIntArray; MinDark, MaxDark:Byte; Replace, Tol:Integer): T2DIntArray;
var
  w,h,x,y: Int32;
  r,g,b:Byte;
  intensity: Int32;
begin
  H := High(ImgArr);
  W := High(ImgArr[0]);
  SetLength(Result, H+1, W+1);
  for y:=0 to H do
    for x:=0 to W do
    begin
      ColorToRGB(ImgArr[y][x], R,G,B);
      Intensity := (R+B+G) div 3;
      if (Max(Max(R,G),B) - Min(Min(R,G),B) <= Tol) and
         (InRange(Intensity, MinDark, MaxDark)) then
        Result[y][x] := Replace
      else
        Result[y][x] := ImgArr[y][x];
    end;
end;





//-- Image resizing ----------------------------------------------------------||

(*
 NEAREST NEIGHBOR
*)
function ResizeMat_NEAREST(ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,i,j: Integer;
  ratioX,ratioY: Single;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
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
           upscaling, I almost rather see my self scaling with NN + Blur..
*)
function ResizeMat_BILINEAR(ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,p0,p1,p2,p3,i,j: Int32;
  ratioX,ratioY,dx,dy: Single;
  R,G,B: Single;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
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
function _ImGetColor(ImgArr:T2DIntArray; W,H, X,Y, C:Integer): Byte; Inline;
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
          Kinda get that it's not faster, no "deep" optimizations are used.
*)
function ResizeMat_BICUBIC(ImgArr:T2DIntArray; NewW, NewH: Integer): T2DIntArray;
var
  W,H,x,y,i,j,k,jj,yy,col: Int32;
  a0,a1,a2,a3,d0,d2,d3:Single;
  ratioX,ratioY,dx,dy: Single;
  C: Array of Single;
  Chan:TByteArray;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);
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
procedure ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo);
begin
  case Method of
    RA_NEAREST: ImgArr := ResizeMat_NEAREST(ImgArr, NewW, NewH);
    RA_BILINEAR:ImgArr := ResizeMat_BILINEAR(ImgArr, NewW, NewH);
    RA_BICUBIC: ImgArr := ResizeMat_BICUBIC(ImgArr, NewW, NewH);
  end;
end;











//---------- IMAGE ROTATING
function __GetNewSizeRotated(W,H:Int32; Angle:Single): TBox;
  function Rotate(p:TPoint; angle:Single; mx,my:Int32): TPoint;
  begin
    Result.X := Ceil(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
    Result.Y := Ceil(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y - my));
  end;
var
  B: TPointArray;
  i:Int32;
begin
  SetLength(B, 4);
  Result := Box($FFFFFF,$FFFFFF,0,0);
  B[0]:= Rotate(Point(0,h),angle, W div 2, H div 2);
  B[1]:= Rotate(Point(w,h),angle, W div 2, H div 2);
  B[2]:= Rotate(Point(w,0),angle, W div 2, H div 2);
  B[3]:= Rotate(Point(0,0),angle, W div 2, H div 2);
  for i:=0 to 3 do begin
    if B[i].x > Result.X2 then
      Result.X2 := B[i].x
    else if B[i].x < Result.X1 then
      Result.X1 := B[i].x;
    if B[i].Y > Result.Y2 then
      Result.Y2 := B[i].y
    else if B[i].y < Result.Y1 then
      Result.Y1 := B[i].y;
  end;
end;



function __RotateBI(ImgArr:T2DIntArray; Angle:Single): T2DIntArray;
var
  i,j,x,y,R,G,B,mx,my,W,H,fX,fY,cX,cY: Int32;
  dist, polAngle,rX,rY,dX,dY:Single;
  p0,p1,p2,p3: TRGB32;
  topR,topG,topB,BtmR,btmG,btmB:Single;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);

  mX := W div 2;
  mY := H div 2;
  SetLength(Result, H,W);
  for i := 0 to H-1 do begin
    for j := 0 to W-1 do begin
      // convert raster to Cartesian
      x := j - mX;
      y := mY - i;

      // convert Cartesian to polar
      dist := Sqrt(sqr(x) + sqr(y));
      if (x = 0) then begin
        if (y = 0) then begin
            // centre of image, no rotation needed
            Result[i,j] := ImgArr[i,j];
            Continue;
        end else if (y < 0) then
            polAngle := 1.5 * PI
        else
            polAngle := 0.5 * PI;
      end else
        polAngle := ArcTan2(y, x);

      polAngle := polAngle - Angle;

      rX := mX + (dist * Cos(polAngle));
      rY := mY - (dist * Sin(polAngle));

      fX := Floor(rX);
      fY := Floor(rY);
      cX := Ceil(rX);
      cY := Ceil(rY);

      if (fX < 0) or (cX < 0) or (fX >= W) or (cX >= W) or
         (fY < 0) or (cY < 0) or (fY >= H) or (cY >= H) then
        Continue;

      dx := rX - fX;
      dy := rY - fY;

      p0 := TRGB32(ImgArr[fY, fX]);
      p1 := TRGB32(ImgArr[fY, cX]);
      p2 := TRGB32(ImgArr[cY, fX]);
      p3 := TRGB32(ImgArr[cY, cX]);

      // linearly interpolate horizontally between top neighbours
      TopR := (1 - dx) * p0.R + dx * p1.R;
      TopG := (1 - dx) * p0.G + dx * p1.G;
      TopB := (1 - dx) * p0.B + dx * p1.B;

      // linearly interpolate horizontally between bottom neighbours
      BtmR := (1 - dx) * p2.R + dx * p3.R;
      BtmG := (1 - dx) * p2.G + dx * p3.G;
      BtmB := (1 - dx) * p2.B + dx * p3.B;

      // linearly interpolate vertically between top and bottom interpolated results
      R := Round((1 - dy) * TopR + dy * BtmR);
      G := Round((1 - dy) * TopG + dy * BtmG);
      B := Round((1 - dy) * TopB + dy * BtmB);

      // make sure colour values are valid
      if (R < 0)  then R := 0
      else if (R > 255)then R := 255;
      if (G < 0)  then G := 0
      else if (G > 255)then G := 255;
      if (B < 0)  then B := 0
      else if (B > 255)then B := 255;

      Result[i,j] := (B or (G shl 8) or (R shl 16));
    end;
  end;
end;


function __RotateExpandBI(ImgArr:T2DIntArray; Angle:Single): T2DIntArray;
var
  i,j,x,y,R,G,B,mx,my,W,H,nW,nH,fX,fY,cX,cY: Int32;
  dist, polAngle,rX,rY,dX,dY:Single;
  topR,topG,topB,BtmR,btmG,btmB:Single;
  p0,p1,p2,p3: TRGB32;
  NewB:TBox;
  xxx:Int32;
begin
  W := Length(ImgArr[0]);
  H := Length(ImgArr);

  NewB := __GetNewSizeRotated(W-1,H-1,Angle);
  nW := NewB.Width;
  nH := NewB.Height;
  mX := nW div 2;
  mY := nH div 2;
  SetLength(Result,nH,nW);
  for i := 0 to nH-1 do begin
    for j := 0 to nW-1 do begin
      x := j - mX;
      y := mY - i;

      dist := Sqrt(sqr(x) + sqr(y));
      if (x = 0) then begin
        if (y = 0) then begin
            Result[i,j] := ImgArr[i+NewB.y1, j+NewB.y1];
            Continue;
        end else if (y < 0) then
            polAngle := 1.5 * PI
        else
            polAngle := 0.5 * PI;
      end else
        polAngle := ArcTan2(y, x);

      polAngle := polAngle - Angle;

      rX := mX + (dist * Cos(polAngle));
      rY := mY - (dist * Sin(polAngle));

      fX := Floor(rX)+NewB.x1;
      fY := Floor(rY)+NewB.y1;
      cX := Ceil(rX)+NewB.x1;
      cY := Ceil(rY)+NewB.y1;

      if (fX < 0) or (cX < 0) or (fX >= W) or (cX >= W) or
         (fY < 0) or (cY < 0) or (fY >= H) or (cY >= H) then
        Continue;

      dx := rX - (fX - NewB.x1);
      dy := rY - (fY - NewB.y1);

      p0 := TRGB32(ImgArr[fY, fX]);
      p1 := TRGB32(ImgArr[fY, cX]);
      p2 := TRGB32(ImgArr[cY, fX]);
      p3 := TRGB32(ImgArr[cY, cX]);

      // linearly interpolate horizontally between top neighbours
      TopR := (1 - dx) * p0.R + dx * p1.R;
      TopG := (1 - dx) * p0.G + dx * p1.G;
      TopB := (1 - dx) * p0.B + dx * p1.B;

      // linearly interpolate horizontally between bottom neighbours
      BtmR := (1 - dx) * p2.R + dx * p3.R;
      BtmG := (1 - dx) * p2.G + dx * p3.G;
      BtmB := (1 - dx) * p2.B + dx * p3.B;

      // linearly interpolate vertically between top and bottom interpolated results
      R := Round((1 - dy) * TopR + dy * BtmR);
      G := Round((1 - dy) * TopG + dy * BtmG);
      B := Round((1 - dy) * TopB + dy * BtmB);

      // make sure colour values are valid
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


function __RotateNN(Mat:T2DIntArray; Angle:Single): T2DIntArray;
var
  W,H,x,y,mx,my:Int32;
  PT:TPoint;
  cosa,sina:Single;
begin
  W := High(Mat[0]);
  H := High(Mat);
  mx := W div 2;
  my := H div 2;
  SetLength(Result, H+1,W+1);
  cosa := cos(angle);
  sina := sin(angle);
  for x:=0 to W do
    for y:=0 to H do
    begin
      pt.x := Round(mx + cosa * (x - mx) - sina * (y - my));
      pt.y := Round(my + sina * (x - mx) + cosa * (y - my));
      if pt.InBox(0,0,W-1,H-1) then
        Result[y,x] := Mat[pt.y,pt.x];
    end;
end;


function __RotateExpandNN(Mat:T2DIntArray; Angle:Single): T2DIntArray;
var
  nW,nH,W,H,x,y,mx,my:Int32;
  PT:TPoint;
  NewB:TBox;
  cosa,sina:Single;
begin
  W := Length(Mat[0]);
  H := Length(Mat);
  mx := W div 2;
  my := H div 2;
  NewB := __GetNewSizeRotated(W-1,H-1,Angle);
  nW := NewB.Width;
  nH := NewB.Height;
  SetLength(Result, nH,nW);
  cosa := cos(angle);
  sina := sin(angle);
  for x:=0 to nW do
    for y:=0 to nH do
    begin
      pt.x := Round(mx + cosa * (NewB.x1+x - mx) - sina * (NewB.y1+y - my));
      pt.y := Round(my + sina * (NewB.x1+x - mx) + cosa * (NewB.y1+y - my));
      if pt.InBox(0,0,W-1,H-1) then
        Result[y,x] := Mat[pt.y,pt.x];
    end;
end;


function ImRotate(Mat:T2DIntArray; Angle:Single; Expand:Boolean; BiLinear:Boolean=True): T2DIntArray;
begin
  case Expand of
    True:
      case BiLinear of
        True: Result := __RotateExpandBI(Mat,Angle);
        False: Result := __RotateExpandNN(Mat,Angle);
      end;
    False:
      case BiLinear of
        True: Result := __RotateBI(Mat,Angle);
        False: Result := __RotateNN(Mat,Angle);
      end;
  end;
end;







end.
