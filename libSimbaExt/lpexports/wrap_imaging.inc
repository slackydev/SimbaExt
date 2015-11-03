{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap imaging methods (Imaging.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure exp_Grayscale(var se:SExt; const image:T2DIntArray; var Dest:T2DIntArray); cdecl;
begin
  Grayscale(image, Dest);
end;

function exp_GaussKernel(var se:SExt; KernelRadius:Integer; Sigma:Single): T2DFloatArray; cdecl;
begin
  Result := GaussKernel(KernelRadius, Sigma);
end;

procedure exp_Blur(var se:SExt; const image:T2DIntArray; var Dest:T2DIntArray; Radius:Integer); cdecl;
begin
  Blur(image, Dest, Radius);
end;

function exp_MedianBlur(var se:SExt; const image: T2DIntArray; Radius:Int32): T2DIntArray; cdecl;
begin
  Result := MedianBlur(image, Radius);
end;

function exp_Brighten(var se:SExt; const image:T2DIntArray; Amount:Extended): T2DIntArray; cdecl;
begin
  Result := Brighten(image, Amount);
end;

procedure exp_Threshold(var se:SExt; var image:T2DIntArray; Thresh:Byte; Alpha, Beta:Int32); cdecl;
begin
  Threshold(image, Thresh, Alpha, Beta);
end;

procedure exp_ThresholdAdap(var se:SExt; var image:T2DIntArray; Alpha, Beta: Int32; Method:EThreshAlgo; C:Int32); cdecl;
begin
  ThresholdAdaptive(image, Alpha, Beta, Method, C);
end;

function exp_FindContours(var se:SExt; const image: T2DIntArray; Thresh: Int32): T2DPointArray; cdecl;
begin
  Result := FindContours(image, Thresh);
end;

function exp_CEdges(var se:SExt; const image: T2DIntArray; MinDiff: Int32): TPointArray; cdecl;
begin
  Result := CEdges(image, MinDiff);
end;

function exp_Sobel(var se:SExt; const image: T2DIntArray; Axis:Int8): T2DIntArray; cdecl;
begin
  if Axis in [0,1] then
    Result := Sobel(image,axis)
  else
    Result := Sobel(image);
end;

function exp_Convolve(var se:SExt; const image:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray; cdecl;
begin
  Result := Convolve(image, Mask);
end;

procedure exp_GaussBlur(var se:SExt; const image:T2DIntArray; var Dest:T2DIntArray; Radius:Int32; Sigma:Single); cdecl;
begin
  GaussBlur(image, Dest, Radius, Sigma);
end;

function exp_Blend(var se:SExt; const image1,image2: T2DIntArray; Alpha: Single): T2DIntArray; cdecl;
begin
  Result := Blend(image1, image2, Alpha);
end;

function exp_CompareAt(var se:SExt; const large,small:T2DIntArray; pt:TPoint; tol:Int32): Single; cdecl;
begin
  Result := CompareAt(large, small, pt, tol);
end;

procedure exp_Resize(var se:SExt; var image:T2DIntArray; NewW, NewH: Integer; Method:EResizeAlgo); Cdecl;
begin
  Resize(image, NewW, NewH, Method);
end;

function exp_Sample(var se:SExt; image:T2DIntArray; SampleScale:Int32): T2DIntArray; Cdecl;
begin
  Result := Sample(image, SampleScale);
end;

function exp_Rotate(var se:SExt; const image:T2DIntArray; Angle:Single; Expand:LongBool; Bilinear:LongBool=True): T2DIntArray; Cdecl;
begin
  Result := Rotate(image, Angle, Expand, Bilinear);
end;
