{!DOCTOPIC}{ 
  Imaging functions
}

{!DOCREF} {
  @method: function se.GaussKernel(Radius: Integer; Sigma:Single): T2DFloatArray;  
  @desc: Generates a gussin matrix/kernel.
}
function SimbaExt.GaussKernel(Radius: Integer; Sigma:Single): T2DFloatArray;  
begin
  Result := exp_GaussKernel(Radius, Sigma);
end;

{!DOCREF} {
  @method: function se.ImBlurFilter(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;    
  @desc: Applies a box-blur to the image. Running it multiple times with a small blur results similarly as to what a gaussian blur would, but larger.
}
function SimbaExt.ImBlur(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;  
begin
  Result := exp_ImBlur(ImgArr, Radius);
end;


{!DOCREF} {
  @method: function se.ImMedianFilter(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;  
  @desc: Applies a median filter. Picks the median pixel value in a window with the given radius `Radius`.
}
function SimbaExt.ImMedianBlur(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;  
begin
  Result := exp_ImMedianBlur(ImgArr, Radius);
end;


{!DOCREF} {
  @method: function se.ImBrighten(ImgArr:TIntMatrix; Amount:Extended; Legacy:Boolean):  TIntMatrix;  
  @desc: Allows you to modify the brightness of the image
}
function SimbaExt.ImBrighten(ImgArr:TIntMatrix; Amount:Extended; Legacy:Boolean):  TIntMatrix;  
begin
  Result := exp_ImBrighten(ImgArr, Amount, Legacy);
end;


{!DOCREF} {
  @method: function se.ImEnhance(ImgArr:TIntMatrix; Enhancement:Byte; C:Extended=0):  TIntMatrix;  
  @desc: Enhances R,G,B levels.
}
function SimbaExt.ImEnhance(ImgArr:TIntMatrix; Enhancement:Byte; C:Extended=0):  TIntMatrix;  
begin
  Result := exp_ImEnhance(ImgArr, Enhancement, C);
end;


{!DOCREF} {
  @method: function se.ImThreshold(const ImgArr:TIntMatrix; Threshold, Alpha, Beta:Byte; Invert:Boolean=False):  TIntMatrix;  
  @desc: 
    A simple threshold function. Anything above Threshold = Beta, and bellow = Alpha. Swaps Alpha and beta if Invert=True
    [params]
      ImgArr:     A 2D matrix representation of an image.
      Alpha,Beta: Lower and upper result colors (0-255).
      Invert:     Invert the result
    [/params]
}
function SimbaExt.ImThreshold(const ImgArr:TIntMatrix; Threshold, Alpha, Beta:Byte; Invert:Boolean=False):  TIntMatrix;  
begin
  Result := exp_ImThreshold(ImgArr, Threshold, Alpha, Beta, Invert);
end;


{!DOCREF} {
  @method: function se.ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Int32=0):  TIntMatrix;  
  @desc: 
    Threshold function which first calculates the average color of the image, then turns anything above Mean = Beta, and bellow = Alpha.
    [params]
      ImgArr:     A 2D matrix representation of an image.
      Alpha,Beta: Lower and upper result colors (0-255).
      Invert:     Invert the result
      C:          Modiefier to mean. Negative C substracts from mean, positive C adds to mean.
      Method:     TA_MEAN | TA_MINMAX
    [/params]
}
function SimbaExt.ImThresholdAdaptive(const ImgArr:TIntMatrix; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Int32=0):  TIntMatrix;  
begin
  Result := exp_ImThresholdAdaptive(ImgArr, Alpha, Beta, Invert, Method, C);
end;


{!DOCREF} {
  @method: function se.ImFindContours(const ImgArr:TIntMatrix; Outlines:Boolean):  T2DPointArray;  
  @desc: Meh...
}
function SimbaExt.ImFindContours(const ImgArr:TIntMatrix; Outlines:Boolean):  T2DPointArray;  
begin
  Result := exp_ImFindContours(ImgArr,Outlines);
end;


{!DOCREF} {
  @method: function se.ImCEdges(const ImgArr: TIntMatrix; MinDiff: Integer):  TPointArray;
  @desc: Meh...
}
function SimbaExt.ImCEdges(const ImgArr: TIntMatrix; MinDiff: Integer):  TPointArray;  
begin
  Result := exp_ImCEdges(ImgArr, MinDiff);
end;


{!DOCREF} {
  @method: function se.ImSobel(const ImgArr:TIntMatrix): TIntMatrix;  
  @desc: Applies the sobel function on the image, both x, any y-wise 
}
function SimbaExt.ImSobel(const ImgArr:TIntMatrix): TIntMatrix;  
begin
  Result := exp_ImSobel(ImgArr);
end;


{!DOCREF} {
  @method: function se.ImConvolve(const ImgArr:TIntMatrix; Mask:T2DFloatArray): TIntMatrix;  
  @desc: 
    2D Convoution.
    [code=pascal]
    var Im:TRafBitmap;
    begin
      Im.Open('lena.png');
      Im.FromMatrix(  se.ImConvolve(Im.ToMatrix(), se.GaussKernel(5,3))  );
      Im.Debug();
      Im.Free();
    end;  
    [/code]
}
function SimbaExt.ImConvolve(const ImgArr:TIntMatrix; Mask:T2DFloatArray): TIntMatrix;  
begin
  Result := exp_ImConvolve(ImgArr, Mask);
end;


{!DOCREF} {
  @method: function se.ImGaussBlur(const ImgArr:TIntMatrix; Radius: Int32; Sigma:Single=1.5): TIntMatrix;  
  @desc: Applies a gaussian blur to the image. 
}
function SimbaExt.ImGaussBlur(const ImgArr:TIntMatrix; Radius: Int32; Sigma:Single=1.5): TIntMatrix;  
begin
  SetLength(Result, Length(ImgArr), Length(ImgArr[0]));
  exp_ImGaussBlur(ImgArr, Result, Radius, Sigma);
end;


{!DOCREF} {
  @method: function se.ImBlend(ImgArr1, ImgArr2:TIntMatrix; Alpha:Single): TIntMatrix;  
  @desc: Applies a gaussian blur to the image. 
}
function SimbaExt.ImBlend(ImgArr1, ImgArr2:TIntMatrix; Alpha:Single): TIntMatrix; 
begin
  Result := exp_ImBlend(ImgArr1, ImgArr2, Alpha);
end;


{!DOCREF} {
  @method: procedure se.ImResize(var ImgArr:TIntMatrix; NewW, NewH: Int32; Method:TResizeAlgo);  
  @desc: Resize the image (matrix) by one of RA_NEAREST, RA_BICUBIC, and RA_BILINEAR interpolation 
}
procedure SimbaExt.ImResize(var ImgArr:TIntMatrix; NewW, NewH: Integer; Method:TResizeAlgo);  
begin
  exp_ImResize(ImgArr, NewW, NewH, Method);
end;


{!DOCREF} {
  @method: function se.ImRotate(ImgArr:T2DIntArray; Angle:Single; Expand:Boolean; BiLinear:Boolean=True): T2DIntArray;
  @desc: 
    Returns a rotated version of the image matrix, you can choose if you want to `expand` it, and if it should use `bilinear` interpolation (smoother rotation).
    Angle should be given in radians.
}
function SimbaExt.ImRotate(ImgArr:T2DIntArray; Angle:Single; Expand:Boolean; Bilinear:Boolean=True): T2DIntArray; 
begin
  Result := exp_ImRotate(ImgArr, Angle, Expand, Bilinear);
end;


{*=========================================================================================|
| CornerDet.pas                                                  [placing it hear for now] |
| Update: Replaced Exteded with Single :: Resulted in ~2x speedup                          |
|=========================================================================================*}
{!DOCREF} {
  @method: function se.CornerResponse(const ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer): T2DFloatArray;   
  @desc: Computes the harris response of the image, usually used to locate the corners.
}
function SimbaExt.CornerResponse(const ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer): T2DFloatArray;  
begin
  Result := exp_CornerResponse(ImgArr, GaussDev, KSize);
end;


{!DOCREF} {
  @method: function se.FindCornerPoints(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image.[br] 
    
    A few c'overloads' to simplify your life:
    [code=pascal]
    > function se.FindCornerPoints(var ImgArr:TIntMatrix; Thresh:Single; Footprint:Integer): TPointArray; overload; 
    > function se.FindCornerPoints(var ImgArr:TIntMatrix; Thresh:Single): TPointArray; overload;  
    [/code]
    
    [params]
      ImgArr:     A 2D matrix representation of an image.
      GaussDev:   Guassian deviation, used when we blur the image, a value between 1-3 is normal.
      KSize:      Size of the gaussblur filter, 1 is usually good.
      Thresh:     This is a tolerance, anything above the threshold = result point.
      Footprint:  The square which we check for a peak, larger is better, but also slower.
    [/params]
}
function SimbaExt.FindCornerPoints(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;  
begin
  Result := exp_FindCornerPoints(ImgArr, GaussDev, KSize, Thresh, Footprint);
end;

function SimbaExt.FindCornerPoints(var ImgArr:TIntMatrix; Thresh:Single; Footprint:Integer): TPointArray; overload; 
begin
  Result := exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, Footprint);
end;

function SimbaExt.FindCornerPoints(var ImgArr:TIntMatrix; Thresh:Single): TPointArray; overload;   
begin
  Result := exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, 5);
end;



{!DOCREF} {
  @method: function se.FindCornerMidPoints(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image.
    Similar to c'se.FindCornerPoints', only that this one uses ClusterTPA to find the mean of each point within the given tolerance.
    So if two points are within the given MinDist, they will be merged as one.[br] 
    
    A few c'overloads' to simplify your life:
    [code=delphi]
    > function se.FindCornerMidPoints(var ImgArr:TIntMatrix; Thresh:Single; MinDist:Integer): TPointArray; overload; 
    > function se.FindCornerMidPoints(var ImgArr:TIntMatrix; Thresh:Single): TPointArray; overload; 
    [/code]
    
    Example:
    [code=pascal]
    var
      BMP:TRafBitmap;
      TPA: TPointArray;
      i:Int32;
    begin
      BMP.Open('calibrationimage.png');
      TPA := se.FindCornerMidPoints(BMP.ToMatrix(), 2.0, 2, 0.07, 3);

      for i:=0 to High(TPA) do
        BMP.SetPixels(se.TPACross(TPA[i], 3), 255);

      BMP.Debug();
      BMP.Free;
    end.
    [/code]
    Output:
    [img]http://slackworld.net/downloads/calibrationimage.png[/img]
}
function SimbaExt.FindCornerMidPoints(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
begin
  Result := exp_FindCornerMidPoints(ImgArr, GaussDev, KSize, Thresh, MinDist);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:TIntMatrix; Thresh:Single; MinDist:Integer): TPointArray; overload; 
begin
  Result := exp_FindCornerPoints(ImgArr, 2.0, 1, Thresh, MinDist);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:TIntMatrix; Thresh:Single): TPointArray; overload;   
begin
  Result := exp_FindCornerPoints(ImgArr, 2.0, 1, Thresh, 3);
end;
