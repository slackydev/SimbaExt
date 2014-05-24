{!DOCTOPIC}{ 
  Imaging functions
}

{!DOCREF} {
  @method: function se.GaussKernel(Radius: Integer; Sigma:Single): T2DFloatArray;  
  @desc: Generates a gussin matrix/kernel.
}
function SimbaExt.GaussKernel(Radius: Integer; Sigma:Single): T2DFloatArray;  
begin
  exp_GaussKernel(Radius, Sigma, Result);
end;

{!DOCREF} {
  @method: function se.ImBlurFilter(ImgArr: T2DIntArray; Block:Integer):  T2DIntArray;    
  @desc: Applies a box-blur to the image. Running it multiple times with a small blur results similarly as to what a gaussian blur would.
}
function SimbaExt.ImBlurFilter(ImgArr: T2DIntArray; Block:Integer):  T2DIntArray;  
begin
  exp_ImBlurFilter(ImgArr, Block, Result);
end;

{!DOCREF} {
  @method: function se.ImMedianFilter(ImgArr: T2DIntArray; Block:Integer):  T2DIntArray;  
  @desc: Applies a median filter to the image.
}
function SimbaExt.ImMedianFilter(ImgArr: T2DIntArray; Block:Integer):  T2DIntArray;  
begin
  exp_ImMedianFilter(ImgArr, Block, Result);
end;

{!DOCREF} {
  @method: function se.ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean):  T2DIntArray;  
  @desc: Allows you to modify the brightness of the image
}
function SimbaExt.ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean):  T2DIntArray;  
begin
  exp_ImBrighten(ImgArr, Amount, Legacy, Result);
end;

{!DOCREF} {
  @method: function se.ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended):  T2DIntArray;  
  @desc: Enhances R,G,B levels.
}
function SimbaExt.ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended):  T2DIntArray;  
begin
 exp_ImEnhance(ImgArr, Enhancement, C, Result);
end;

{!DOCREF} {
  @method: function se.ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean=False):  T2DIntArray;  
  @desc: A simple threshold function. Anything above Threshold = Beta, and bellow = Alpha. Swaps Alpha and beta if Invert=True
}
function SimbaExt.ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean=False):  T2DIntArray;  
begin
  exp_ImThreshold(ImgArr, Threshold, Alpha, Beta, Invert, Result);
end;


{!DOCREF} {
  @method: function se.ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Int32=0):  T2DIntArray;  
  @desc: 
    Threshold function which first calculates the average color of the image, then turns anything above Mean = Beta, and bellow = Alpha.
    @params:
      ImgArr:     A 2D matrix representation of an image.
      Alpha,Beta: Lower and upper result colors (0-255).
      Invert:     Invert the result
      C:          Modiefier to mean. Negative C substracts from mean, positive C adds to mean.
      Method:     TA_MEAN | TA_MINMAX
}
function SimbaExt.ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Int32=0):  T2DIntArray;  
begin
  exp_ImThresholdAdaptive(ImgArr, Alpha, Beta, Invert, Method, C, Result);
end;


{!DOCREF} {
  @method: function se.ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean):  T2DPointArray;  
  @desc: Meh...
}
function SimbaExt.ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean):  T2DPointArray;  
begin
  exp_ImFindContours(ImgArr,Outlines, Result);
end;

{!DOCREF} {
  @method: function se.ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer):  TPointArray;
  @desc: Meh...
}
function SimbaExt.ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer):  TPointArray;  
begin
  exp_ImCEdges(ImgArr, MinDiff, Result);
end;


{!DOCREF} {
  @method: function se.ImSobel(const ImgArr:T2DIntArray): T2DIntArray;  
  @desc: Applies the sobel function on the image, both x, any y-wise 
}
function SimbaExt.ImSobel(const ImgArr:T2DIntArray): T2DIntArray;  
begin
  exp_ImSobel(ImgArr, Result);
end;


{!DOCREF} {
  @method: function se.ImConvolve(const ImgArr:T2DIntArray; Mask:T2DFloatArray): T2DIntArray;  
  @desc: 
    2D Convoution.
    [code=pascal]
    var Im:TRafBitmap;
    begin
      Im.Open('test.png');
      Im.FromMatrix(  se.Convolve(Im.ToMatrix(), se.GaussKernel(5,2))  ); 
      Im.Debug();
      Im.Free();
    end;
    [/code]
}
function SimbaExt.ImConvolve(const ImgArr:T2DIntArray; Mask:T2DFloatArray): T2DIntArray;  
begin
  exp_ImConvolve(ImgArr, Mask, Result);
end;


{!DOCREF} {
  @method: function se.ImGaussBlur(const ImgArr:T2DIntArray; Radius: Int32; Sigma:Single): T2DIntArray;  
  @desc: Applies a gaussion blur to the image.
}
function SimbaExt.ImGaussBlur(const ImgArr:T2DIntArray; Radius: Int32; Sigma:Single): T2DIntArray;  
begin
  exp_ImGaussBlur(ImgArr, Radius, Sigma, Result);
end;


{!DOCREF} {
  @method: procedure se.ImResize(var ImgArr:T2DIntArray; NewW, NewH: Int32; Method:TResizeAlgo);  
  @desc: Resize the image (matrix) by one of RA_NEAREST, RA_BICUBIC, and RA_BILINEAR interpolation 
}
procedure SimbaExt.ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo);  
begin
  exp_ImResize(ImgArr, NewW, NewH, Method);
end;




{*=========================================================================================|
| CornerDet.pas                                                  [placing it hear for now] |
| Update: Replaced Exteded with Single :: Resulted in ~2x speedup                          |
|=========================================================================================*}
{!DOCREF} {
  @method: function se.CornerResponse(const ImgArr:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;   
  @desc: Computes the harris response of the image, usually used to locate the corners.
}
function SimbaExt.CornerResponse(const ImgArr:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray;  
begin
  exp_CornerResponse(ImgArr, GaussDev, KSize, Result);
end;


{!DOCREF} {
  @method: function se.FindCornerPoints(var ImgArr:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image.
    
    A few c'overloads', to simplify your life:
    [code=pascal]
    > function se.FindCornerPoints(var ImgArr:T2DIntArray; Thresh:Single; Footprint:Integer): TPointArray; overload; 
    > function se.FindCornerPoints(var ImgArr:T2DIntArray; Thresh:Single): TPointArray; overload;  
    [/code]
}
function SimbaExt.FindCornerPoints(var ImgArr:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;  
begin
  exp_FindCornerPoints(ImgArr, GaussDev, KSize, Thresh, Footprint, Result);
end;

function SimbaExt.FindCornerPoints(var ImgArr:T2DIntegerArray; Thresh:Single; Footprint:Integer): TPointArray; overload; 
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, Footprint, Result);
end;

function SimbaExt.FindCornerPoints(var ImgArr:T2DIntegerArray; Thresh:Single): TPointArray; overload;   
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, 5, Result);
end;



{!DOCREF} {
  @method: function se.FindCornerMidPoints(var ImgArr:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image.
    Similar to c'se.FindCornerPoints', only that this one uses ClusterTPA to find the mean of each point within the given tolerance.
    So if two points are within the given MinDist, they will be merged as one. 
    
    A few c'overloads', to simplify your life:
    [code=delphi]
    > function se.FindCornerMidPoints(var ImgArr:T2DIntArray; Thresh:Single; MinDist:Integer): TPointArray; overload; 
    > function se.FindCornerMidPoints(var ImgArr:T2DIntArray; Thresh:Single): TPointArray; overload; 
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
function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
begin
  exp_FindCornerMidPoints(ImgArr, GaussDev, KSize, Thresh, MinDist, Result);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntArray; Thresh:Single; MinDist:Integer): TPointArray; overload; 
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, MinDist, Result);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntArray; Thresh:Single): TPointArray; overload;   
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, 3, Result);
end;
