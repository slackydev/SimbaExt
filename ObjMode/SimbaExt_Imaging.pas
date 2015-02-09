{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 This file is a Doc-stub | The code is in SimbaExt.dll
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{!DOCTOPIC}{ 
  Imaging functions
}

{!DOCREF} {
  @method: function se.GaussKernel(Radius: Integer; Sigma:Single): TFloatMatrix;  
  @desc: Generates a 2D gaussian matrix/kernel.
}

{!DOCREF} {
  @method: function se.ImBlur(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;    
  @desc: Applies a box-blur to the image. Running it multiple times with a small blur results similarly as to what a gaussian blur would, but larger.
}


{!DOCREF} {
  @method: function se.ImMedianBlur(ImgArr: TIntMatrix; Radius:Integer):  TIntMatrix;  
  @desc: Applies a median filter. Picks the median pixel value in a window with the given radius `Radius`.
}


{!DOCREF} {
  @method: function se.ImBrighten(ImgArr:TIntMatrix; Amount:Extended): TIntMatrix;  
  @desc: Adjust the brightness of the image
}


{!DOCREF} {
  @method: procedure se.ImThreshold(var ImgArr:TIntMatrix; Threshold, Alpha, Beta:Byte; Invert:Boolean=False);
  @desc: 
    A simple threshold function. Anything above Threshold = Beta, and bellow = Alpha. Swaps Alpha and beta if Invert=True
    [params]
      ImgArr:     A 2D matrix representation of an image.
      Alpha,Beta: Lower and upper result colors (0-255).
      Invert:     Invert the result
    [/params]
}


{!DOCREF} {
  @method: procedure se.ImThresholdAdaptive(var ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:EThreshAlgo; C:Int32=0);
  @desc: 
    Threshold function which first calculates the average color of the image, then turns anything above Mean = Beta, and bellow = Alpha.
    [params]
      ImgArr:     A 2D matrix representation of an image.
      Alpha,Beta: Lower and upper result colors (0-255).
      Invert:     Invert the result
      C:          Modiefier to mean. Negative C substracts from mean, positive C adds to mean.
      Method:     `ETA_MEAN`, `ETA_MINMAX`
    [/params]
}


{!DOCREF} {
  @method: function se.ImCEdges(const ImgArr: TIntMatrix; MinDiff: Integer):  TPointArray;
  @desc: Returns the edges of the image, a very simple algorithm is used for this, and it's not particularly reliable.
}


{!DOCREF} {
  @method: function se.ImSobel(const ImgArr:TIntMatrix): TIntMatrix;  
  @desc: Applies the sobel function on the image, both x, any y-wise 
}


{!DOCREF} {
  @method: function se.ImConvolve(const ImgArr:TIntMatrix; Mask:TFloatMatrix): TIntMatrix;  
  @desc: 
    2D Convoution, applies the filter you give to the image `ImgArr`.
    
    EG: Apply a 5x5 gaussion blur to a image (tho ImGaussBlur is prefered for this):
    [code=pascal]
    var 
      Im:TRafBitmap;
      D:TIntMatrix;
    begin
      Im.Open('lena.png');
      D := se.ImConvolve(Im.ToMatrix(), se.GaussKernel(5,3));
      Im.FromMatrix(D);
      Im.Debug();
      Im.Free();
    end;  
    [/code]
}


{!DOCREF} {
  @method: procedure se.ImGaussBlur(const ImgArr:TIntMatrix; var Dest:TIntMatrix; Radius: Int32; Sigma:Single=1.5);  
  @desc: Applies a gaussian blur to the destionation `dest`. 
}


{!DOCREF} {
  @method: function se.ImBlend(ImgArr1, ImgArr2:TIntMatrix; Alpha:Single): TIntMatrix;  
  @desc: Applies a gaussian blur to the image. 
}


{!DOCREF} {
  @method: procedure se.ImResize(var ImgArr:TIntMatrix; NewW, NewH: Int32; Method:EResizeAlgo);  
  @desc: Resize the image (matrix) by using ERA_NEAREST, ERA_BICUBIC, or ERA_BILINEAR interpolation 
}

{!DOCREF} {
  @method: function se.ImSample(ImgArr:T2DIntArray; SampleScale:Int32): T2DIntArray;
  @desc: Returns a smaller sample of the given image.
}

{!DOCREF} {
  @method: function se.ImRotate(ImgArr:T2DIntArray; Angle:Single; Expand:Boolean; BiLinear:Boolean=True): T2DIntArray;
  @desc: 
    Returns a rotated version of the image matrix, you can choose if you want to `expand` it, and if it should use `bilinear` interpolation (smoother rotation).
    Angle should be given in radians.
}


{*=========================================================================================|
| CornerDet.pas                                                  [placing it hear for now] |
|                                                                                          |
|=========================================================================================*}
{!DOCREF} {
  @method: function se.CornerResponse(const ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer): TFloatMatrix;   
  @desc: Computes the harris response of the image, usually used to locate the corners.
}


{!DOCREF} {
  @method: function se.FindCorners(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image `ImgArr`.
    See `se.FindCornersMid` for an example.[br] 
    
    [params]
      ImgArr:     A 2D matrix representation of an image.
      GaussDev:   Guassian deviation, used when we blur the image, a value between 1-3 is generally decent.
      KSize:      Size of the gaussblur filter, 1 or 2 is usually good.
      Thresh:     This is a tolerance, anything above the threshold = result point, start with something small like `0.05` and work you wat up.
      Footprint:  The square which we check for a peak, try something between 1 and 10.
    [/params]
}



{!DOCREF} {
  @method: function se.FindCornersMid(var ImgArr:TIntMatrix; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
  @desc: 
    Locates all the corner points in the image.
    Similar to `se.FindCornerPoints`, only that this one uses ClusterTPA to find the mean of each point within the given tolerance.
    So if two points are within the given MinDist, they will be merged as one. All other parameters are the same as `se.FindCornerPoints`[br] 
    
    Example:
    [code=pascal]
    var
      BMP:TRafBitmap;
      TPA: TPointArray;
      i:Int32;
    begin
      BMP.Open('calibrationimage.png');
      TPA := se.FindCornersMid(BMP.ToMatrix(), 2.0, 2, 0.07, 3);

      for i:=0 to High(TPA) do
        BMP.SetPixels(se.TPACross(TPA[i], 3), 255);

      BMP.Debug();
      BMP.Free;
    end.
    [/code]
    Output:
    [img]http://slackworld.net/downloads/calibrationimage.png[/img]
}
