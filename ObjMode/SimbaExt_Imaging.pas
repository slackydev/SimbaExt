{!DOCTOPIC}{ 
  Imaging functions
}
function SimbaExt.GaussKernel(Radius: Integer; Sigma:Single): T2DFloatArray;  
begin
  exp_GaussKernel(Radius, Sigma, Result);
end;

function SimbaExt.ImBlurFilter(ImgArr: T2DIntegerArray; Block:Integer):  T2DIntegerArray;  
begin
  exp_ImBlurFilter(ImgArr, Block, Result);
end;

function SimbaExt.ImMedianFilter(ImgArr: T2DIntegerArray; Block:Integer):  T2DIntegerArray;  
begin
  exp_ImMedianFilter(ImgArr, Block, Result);
end;

function SimbaExt.ImBrighten(ImgArr:T2DIntegerArray; Amount:Extended; Legacy:Boolean):  T2DIntegerArray;  
begin
  exp_ImBrighten(ImgArr, Amount, Legacy, Result);
end;

function SimbaExt.ImEnhance(ImgArr:T2DIntegerArray; Enhancement:Byte; C:Extended):  T2DIntegerArray;  
begin
 exp_ImEnhance(ImgArr, Enhancement, C, Result);
end;

function SimbaExt.ImThreshold(const ImgArr:T2DIntegerArray; Threshold, Alpha, Beta:Byte; Invert:Boolean):  T2DIntegerArray;  
begin
  exp_ImThreshold(ImgArr, Threshold, Alpha, Beta, Invert, Result);
end;

function SimbaExt.ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TxThreshMethod; C:Integer):  T2DIntegerArray;  
begin
  exp_ImThresholdAdaptive(ImgArr, Alpha, Beta, Invert, Method, C, Result);
end;

function SimbaExt.ImFindContours(const ImgArr:T2DIntegerArray; Outlines:Boolean):  T2DPointArray;  
begin
  exp_ImFindContours(ImgArr,Outlines, Result);
end;

function SimbaExt.ImCEdges(const ImgArr: T2DIntegerArray; MinDiff: Integer):  TPointArray;  
begin
  exp_ImCEdges(ImgArr, MinDiff, Result);
end;

function SimbaExt.ImSobel(const ImgArr:T2DIntegerArray): T2DIntArray;  
begin
  exp_ImSobel(ImgArr, Result);
end;

function SimbaExt.ImConvolve(const ImgArr:T2DIntegerArray; Mask:T2DFloatArray): T2DIntArray;  
begin
  exp_ImConvolve(ImgArr, Mask, Result);
end;

function SimbaExt.ImGaussBlur(const ImgArr:T2DIntegerArray; Radius: Integer; Sigma:Single): T2DIntArray;  
begin
  exp_ImGaussBlur(ImgArr, Radius, Sigma, Result);
end;

procedure SimbaExt.ImResize(var ImgArr:T2DIntegerArray; NewW, NewH: Integer; Method:TxResizeMethod);  
begin
  exp_ImResize(ImgArr, NewW, NewH, Method);
end;




{*=========================================================================================|
| CornerDet.pas                                                  [placing it hear for now] |
| Update: Replaced Exteded with Single :: Resulted in ~2x speedup                          |
|=========================================================================================*}
function SimbaExt.CornerResponse(const ImgArr:T2DIntegerArray; GaussDev:Single; KSize:Integer): T2DFloatArray;  
begin
  exp_CornerResponse(ImgArr, GaussDev, KSize, Result);
end;


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

// Similar to the above, only that it uses ClusterTPA to find the mean of each point within the given tolerance.
// So if two points are within the given MinDist, they will be merged as one.
function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;  
begin
  exp_FindCornerMidPoints(ImgArr, GaussDev, KSize, Thresh, MinDist, Result);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntegerArray; Thresh:Single; MinDist:Integer): TPointArray; overload; 
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, MinDist, Result);
end;

function SimbaExt.FindCornerMidPoints(var ImgArr:T2DIntegerArray; Thresh:Single): TPointArray; overload;   
begin
  exp_FindCornerPoints(ImgArr, 1.0, 1, Thresh, 3, Result);
end;
