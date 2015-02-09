{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports imaging methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@exp_GaussKernel,	'function SimbaExt.GaussKernel(KernelRadius:Integer; Sigma:Single): TFloatMatrix;');
AddMethod(@exp_ImGrayscale,	'procedure SimbaExt.imGrayscale(const ImgArr:T2DIntArray; var Dest:T2DIntArray);');
AddMethod(@exp_ImBlur,		'procedure SimbaExt.imBlur(const ImgArr:TIntMatrix; var Dest:TIntMatrix; Radius:Int32);');
AddMethod(@exp_ImMedianBlur,	'function SimbaExt.imMedianBlur(const ImgArr: TIntMatrix; Radius:Integer): TIntMatrix;');
AddMethod(@exp_ImBrighten,	'function SimbaExt.imBrighten(const ImgArr:TIntMatrix; Amount:Extended): TIntMatrix;');
AddMethod(@exp_ImThreshold,	'procedure SimbaExt.imThreshold(var ImgArr:TIntMatrix; Threshold:Byte; Alpha, Beta:Int32);');
AddMethod(@exp_ImThresholdAdaptive,	'procedure SimbaExt.imThresholdAdaptive(var ImgArr:TIntMatrix; Alpha, Beta: Int32; Method:EThreshAlgo; C:Int32=0);');
AddMethod(@exp_ImFindContours,	'function SimbaExt.imFindContours(const ImgArr: TIntMatrix; Threshold: Int32): T2DPointArray;');
AddMethod(@exp_ImCEdges,	'function SimbaExt.imCEdges(const ImgArr: TIntMatrix; MinDiff: Int32): TPointArray;');
AddMethod(@exp_ImSobel,		'function SimbaExt.imSobel(const ImgArr: TIntMatrix; Axis:Int8=-1): TIntMatrix;');
AddMethod(@exp_ImConvolve,	'function SimbaExt.imConvolve(const ImgArr:TIntMatrix; const Mask:TFloatMatrix): TIntMatrix;');
AddMethod(@exp_ImGaussBlur,	'procedure SimbaExt.imGaussBlur(const ImgArr:TIntMatrix; var Dest:TIntMatrix; Radius:Int32; Sigma:Single);');
AddMethod(@exp_ImBlend,		'function SimbaExt.imBlend(const ImgArr1,ImgArr2: TIntMatrix; Alpha: Single): TIntMatrix;');
AddMethod(@exp_ImCompareAt,	'function SimbaExt.imCompareAt(const large,small:TIntMatrix; pt:TPoint; tol:Int32): Single;');
AddMethod(@exp_ImResize,	'procedure SimbaExt.imResize(var ImgArr:TIntMatrix; NewW, NewH: Int32; Method:EResizeAlgo);');
AddMethod(@exp_ImSample,	'function SimbaExt.imSample(ImgArr:TIntMatrix; Downscale:Int32): TIntMatrix;');
AddMethod(@exp_ImRotate,	'function SimbaExt.imRotate(const Mat:TIntMatrix; Angle:Single; Expand:LongBool; Bilinear:LongBool=True): TIntMatrix;');
