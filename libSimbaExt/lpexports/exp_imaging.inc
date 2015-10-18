{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports imaging methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@exp_GaussKernel,	'function SimbaExt.GaussKernel(kernelRadius:Integer; sigma:Single): TFloatMatrix;');
AddMethod(@exp_Grayscale,	'procedure SimbaExt.Grayscale(const image:T2DIntArray; var dest:T2DIntArray);');
AddMethod(@exp_Blur,		'procedure SimbaExt.Blur(const image:TIntMatrix; var dest:TIntMatrix; radius:Int32);');
AddMethod(@exp_MedianBlur,	'function SimbaExt.MedianBlur(const image: TIntMatrix; radius:Integer): TIntMatrix;');
AddMethod(@exp_Brighten,	'function SimbaExt.Brighten(const image:TIntMatrix; amount:Extended): TIntMatrix;');
AddMethod(@exp_Threshold,	'procedure SimbaExt.Threshold(var image:TIntMatrix; thresh:Byte; Alpha, Beta:Int32);');
AddMethod(@exp_ThresholdAdap,	'procedure SimbaExt.ThresholdAdaptive(var image:TIntMatrix; alpha, beta: Int32; method:EThreshAlgo; c:Int32=0);');
AddMethod(@exp_FindContours,	'function SimbaExt.FindContours(const image: TIntMatrix; thresh: Int32): T2DPointArray;');
AddMethod(@exp_CEdges,		'function SimbaExt.CEdges(const image: TIntMatrix; minDiff: Int32): TPointArray;');
AddMethod(@exp_Sobel,		'function SimbaExt.Sobel(const image: TIntMatrix; axis:Int8=-1): TIntMatrix;');
AddMethod(@exp_Convolve,	'function SimbaExt.Convolve(const image:TIntMatrix; const mask:TFloatMatrix): TIntMatrix;');
AddMethod(@exp_GaussBlur,	'procedure SimbaExt.GaussBlur(const image:TIntMatrix; var dest:TIntMatrix; radius:Int32; sigma:Single);');
AddMethod(@exp_Blend,		'function SimbaExt.Blend(const image1,image2: TIntMatrix; alpha: Single): TIntMatrix;');
AddMethod(@exp_CompareAt,	'function SimbaExt.CompareAt(const large,small:TIntMatrix; pt:TPoint; tol:Int32): Single;');
AddMethod(@exp_Resize,		'procedure SimbaExt.Resize(var image:TIntMatrix; newW, newH: Int32; method:EResizeAlgo);');
AddMethod(@exp_Sample,		'function SimbaExt.Sample(image:TIntMatrix; downscale:Int32): TIntMatrix;');
AddMethod(@exp_Rotate,		'function SimbaExt.Rotate(const Mat:TIntMatrix; angle:Single; expand:LongBool; bilinear:LongBool=True): TIntMatrix;');
