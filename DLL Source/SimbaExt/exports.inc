{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

AddMethod(@PopException, 'function exp_GetException(var Msg:String): Boolean;');

// CoreMath.pas
AddMethod(@exp_DistManhattan,	'function exp_DistManhattan(pt1,pt2: TPoint): Extended;');
AddMethod(@exp_DistEuclidean,	'function exp_DistEuclidean(pt1,pt2: TPoint): Extended;');
AddMethod(@exp_DistChebyshev,	'function exp_DistChebyshev(pt1,pt2: TPoint): Extended;');
AddMethod(@exp_DistOctagonal,	'function exp_DistOctagonal(pt1,pt2: TPoint): Extended;');
AddMethod(@exp_DistToLine,     'function exp_DistToLine(Pt, sA, sB: TPoint): Extended;');
AddMethod(@exp_Modulo,	    'function exp_Modulo(X,Y:Extended): Extended;');
AddMethod(@exp_IModulo,	  'function exp_IModulo(X,Y:Integer): Integer;');
AddMethod(@exp_InCircle,	  'function exp_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;');
AddMethod(@exp_InEllipse,	'function exp_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;');
AddMethod(@exp_InRect,	    'function exp_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;');
AddMethod(@exp_InPoly,	    'function exp_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;');
AddMethod(@exp_InPolyR,	  'function exp_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;');
AddMethod(@exp_InPolyW,	  'function exp_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;');
AddMethod(@exp_DeltaAngle,	'function exp_DeltaAngle(DegA,DegB:Extended): Extended;');
AddMethod(@exp_Min3f,	'function Min(X,Y,Z:Extended): Extended; overload;');
AddMethod(@exp_Min3i,	'function Min(X,Y,Z:Int64): Int64; overload;');
AddMethod(@exp_Max3f,	'function Max(X,Y,Z:Extended): Extended; overload;');
AddMethod(@exp_Max3i,	'function Max(X,Y,Z:Int64): Int64; overload;');

// Numeric.pas
AddMethod(@SumPtr,         'function exp_SumPtr(Ptr:PChar; Size:UInt8; Len:LongInt; Signed:Boolean=False): Int64;');
AddMethod(@SumFPtr,        'function exp_SumFPtr(Ptr:PChar; Size:UInt8; Len:LongInt): Extended;');
AddMethod(@MinMaxPtr,        'procedure exp_MinMaxPtr(Ptr:PChar; Size:UInt8; Len:LongInt; Signed:Boolean; var Min,Max:Int64);');
AddMethod(@MinMaxFPtr,       'procedure exp_MinMaxFPtr(Ptr:PChar; Size:UInt8; Len:LongInt; var Min,Max:Extended);');
AddMethod(@exp_SumTBA,	    'function exp_SumTBA(const Arr: TByteArray): Int64;');
AddMethod(@exp_SumTIA,	    'function exp_SumTIA(const Arr: TIntArray): Int64;');
AddMethod(@exp_SumTEA,	    'function exp_SumTEA(const Arr: TExtArray): Extended;');
AddMethod(@exp_MinMaxTBA,	'procedure exp_MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte);');
AddMethod(@exp_MinMaxTIA,	'procedure exp_MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer);');
AddMethod(@exp_MinMaxTEA,	'procedure exp_MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended);');
AddMethod(@exp_TIACombinations,	'function exp_TIACombinations(const Arr: TIntArray; Seq:Integer): T2DIntArray;');
AddMethod(@exp_TEACombinations,	'function exp_TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray;');


// Std.pas
(** Array slicing **)
AddMethod(@exp_Slice1,  'function exp_Slice(Arr:TIntArray; Start,Stop,Step:Int32): TIntArray; overload;');
AddMethod(@exp_Slice2,  'function exp_Slice(Arr:TExtArray; Start,Stop,Step:Int32): TExtArray; overload;');
AddMethod(@exp_Slice3,  'function exp_Slice(Arr:TFloatArray; Start,Stop,Step:Int32): TFloatArray; overload;');
AddMethod(@exp_Slice4,  'function exp_Slice(Arr:TDoubleArray; Start,Stop,Step:Int32): TDoubleArray; overload;');
AddMethod(@exp_Slice5,  'function exp_Slice(Arr:TPointArray; Start,Stop,Step:Int32): TPointArray; overload;');
AddMethod(@exp_Slice6,  'function exp_Slice(Arr:TByteArray; Start,Stop,Step:Int32): TByteArray; overload;');
AddMethod(@exp_Slice7,  'function exp_Slice(Arr:TBoxArray; Start,Stop,Step:Int32): TBoxArray; overload;');
AddMethod(@exp_Slice8,  'function exp_Slice(Arr:String; Start,Stop,Step:Int32): String; overload;');
AddMethod(@exp_Slice9,  'function exp_Slice(Arr:T2DIntArray; Start,Stop,Step:Int32): T2DIntArray; overload;');
AddMethod(@exp_Slice10, 'function exp_Slice(Arr:T2DExtArray; Start,Stop,Step:Int32): T2DExtArray; overload;');
AddMethod(@exp_Slice11, 'function exp_Slice(Arr:T2DFloatArray; Start,Stop,Step:Int32): T2DFloatArray; overload;');
AddMethod(@exp_Slice12, 'function exp_Slice(Arr:T2DDoubleArray; Start,Stop,Step:Int32): T2DDoubleArray; overload;');
AddMethod(@exp_Slice13, 'function exp_Slice(Arr:T2DPointArray; Start,Stop,Step:Int32): T2DPointArray; overload;');
AddMethod(@exp_Slice14, 'function exp_Slice(Arr:T2DByteArray; Start,Stop,Step:Int32): T2DByteArray; overload;');
AddMethod(@exp_Slice15, 'function exp_Slice(Arr:T2DBoxArray; Start,Stop,Step:Int32): T2DBoxArray; overload;');
AddMethod(@exp_Slice16, 'function exp_Slice(Arr:TStringArray; Start,Stop,Step:Int32): TStringArray; overload;');

(** Array pos **)
AddMethod(@exp_Find1, 'function exp_Find(Arr:TIntArray; Seq:TIntArray): Int32; overload;');
AddMethod(@exp_Find2, 'function exp_Find(Arr:TExtArray; Seq:TExtArray): Int32; overload;');
AddMethod(@exp_Find3, 'function exp_Find(Arr:TFloatArray; Seq:TFloatArray): Int32; overload;');
AddMethod(@exp_Find4, 'function exp_Find(Arr:TDoubleArray; Seq:TDoubleArray): Int32; overload;');
AddMethod(@exp_Find5, 'function exp_Find(Arr:TPointArray; Seq:TPointArray): Int32; overload;');
AddMethod(@exp_Find6, 'function exp_Find(Arr:TByteArray; Seq:TByteArray): Int32; overload;');
AddMethod(@exp_Find7, 'function exp_Find(Arr:TBoxArray; Seq:TBoxArray): Int32; overload;');
AddMethod(@exp_Find8, 'function exp_Find(Arr:String; Seq:String): Int32; overload;');
AddMethod(@exp_Find9, 'function exp_Find(Arr:TStringArray; Seq:TStringArray): Int32; overload;');

(** Array pos multi **)
AddMethod(@exp_FindAll1, 'function exp_FindAll(Arr:TIntArray; Seq:TIntArray): TIntArray; overload;');
AddMethod(@exp_FindAll2, 'function exp_FindAll(Arr:TExtArray; Seq:TExtArray): TIntArray; overload;');
AddMethod(@exp_FindAll3, 'function exp_FindAll(Arr:TFloatArray; Seq:TFloatArray): TIntArray; overload;');
AddMethod(@exp_FindAll4, 'function exp_FindAll(Arr:TDoubleArray; Seq:TDoubleArray): TIntArray; overload;');
AddMethod(@exp_FindAll5, 'function exp_FindAll(Arr:TPointArray; Seq:TPointArray): TIntArray; overload;');
AddMethod(@exp_FindAll6, 'function exp_FindAll(Arr:TByteArray; Seq:TByteArray): TIntArray; overload;');
AddMethod(@exp_FindAll7, 'function exp_FindAll(Arr:TBoxArray; Seq:TBoxArray): TIntArray; overload;');
AddMethod(@exp_FindAll8, 'function exp_FindAll(Arr:String; Seq:String): TIntArray; overload;');
AddMethod(@exp_FindAll9, 'function exp_FindAll(Arr:TStringArray; Seq:TStringArray): TIntArray; overload;');

// Sorting.pas
AddMethod(@exp_SortTBA,	'procedure exp_SortTBA(var Arr: TByteArray);');
AddMethod(@exp_SortTIA,	'procedure exp_SortTIA(var Arr: TIntArray);');
AddMethod(@exp_SortTFA,	'procedure exp_SortTFA(var Arr: TFloatArray);');
AddMethod(@exp_SortTDA,	'procedure exp_SortTDA(var Arr: TDoubleArray);');
AddMethod(@exp_SortTEA,	'procedure exp_SortTEA(var Arr: TExtendedArray);');
AddMethod(@exp_SortTPA,	'procedure exp_SortTPA(var Arr: TPointArray);');
AddMethod(@exp_SortTPAFrom,	'procedure exp_SortTPAFrom(var Arr: TPointArray; const From:TPoint);');
AddMethod(@exp_SortTPAByRow,	'procedure exp_SortTPAByRow(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByColumn,	'procedure exp_SortTPAByColumn(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByX,           'procedure exp_SortTPAByX(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByY,           'procedure exp_SortTPAByY(var Arr: TPointArray);');
AddMethod(@exp_SortTSA,              'procedure exp_SortTSA(var Arr: TStringArray; CaseInsesitive:Boolean=False);');
AddMethod(@exp_SortTSANatural,	'procedure exp_SortTSANatural(var Arr: TStringArray);');
AddMethod(@exp_SortATPAByLength,	'procedure exp_SortATPAByLength(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByMean,	'procedure exp_SortATPAByMean(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByFirst,	'procedure exp_SortATPAByFirst(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByIndex,	'procedure exp_SortATPAByIndex(var Arr:T2DPointArray; index:Int32);');
AddMethod(@exp_SortATBAByLength,	'procedure exp_SortATBAByLength(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByMean,	'procedure exp_SortATBAByMean(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByFirst,	'procedure exp_SortATBAByFirst(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByIndex,	'procedure exp_SortATBAByIndex(var Arr:T2DByteArray; index:Int32);');
AddMethod(@exp_SortATIAByLength,	'procedure exp_SortATIAByLength(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByMean,	'procedure exp_SortATIAByMean(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByFirst,	'procedure exp_SortATIAByFirst(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByIndex,	'procedure exp_SortATIAByIndex(var Arr:T2DIntArray; index:Int32);');
AddMethod(@exp_SortATEAByLength,	'procedure exp_SortATEAByLength(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByMean,	'procedure exp_SortATEAByMean(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByFirst,	'procedure exp_SortATEAByFirst(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByIndex,	'procedure exp_SortATEAByIndex(var Arr:T2DExtArray; index:Int32);');


// Finder.pas
AddMethod(@exp_MatchColorRGB,	'function exp_MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
AddMethod(@exp_MatchColorXYZ,	'function exp_MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
AddMethod(@exp_MatchColorLAB,	'function exp_MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
AddMethod(@exp_MatchColorLCH,	'function exp_MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');

AddMethod(@exp_ImFindColorTolEx,	'function exp_ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean;');
AddMethod(@exp_ImFindColorsTolEx,	'function exp_ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;');
AddMethod(@exp_ImFindColorTolExLCH,	'function exp_ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');
AddMethod(@exp_ImFindColorTolExLAB,	'function exp_ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');


// PointTools.pas and related
AddMethod(@exp_ScalePoint,	'function exp_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;');
AddMethod(@exp_SumTPA,	    'function exp_SumTPA(const TPA: TPointArray): TPoint;');
AddMethod(@exp_TPASplitAxis,	'procedure exp_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntArray);');
AddMethod(@exp_TPAJoinAxis,	'procedure exp_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);');
AddMethod(@exp_TPAFilter,	'procedure exp_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);');
AddMethod(@exp_TPAFilterBounds,	'procedure exp_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);');
AddMethod(@exp_ATPAFilter,	'procedure exp_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);');
AddMethod(@exp_TPAExtremes,	'function exp_TPAExtremes(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_TPABBox,	    'function exp_TPABBox(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_TPACenter,	'function exp_TPACenter(const TPA: TPointArray; Method: TCenterAlgo; Inside:Boolean): TPoint;');
AddMethod(@exp_GetAdjacent,	'procedure exp_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);');
AddMethod(@exp_TPACircularity,	'function exp_TPACircularity(const TPA: TPointArray): Extended;');
AddMethod(@exp_TPAConvexity,		'function exp_TPAConvexity(const TPA: TPointArray): Extended;');
AddMethod(@exp_ReverseTPA,	'procedure exp_ReverseTPA(var TPA: TPointArray);');
AddMethod(@exp_TPARemoveDupes,	'procedure exp_TPARemoveDupes(var TPA: TPointArray);');
AddMethod(@exp_LongestPolyVector,	'procedure exp_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);');
AddMethod(@exp_InvertTPA,	  'function exp_InvertTPA(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_RotateTPA,	'function exp_RotateTPA(const TPA: TPointArray; Radians: Extended): TPointArray;');
AddMethod(@exp_TPAPartition,	'function exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;');
AddMethod(@exp_AlignTPA,	    'function exp_AlignTPA(const TPA:TPointArray; Method: TAlignAlgo; var Angle:Extended): TPointArray;');
AddMethod(@exp_CleanSortTPA,	'function exp_CleanSortTPA(const TPA: TPointArray): TPointArray;');
AddMethod(@exp_UniteTPA,	    'function exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray;');
AddMethod(@exp_TPALine,	    'procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);');
AddMethod(@exp_ConnectTPA,	'function exp_ConnectTPA(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_ConnectTPAEx,	'function exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray;');
AddMethod(@exp_XagonPoints,	'function exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
AddMethod(@exp_TPAEllipseBase,	'function exp_TPAEllipseBase(const Center: TPoint; RadiusX, RadiusY:Integer): TPointArray;');
AddMethod(@exp_TPAEllipse,	'function exp_TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean): TPointArray;');
AddMethod(@exp_TPACircle,	'function exp_TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean): TPointArray;');
AddMethod(@exp_TPASimplePoly,'function exp_TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
AddMethod(@exp_ConvexHull,	'function exp_ConvexHull(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_FloodFillTPAEx,'function exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean): TPointArray;');
AddMethod(@exp_FloodFillTPA,	'function exp_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;');
AddMethod(@exp_TPAOutline,	'function exp_TPAOutline(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_TPABorder,	'function exp_TPABorder(const TPA:TPointArray): TPointArray;');
AddMethod(@exp_FloodFillPolygon,	'function exp_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray;');
AddMethod(@exp_ClusterTPAEx,	'function exp_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;');
AddMethod(@exp_ClusterTPA,	'function exp_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;');
AddMethod(@exp_TPAEdges,	'function exp_TPAEdges(const TPA: TPointArray): TPointArray;');

AddMethod(@exp_TPASkeleton,	'function exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray;');
AddMethod(@exp_TPAReduce,	'function exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;');
AddMethod(@exp_TPAExpand,	'function exp_TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray;');

AddMethod(@exp_Spline,	'function exp_Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean): TPointArray;');


// MatrixTools.pas
AddMethod(@exp_NewMatrixEx,	'function exp_NewMatrixEx(W,H, Init:Integer): T2DIntArray;');
AddMethod(@exp_NewMatrix,	'function exp_NewMatrix(W,H:Integer): T2DIntArray;');
AddMethod(@exp_TPAToMatrixEx,'function exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray;');
AddMethod(@exp_TPAToMatrix,	'function exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray;');
AddMethod(@exp_MatFromTIA,	'function exp_MatFromTIA(const Arr:TIntArray; Width,Height:Integer): T2DIntArray;');
AddMethod(@exp_PadMatrix,	'procedure exp_PadMatrix(var Matrix:T2DIntArray; HPad,WPad:Integer);');
AddMethod(@exp_FloodFillMatrix,	'function exp_FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean): TPointArray;');


// Imaging.pas
AddMethod(@exp_GaussKernel,	  'function exp_GaussKernel(KernelRadius:Integer; Sigma:Single): T2DFloatArray;');
AddMethod(@exp_ImBlur,	  'function exp_ImBlur(ImgArr: T2DIntArray; Radius:Integer): T2DIntArray;');
AddMethod(@exp_ImMedianBlur,   'function exp_ImMedianBlur(ImgArr: T2DIntArray; Radius:Integer): T2DIntArray;');
AddMethod(@exp_ImBrighten,     'function exp_ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean): T2DIntArray;');
AddMethod(@exp_ImEnhance,      'function exp_ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended): T2DIntArray;');
AddMethod(@exp_ImThreshold,	  'function exp_ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean): T2DIntArray;');
AddMethod(@exp_ImThresholdAdaptive,	'function exp_ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer): T2DIntArray;');
AddMethod(@exp_ImFindContours,       'function exp_ImFindContours(const ImgArr:T2DIntegerArray; Outlines:Boolean): T2DPointArray;');
AddMethod(@exp_ImCEdges,	        'function exp_ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray;');
AddMethod(@exp_ImSobel,          'function exp_ImSobel(const ImgArr: T2DIntArray): T2DIntArray;');
AddMethod(@exp_ImConvolve,       'function exp_ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;');
AddMethod(@exp_ImGaussBlur,      'procedure exp_ImGaussBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32; Sigma:Single);');
AddMethod(@exp_ImBlend,          'function exp_ImBlend(ImgArr1,ImgArr2: T2DIntArray; Alpha: Single): T2DIntArray;');
AddMethod(@exp_ImResize,         'procedure exp_ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo);');
AddMethod(@exp_ImRotate,         'function exp_ImRotate(Mat:T2DIntArray; Angle:Single; Expand:Boolean; Bilinear:Boolean=True): T2DIntArray;');


// SimpleOCR.pas
AddMethod(@exp_ImGetText,     'function exp_ImGetText(ImgArr:T2DIntArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString;');


// Randomize.pas
AddMethod(@exp_RandomTPA,	'function exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;');
AddMethod(@exp_RandomCenterTPA,	'function exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray;');
AddMethod(@exp_RandomTIA,	'function exp_RandomTIA(Amount:Integer; Low,Hi:Integer): TIntArray;');


// StringTools.pas
AddMethod(@exp_StrStrip,	    'function exp_StrStrip(const Text, Chars: String): String;');
AddMethod(@exp_StrStripL,	    'function exp_StrStripL(const Text, Chars: String): String;');
AddMethod(@exp_StrStripR,	    'function exp_StrStripR(const Text, Chars: String): String;');
AddMethod(@exp_StrPosEx,	    'function exp_StrPosEx(const SubStr, Text:String): TIntegerArray;');
AddMethod(@exp_StrPosL,          'function exp_StrPosL(const SubStr, Text: String): Integer;');
AddMethod(@exp_StrPosR,          'function exp_StrPosR(const SubStr, Text: String): Integer;');
AddMethod(@exp_StrReplace,       'function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;');
AddMethod(@exp_StrExplode,       'function exp_StrExplode(const Text, Sep: String): TStringArray;');


// CornerDet.pas
AddMethod(@exp_CornerResponse,        'function exp_CornerResponse(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer): T2DFloatArray;');
AddMethod(@exp_FindCornerPoints,      'function exp_FindCornerPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;');
AddMethod(@exp_FindCornerMidPoints,   'function exp_FindCornerMidPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;');


// MatrixOps.pas
AddMethod(@exp_IndicesB, 'function exp_Indices(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator): TPointArray;');
AddMethod(@exp_IndicesI, 'function exp_Indices(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesE, 'function exp_Indices(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesD, 'function exp_Indices(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesF, 'function exp_Indices(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesExB, 'function exp_Indices(const Mat:T2DByteArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesExI, 'function exp_Indices(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesExE, 'function exp_Indices(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesExD, 'function exp_Indices(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator): TPointArray; overload;');
AddMethod(@exp_IndicesExF, 'function exp_Indices(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator): TPointArray; overload;');

AddMethod(@exp_MinMaxB, 'procedure exp_MinMax(Mat:T2DByteArray; var Min, Max:Byte);');
AddMethod(@exp_MinMaxI, 'procedure exp_MinMax(Mat:T2DIntArray; var Min, Max:Integer); overload;');
AddMethod(@exp_MinMaxE, 'procedure exp_MinMax(Mat:T2DExtArray; var Min, Max:Extended); overload;');
AddMethod(@exp_MinMaxD, 'procedure exp_MinMax(Mat:T2DDoubleArray; var Min, Max:Double); overload;');
AddMethod(@exp_MinMaxF, 'procedure exp_MinMax(Mat:T2DFloatArray; var Min, Max:Single); overload;');

AddMethod(@exp_ArgMaxB, 'function exp_ArgMax(Mat:T2DByteArray): TPoint;');
AddMethod(@exp_ArgMaxI, 'function exp_ArgMax(Mat:T2DIntArray): TPoint; overload;');
AddMethod(@exp_ArgMaxE, 'function exp_ArgMax(Mat:T2DExtArray): TPoint; overload;');
AddMethod(@exp_ArgMaxD, 'function exp_ArgMax(Mat:T2DDoubleArray): TPoint; overload;');
AddMethod(@exp_ArgMaxF, 'function exp_ArgMax(Mat:T2DFloatArray): TPoint; overload;');
AddMethod(@exp_ArgMinB, 'function exp_ArgMin(Mat:T2DByteArray): TPoint; overload;');
AddMethod(@exp_ArgMinI, 'function exp_ArgMin(Mat:T2DIntArray): TPoint; overload;');
AddMethod(@exp_ArgMinE, 'function exp_ArgMin(Mat:T2DExtArray): TPoint; overload;');
AddMethod(@exp_ArgMinD, 'function exp_ArgMin(Mat:T2DDoubleArray): TPoint; overload;');
AddMethod(@exp_ArgMinF, 'function exp_ArgMin(Mat:T2DFloatArray): TPoint; overload;');

AddMethod(@exp_ArgMaxExB, 'function exp_ArgMax(Mat:T2DByteArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMaxExI, 'function exp_ArgMax(Mat:T2DIntArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMaxExE, 'function exp_ArgMax(Mat:T2DExtArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMaxExD, 'function exp_ArgMax(Mat:T2DDoubleArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMaxExF, 'function exp_ArgMax(Mat:T2DFloatArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMinExB, 'function exp_ArgMin(Mat:T2DByteArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMinExI, 'function exp_ArgMin(Mat:T2DIntArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMinExE, 'function exp_ArgMin(Mat:T2DExtArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMinExD, 'function exp_ArgMin(Mat:T2DDoubleArray; B:TBox): TPoint; overload;');
AddMethod(@exp_ArgMinExF, 'function exp_ArgMin(Mat:T2DFloatArray; B:TBox): TPoint; overload;');

AddMethod(@exp_VarMultiB, 'function exp_VarMulti(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean):TByteArray;');
AddMethod(@exp_VarMultiI, 'function exp_VarMulti(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TIntArray; overload;');
AddMethod(@exp_VarMultiE, 'function exp_VarMulti(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TExtArray; overload;');
AddMethod(@exp_VarMultiD, 'function exp_VarMulti(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TDoubleArray; overload;');
AddMethod(@exp_VarMultiF, 'function exp_VarMulti(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TFloatArray; overload;');

AddMethod(@exp_ArgMultiB, 'function exp_ArgMulti(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean): TPointArray;');
AddMethod(@exp_ArgMultiI, 'function exp_ArgMulti(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
AddMethod(@exp_ArgMultiE, 'function exp_ArgMulti(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
AddMethod(@exp_ArgMultiD, 'function exp_ArgMulti(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
AddMethod(@exp_ArgMultiF, 'function exp_ArgMulti(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');

AddMethod(@exp_GetValuesB, 'function exp_GetValues(const Mat:T2DByteArray; const Indices:TPointArray): TByteArray;');
AddMethod(@exp_GetValuesI, 'function exp_GetValues(const Mat:T2DIntArray; const Indices:TPointArray): TIntArray; overload;');
AddMethod(@exp_GetValuesF, 'function exp_GetValues(const Mat:T2DFloatArray; const Indices:TPointArray): TFloatArray; overload;');
AddMethod(@exp_GetValuesD, 'function exp_GetValues(const Mat:T2DDoubleArray; const Indices:TPointArray): TDoubleArray; overload;');
AddMethod(@exp_GetValuesE, 'function exp_GetValues(const Mat:T2DExtArray; const Indices:TPointArray): TExtArray; overload;');

AddMethod(@exp_PutValuesB, 'procedure exp_PutValues(var Matrix:T2DByteArray; const Indices:TPointArray; Values:TByteArray);');
AddMethod(@exp_PutValuesI, 'procedure exp_PutValues(var Matrix:T2DIntArray; const Indices:TPointArray; Values:TIntArray); overload;');
AddMethod(@exp_PutValuesF, 'procedure exp_PutValues(var Matrix:T2DFloatArray; const Indices:TPointArray; Values:TFloatArray); overload;');
AddMethod(@exp_PutValuesD, 'procedure exp_PutValues(var Matrix:T2DDoubleArray; const Indices:TPointArray; Values:TDoubleArray); overload;');
AddMethod(@exp_PutValuesE, 'procedure exp_PutValues(var Matrix:T2DExtArray; const Indices:TPointArray; Values:TExtArray); overload;');

AddMethod(@exp_CombineMatB, 'function exp_CombineMatrix(const Mat1, Mat2:T2DByteArray; Op:Char): T2DByteArray;');
AddMethod(@exp_CombineMatI, 'function exp_CombineMatrix(const Mat1, Mat2:T2DIntArray; Op:Char): T2DIntArray; overload;');
AddMethod(@exp_CombineMatF, 'function exp_CombineMatrix(const Mat1, Mat2:T2DFloatArray; Op:Char): T2DFloatArray; overload;');
AddMethod(@exp_CombineMatD, 'function exp_CombineMatrix(const Mat1, Mat2:T2DDoubleArray; Op:Char): T2DDoubleArray; overload;');
AddMethod(@exp_CombineMatE, 'function exp_CombineMatrix(const Mat1, Mat2:T2DExtArray; Op:Char): T2DExtArray; overload;');

AddMethod(@exp_NormalizeMatB, 'function exp_Normalize(const Mat:T2DByteArray; Alpha, Beta: Byte): T2DByteArray;');
AddMethod(@exp_NormalizeMatI, 'function exp_Normalize(const Mat:T2DIntArray; Alpha, Beta: Int32): T2DIntArray; overload;');
AddMethod(@exp_NormalizeMatF, 'function exp_Normalize(const Mat:T2DFloatArray; Alpha, Beta: Single): T2DFloatArray; overload;');
AddMethod(@exp_NormalizeMatD, 'function exp_Normalize(const Mat:T2DDoubleArray; Alpha, Beta: Double): T2DDoubleArray; overload;');
AddMethod(@exp_NormalizeMatE, 'function exp_Normalize(const Mat:T2DExtArray; Alpha, Beta: Extended): T2DExtArray; overload;');

AddMethod(@exp_GetAreaB, 'function exp_GetArea(const Mat:T2DByteArray; X1,Y1,X2,Y2:Integer): T2DByteArray;');
AddMethod(@exp_GetAreaI, 'function exp_GetArea(const Mat:T2DIntArray; X1,Y1,X2,Y2:Integer): T2DIntArray; overload;');
AddMethod(@exp_GetAreaF, 'function exp_GetArea(const Mat:T2DFloatArray; X1,Y1,X2,Y2:Integer): T2DFloatArray; overload;');
AddMethod(@exp_GetAreaD, 'function exp_GetArea(const Mat:T2DDoubleArray; X1,Y1,X2,Y2:Integer): T2DDoubleArray; overload;');
AddMethod(@exp_GetAreaE, 'function exp_GetArea(const Mat:T2DExtArray; X1,Y1,X2,Y2:Integer): T2DExtArray; overload;');

AddMethod(@exp_GetColsB, 'function exp_GetCols(const Mat:T2DByteArray; FromCol, ToCol:Int32): T2DByteArray;');
AddMethod(@exp_GetColsI, 'function exp_GetCols(const Mat:T2DIntArray; FromCol, ToCol:Int32): T2DIntArray; overload;');
AddMethod(@exp_GetColsF, 'function exp_GetCols(const Mat:T2DFloatArray; FromCol, ToCol:Int32): T2DFloatArray; overload;');
AddMethod(@exp_GetColsD, 'function exp_GetCols(const Mat:T2DDoubleArray; FromCol, ToCol:Int32): T2DDoubleArray; overload;');
AddMethod(@exp_GetColsE, 'function exp_GetCols(const Mat:T2DExtArray; FromCol, ToCol:Int32): T2DExtArray; overload;');

AddMethod(@exp_GetRowsB, 'function exp_GetRows(const Mat:T2DByteArray; FromRow, ToRow:Int32): T2DByteArray;');
AddMethod(@exp_GetRowsI, 'function exp_GetRows(const Mat:T2DIntArray; FromRow, ToRow:Int32): T2DIntArray; overload;');
AddMethod(@exp_GetRowsF, 'function exp_GetRows(const Mat:T2DFloatArray; FromRow, ToRow:Int32): T2DFloatArray; overload;');
AddMethod(@exp_GetRowsD, 'function exp_GetRows(const Mat:T2DDoubleArray; FromRow, ToRow:Int32): T2DDoubleArray; overload;');
AddMethod(@exp_GetRowsE, 'function exp_GetRows(const Mat:T2DExtArray; FromRow, ToRow:Int32): T2DExtArray; overload;');

AddMethod(@exp_FlipMatB, 'function exp_Flip(const Mat:T2DByteArray): T2DByteArray;');
AddMethod(@exp_FlipMatI, 'function exp_Flip(const Mat:T2DIntArray): T2DIntArray; overload;');
AddMethod(@exp_FlipMatF, 'function exp_Flip(const Mat:T2DFloatArray): T2DFloatArray; overload;');
AddMethod(@exp_FlipMatD, 'function exp_Flip(const Mat:T2DDoubleArray): T2DDoubleArray; overload;');
AddMethod(@exp_FlipMatE, 'function exp_Flip(const Mat:T2DExtArray): T2DExtArray; overload;');
