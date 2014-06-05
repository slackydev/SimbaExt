library SimbaExt;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

{$mode objfpc}{$H+}
{$macro on}
{$inline on}

uses
  SysUtils,
  Classes,
  Windows,
  Math,

  CoreTypes,
  CoreMath,
  CoreMisc,
  MatrixMath,
  MatrixOps,
  Std,
  PointList,
  Sorting,
  MatrixTools,
  ColorMath,
  HashMap,
  Numeric,
  Imaging,
  Randomize,
  PointTools,
  Finder,
  SimpleOCR,
  Spline,
  Morphology,
  DensityMap,
  StringTools,
  CornerDet,
  _Tests;

//Include Simba Wrapper
{$I ExportWrap.pas}

type
  TCommand = record
    procAddr: Pointer;
    procDef: PChar;
  end;

var
  commands: array of TCommand;
  commandsLoaded: Boolean;
  OldMemoryManager: TMemoryManager;
  memisset: Boolean = False;


procedure AddCommand(procAddr: Pointer; procDef: PChar);
var L: Integer;
begin
  L := Length(commands);
  SetLength(commands, (l + 1));
  commands[l].procAddr := procAddr;
  commands[l].procDef := procDef;
end;


{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=  THIS GOES OUT OF OUR PLUGIN  =-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure SetupCommands;
begin
  // CoreMath.pas
  AddCommand(@exp_DistManhattan,	'function exp_DistManhattan(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistEuclidean,	'function exp_DistEuclidean(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistChebyshev,	'function exp_DistChebyshev(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistOctagonal,	'function exp_DistOctagonal(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistToLine,     'function exp_DistToLine(Pt, sA, sB: TPoint): Extended;');
  AddCommand(@exp_Modulo,	    'function exp_Modulo(X,Y:Extended): Extended;');
  AddCommand(@exp_IModulo,	  'function exp_IModulo(X,Y:Integer): Integer;');
  AddCommand(@exp_InCircle,	  'function exp_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;');
  AddCommand(@exp_InEllipse,	'function exp_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;');
  AddCommand(@exp_InRect,	    'function exp_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;');
  AddCommand(@exp_InPoly,	    'function exp_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyR,	  'function exp_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyW,	  'function exp_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_DeltaAngle,	'function exp_DeltaAngle(DegA,DegB:Extended): Extended;');



  // Numeric.pas
  AddCommand(@SumPtr,         'function exp_SumPtr(Ptr:PChar; Size:UInt8; Len:LongInt; Signed:Boolean=False): Int64;');
  AddCommand(@SumFPtr,        'function exp_SumFPtr(Ptr:PChar; Size:UInt8; Len:LongInt): Extended;');
  AddCommand(@exp_SumTBA,	    'function exp_SumTBA(const Arr: TByteArray): Int64;');
  AddCommand(@exp_SumTIA,	    'function exp_SumTIA(const Arr: TIntArray): Int64;');
  AddCommand(@exp_SumTEA,	    'function exp_SumTEA(const Arr: TExtArray): Extended;');
  AddCommand(@exp_TIACombinations,	'function exp_TIACombinations(const Arr: TIntArray; Seq:Integer): T2DIntArray;');
  AddCommand(@exp_TEACombinations,	'function exp_TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray;');
  AddCommand(@exp_MinMaxTBA,	'procedure exp_MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte);');
  AddCommand(@exp_MinMaxTIA,	'procedure exp_MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer);');
  AddCommand(@exp_MinMaxTEA,	'procedure exp_MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended);');


  // Std.pas
  AddCommand(@exp_Slice1,	'function exp_Slice(Arr:TIntArray; Start,Stop,Step:Int32): TIntArray;');
  AddCommand(@exp_Slice2,	'function exp_Slice(Arr:TExtArray; Start,Stop,Step:Int32): TExtArray; overload;');
  AddCommand(@exp_Slice3,	'function exp_Slice(Arr:TPointArray; Start,Stop,Step:Int32): TPointArray; overload;');
  AddCommand(@exp_Slice4,	'function exp_Slice(Arr:TByteArray; Start,Stop,Step:Int32): TByteArray; overload;');
  AddCommand(@exp_Slice5,	'function exp_Slice(Arr:TBoxArray; Start,Stop,Step:Int32): TBoxArray; overload;');
  AddCommand(@exp_Slice6,	'function exp_Slice(Arr:String; Start,Stop,Step:Int32): String; overload;');
  AddCommand(@exp_Slice7,	'function exp_Slice(Arr:T2DIntArray; Start,Stop,Step:Int32): T2DIntArray; overload;');
  AddCommand(@exp_Slice8,	'function exp_Slice(Arr:T2DExtArray; Start,Stop,Step:Int32): T2DExtArray; overload;');
  AddCommand(@exp_Slice9,	'function exp_Slice(Arr:T2DPointArray; Start,Stop,Step:Int32): T2DPointArray; overload;');
  AddCommand(@exp_Slice10,	'function exp_Slice(Arr:T2DByteArray; Start,Stop,Step:Int32): T2DByteArray; overload;');
  AddCommand(@exp_Slice11,	'function exp_Slice(Arr:T2DBoxArray; Start,Stop,Step:Int32): T2DBoxArray; overload;');
  AddCommand(@exp_Slice12,	'function exp_Slice(Arr:TStringArray; Start,Stop,Step:Int32): TStringArray; overload;');

  AddCommand(@exp_Find1,	'function exp_Find(Arr:TIntArray; Seq:TIntArray): Int32;');
  AddCommand(@exp_Find2,	'function exp_Find(Arr:TExtArray; Seq:TExtArray): Int32; overload;');
  AddCommand(@exp_Find3,	'function exp_Find(Arr:TPointArray; Seq:TPointArray): Int32; overload;');
  AddCommand(@exp_Find4,	'function exp_Find(Arr:TByteArray; Seq:TByteArray): Int32; overload;');
  AddCommand(@exp_Find5,	'function exp_Find(Arr:TBoxArray; Seq:TBoxArray): Int32; overload;');
  AddCommand(@exp_Find6,	'function exp_Find(Arr:String; Seq:String): Int32; overload;');

  AddCommand(@exp_FindAll1,	'function exp_FindAll(Arr:TIntArray; Seq:TIntArray): TIntArray;');
  AddCommand(@exp_FindAll2,	'function exp_FindAll(Arr:TExtArray; Seq:TExtArray): TIntArray; overload;');
  AddCommand(@exp_FindAll3,	'function exp_FindAll(Arr:TPointArray; Seq:TPointArray): TIntArray; overload;');
  AddCommand(@exp_FindAll4,	'function exp_FindAll(Arr:TByteArray; Seq:TByteArray): TIntArray; overload;');
  AddCommand(@exp_FindAll5,	'function exp_FindAll(Arr:TBoxArray; Seq:TBoxArray): TIntArray; overload;');
  AddCommand(@exp_FindAll6,	'function exp_FindAll(Arr:String; Seq:String): TIntArray; overload;');
  

  // Sorting.pas
  AddCommand(@exp_SortTBA,	'procedure exp_SortTBA(var Arr: TByteArray);');
  AddCommand(@exp_SortTIA,	'procedure exp_SortTIA(var Arr: TIntegerArray);');
  AddCommand(@exp_SortTEA,	'procedure exp_SortTEA(var Arr: TExtendedArray);');
  AddCommand(@exp_SortTPA,	'procedure exp_SortTPA(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAFrom,	'procedure exp_SortTPAFrom(var Arr: TPointArray; const From:TPoint);');
  AddCommand(@exp_SortTPAByRow,	'procedure exp_SortTPAByRow(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAByColumn,	'procedure exp_SortTPAByColumn(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAByX,	'procedure exp_SortTPAByX(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAByY,	'procedure exp_SortTPAByY(var Arr: TPointArray);');
  AddCommand(@exp_SortTSA,	'procedure exp_SortTSA(var Arr: TStringArray; CaseInsesitive:Boolean=False);');
  AddCommand(@exp_SortTSANatural,	'procedure exp_SortTSANatural(var Arr: TStringArray);');
  AddCommand(@exp_SortATPAByLength,	'procedure exp_SortATPAByLength(var Arr:T2DPointArray);');
  AddCommand(@exp_SortATPAByMean,	'procedure exp_SortATPAByMean(var Arr:T2DPointArray);');
  AddCommand(@exp_SortATPAByFirst,	'procedure exp_SortATPAByFirst(var Arr:T2DPointArray);');
  AddCommand(@exp_SortATPAByIndex,	'procedure exp_SortATPAByIndex(var Arr:T2DPointArray; index:Int32);');
  AddCommand(@exp_SortATBAByLength,	'procedure exp_SortATBAByLength(var Arr:T2DByteArray);');
  AddCommand(@exp_SortATBAByMean,	'procedure exp_SortATBAByMean(var Arr:T2DByteArray);');
  AddCommand(@exp_SortATBAByFirst,	'procedure exp_SortATBAByFirst(var Arr:T2DByteArray);');
  AddCommand(@exp_SortATBAByIndex,	'procedure exp_SortATBAByIndex(var Arr:T2DByteArray; index:Int32);');
  AddCommand(@exp_SortATIAByLength,	'procedure exp_SortATIAByLength(var Arr:T2DIntArray);');
  AddCommand(@exp_SortATIAByMean,	'procedure exp_SortATIAByMean(var Arr:T2DIntArray);');
  AddCommand(@exp_SortATIAByFirst,	'procedure exp_SortATIAByFirst(var Arr:T2DIntArray);');
  AddCommand(@exp_SortATIAByIndex,	'procedure exp_SortATIAByIndex(var Arr:T2DIntArray; index:Int32);');
  AddCommand(@exp_SortATEAByLength,	'procedure exp_SortATEAByLength(var Arr:T2DExtArray);');
  AddCommand(@exp_SortATEAByMean,	'procedure exp_SortATEAByMean(var Arr:T2DExtArray);');
  AddCommand(@exp_SortATEAByFirst,	'procedure exp_SortATEAByFirst(var Arr:T2DExtArray);');
  AddCommand(@exp_SortATEAByIndex,	'procedure exp_SortATEAByIndex(var Arr:T2DExtArray; index:Int32);');


  // Finder.pas
  AddCommand(@exp_MatchColorRGB,	'function exp_MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
  AddCommand(@exp_MatchColorXYZ,	'function exp_MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
  AddCommand(@exp_MatchColorLAB,	'function exp_MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');
  AddCommand(@exp_MatchColorLCH,	'function exp_MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray;');

  AddCommand(@exp_ImFindColorTolEx,	'function exp_ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorsTolEx,	'function exp_ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLCH,	'function exp_ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLAB,	'function exp_ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');


  // PointTools.pas and related
  AddCommand(@exp_ScalePoint,	'function exp_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;');
  AddCommand(@exp_SumTPA,	    'function exp_SumTPA(const TPA: TPointArray): TPoint;');
  AddCommand(@exp_TPASplitAxis,	'procedure exp_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntArray);');
  AddCommand(@exp_TPAJoinAxis,	'procedure exp_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);');
  AddCommand(@exp_TPAFilter,	'procedure exp_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);');
  AddCommand(@exp_TPAFilterBounds,	'procedure exp_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);');
  AddCommand(@exp_ATPAFilter,	'procedure exp_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);');
  AddCommand(@exp_TPAExtremes,	'function exp_TPAExtremes(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_TPABBox,	    'function exp_TPABBox(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_TPACenter,	'function exp_TPACenter(const TPA: TPointArray; Method: TCenterAlgo; Inside:Boolean): TPoint;');
  AddCommand(@exp_GetAdjacent,	'procedure exp_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);');
  AddCommand(@exp_TPACircularity,	'function exp_TPACircularity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_TPAConvexity,		'function exp_TPAConvexity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_ReverseTPA,	'procedure exp_ReverseTPA(var TPA: TPointArray);');
  AddCommand(@exp_TPARemoveDupes,	'procedure exp_TPARemoveDupes(var TPA: TPointArray);');
  AddCommand(@exp_LongestPolyVector,	'procedure exp_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);');
  AddCommand(@exp_InvertTPA,	  'function exp_InvertTPA(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_RotateTPA,	'function exp_RotateTPA(const TPA: TPointArray; Radians: Extended): TPointArray;');
  AddCommand(@exp_TPAPartition,	'function exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray;');
  AddCommand(@exp_AlignTPA,	    'function exp_AlignTPA(const TPA:TPointArray; Method: TAlignAlgo; var Angle:Extended): TPointArray;');
  AddCommand(@exp_CleanSortTPA,	'function exp_CleanSortTPA(const TPA: TPointArray): TPointArray;');
  AddCommand(@exp_UniteTPA,	    'function exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray;');
  AddCommand(@exp_TPALine,	    'procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);');
  AddCommand(@exp_ConnectTPA,	'function exp_ConnectTPA(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_ConnectTPAEx,	'function exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray;');
  AddCommand(@exp_XagonPoints,	'function exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
  AddCommand(@exp_TPAEllipseBase,	'function exp_TPAEllipseBase(const Center: TPoint; RadiusX, RadiusY:Integer): TPointArray;');
  AddCommand(@exp_TPAEllipse,	'function exp_TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean): TPointArray;');
  AddCommand(@exp_TPACircle,	'function exp_TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean): TPointArray;');
  AddCommand(@exp_TPASimplePoly,'function exp_TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray;');
  AddCommand(@exp_ConvexHull,	'function exp_ConvexHull(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_FloodFillTPAEx,'function exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean): TPointArray;');
  AddCommand(@exp_FloodFillTPA,	'function exp_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray;');
  AddCommand(@exp_TPAOutline,	'function exp_TPAOutline(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_TPABorder,	'function exp_TPABorder(const TPA:TPointArray): TPointArray;');
  AddCommand(@exp_FloodFillPolygon,	'function exp_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray;');
  AddCommand(@exp_ClusterTPAEx,	'function exp_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray;');
  AddCommand(@exp_ClusterTPA,	'function exp_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray;');
  AddCommand(@exp_TPAEdges,	'function exp_TPAEdges(const TPA: TPointArray): TPointArray;');

  AddCommand(@exp_TPASkeleton,	'function exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray;');
  AddCommand(@exp_TPAReduce,	'function exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray;');
  AddCommand(@exp_TPAExpand,	'function exp_TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray;');

  AddCommand(@exp_Spline,	'function exp_Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean): TPointArray;');


  // MatrixTools.pas
  AddCommand(@exp_NewMatrixEx,	'function exp_NewMatrixEx(W,H, Init:Integer): T2DIntArray;');
  AddCommand(@exp_NewMatrix,	'function exp_NewMatrix(W,H:Integer): T2DIntArray;');
  AddCommand(@exp_MatInsertTPA,	'procedure exp_MatInsertTPA(var Matrix:T2DIntArray; const TPA:TPointArray; Value:Integer);');
  AddCommand(@exp_TPAToMatrixEx,'function exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray;');
  AddCommand(@exp_TPAToMatrix,	'function exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray;');
  AddCommand(@exp_NormalizeMat,	'function exp_NormalizeMat(const Mat:T2DIntArray; Alpha, Beta:Integer): T2DIntArray;');
  AddCommand(@exp_MatGetValues,	'function exp_MatGetValues(const Mat:T2DIntArray; const Indices:TPointArray): TIntArray;');
  AddCommand(@exp_MatGetCol,	'function exp_MatGetCol(const Mat:T2DIntArray; Column:Integer): TIntArray;');
  AddCommand(@exp_MatGetRow,	'function exp_MatGetRow(const Mat:T2DIntArray; Row:Integer): TIntArray;');
  AddCommand(@exp_MatGetCols,	'function exp_MatGetCols(const Mat:T2DIntArray; FromCol, ToCol:Integer): T2DIntArray;');
  AddCommand(@exp_MatGetRows,	'function exp_MatGetRows(const Mat:T2DIntArray; FromRow, ToRow:Integer): T2DIntArray;');
  AddCommand(@exp_MatGetArea,	'function exp_MatGetArea(const Mat:T2DIntArray; X1,Y1,X2,Y2:Integer): T2DIntArray;');
  AddCommand(@exp_MatFromTIA,	'function exp_MatFromTIA(const Arr:TIntArray; Width,Height:Integer): T2DIntArray;');
  AddCommand(@exp_PadMatrix,	'procedure exp_PadMatrix(var Matrix:T2DIntArray; HPad,WPad:Integer);');
  AddCommand(@exp_FloodFillMatrix,	'function exp_FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean): TPointArray;');


  // Imaging.pas
  AddCommand(@exp_GaussKernel,	'function exp_GaussKernel(KernelRadius:Integer; Sigma:Single): T2DFloatArray;');
  AddCommand(@exp_ImBlurFilter,	'function exp_ImBlurFilter(ImgArr: T2DIntegerArray; Block:Integer): T2DIntArray;');
  AddCommand(@exp_ImMedianFilter,	'function exp_ImMedianFilter(ImgArr: T2DIntegerArray; Block:Integer): T2DIntArray;');
  AddCommand(@exp_ImBrighten,	'function exp_ImBrighten(ImgArr:T2DIntegerArray; Amount:Extended; Legacy:Boolean): T2DIntArray;');
  AddCommand(@exp_ImEnhance,	'function exp_ImEnhance(ImgArr:T2DIntegerArray; Enhancement:Byte; C:Extended): T2DIntArray;');
  AddCommand(@exp_ImThreshold,	'function exp_ImThreshold(const ImgArr:T2DIntegerArray; Threshold, Alpha, Beta:Byte; Invert:Boolean): T2DIntArray;');
  AddCommand(@exp_ImThresholdAdaptive,	'function exp_ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer): T2DIntArray;');
  AddCommand(@exp_ImFindContours,       'function exp_ImFindContours(const ImgArr:T2DIntegerArray; Outlines:Boolean): T2DPointArray;');
  AddCommand(@exp_ImCEdges,	    'function exp_ImCEdges(const ImgArr: T2DIntegerArray; MinDiff: Integer): TPointArray;');
  AddCommand(@exp_ImSobel,          'function exp_ImSobel(const ImgArr: T2DIntArray): T2DIntArray;');
  AddCommand(@exp_ImConvolve,       'function exp_ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray;');
  AddCommand(@exp_ImGaussBlur,      'function exp_ImGaussBlur(const ImgArr: T2DIntArray; Radius: Integer; Sigma: Single): T2DIntArray;');
  AddCommand(@exp_ImBlend,          'function exp_ImBlend(ImgArr1,ImgArr2: T2DIntArray; Alpha: Single): T2DIntArray;');
  AddCommand(@exp_ImResize,         'procedure exp_ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo);');


  // SimpleOCR.pas
  AddCommand(@exp_ImGetText,     'function exp_ImGetText(ImgArr:T2DIntArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString;');


  // Randomize.pas
  AddCommand(@exp_RandomTPA,	'function exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;');
  AddCommand(@exp_RandomCenterTPA,	'function exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray;');
  AddCommand(@exp_RandomTIA,	'function exp_RandomTIA(Amount:Integer; Low,Hi:Integer): TIntArray;');


  // StringTools.pas
  AddCommand(@exp_StrStrip,	    'function exp_StrStrip(const Text, Chars: String): String;');
  AddCommand(@exp_StrStripL,	    'function exp_StrStripL(const Text, Chars: String): String;');
  AddCommand(@exp_StrStripR,	    'function exp_StrStripR(const Text, Chars: String): String;');
  AddCommand(@exp_StrPosEx,	    'function exp_StrPosEx(const SubStr, Text:String): TIntegerArray;');
  AddCommand(@exp_StrPosL,          'function exp_StrPosL(const SubStr, Text: String): Integer;');
  AddCommand(@exp_StrPosR,          'function exp_StrPosR(const SubStr, Text: String): Integer;');
  AddCommand(@exp_StrReplace,       'function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;');
  AddCommand(@exp_StrExplode,       'function exp_StrExplode(const Text, Sep: String): TStringArray;');


  // CornerDet.pas
  AddCommand(@exp_CornerResponse,        'function exp_CornerResponse(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer): T2DFloatArray;');
  AddCommand(@exp_FindCornerPoints,      'function exp_FindCornerPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;');
  AddCommand(@exp_FindCornerMidPoints,   'function exp_FindCornerMidPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;');


  // MatrixOps.pas
  AddCommand(@exp_IndicesB, 'function exp_Indices(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator): TPointArray;');
  AddCommand(@exp_IndicesI, 'function exp_Indices(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesE, 'function exp_Indices(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesD, 'function exp_Indices(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesF, 'function exp_Indices(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesExB, 'function exp_Indices(const Mat:T2DByteArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesExI, 'function exp_Indices(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesExE, 'function exp_Indices(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesExD, 'function exp_Indices(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator): TPointArray; overload;');
  AddCommand(@exp_IndicesExF, 'function exp_Indices(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator): TPointArray; overload;');

  AddCommand(@exp_MinMaxB, 'procedure exp_MinMax(Mat:T2DByteArray; var Min, Max:Byte);');
  AddCommand(@exp_MinMaxI, 'procedure exp_MinMax(Mat:T2DIntArray; var Min, Max:Integer); overload;');
  AddCommand(@exp_MinMaxE, 'procedure exp_MinMax(Mat:T2DExtArray; var Min, Max:Extended); overload;');
  AddCommand(@exp_MinMaxD, 'procedure exp_MinMax(Mat:T2DDoubleArray; var Min, Max:Double); overload;');
  AddCommand(@exp_MinMaxF, 'procedure exp_MinMax(Mat:T2DFloatArray; var Min, Max:Single); overload;');

  AddCommand(@exp_ArgMaxB, 'function exp_ArgMax(Mat:T2DByteArray): TPoint;');
  AddCommand(@exp_ArgMaxI, 'function exp_ArgMax(Mat:T2DIntArray): TPoint; overload;');
  AddCommand(@exp_ArgMaxE, 'function exp_ArgMax(Mat:T2DExtArray): TPoint; overload;');
  AddCommand(@exp_ArgMaxD, 'function exp_ArgMax(Mat:T2DDoubleArray): TPoint; overload;');
  AddCommand(@exp_ArgMaxF, 'function exp_ArgMax(Mat:T2DFloatArray): TPoint; overload;');
  AddCommand(@exp_ArgMinB, 'function exp_ArgMin(Mat:T2DByteArray): TPoint; overload;');
  AddCommand(@exp_ArgMinI, 'function exp_ArgMin(Mat:T2DIntArray): TPoint; overload;');
  AddCommand(@exp_ArgMinE, 'function exp_ArgMin(Mat:T2DExtArray): TPoint; overload;');
  AddCommand(@exp_ArgMinD, 'function exp_ArgMin(Mat:T2DDoubleArray): TPoint; overload;');
  AddCommand(@exp_ArgMinF, 'function exp_ArgMin(Mat:T2DFloatArray): TPoint; overload;');

  AddCommand(@exp_ArgMaxExB, 'function exp_ArgMax(Mat:T2DByteArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMaxExI, 'function exp_ArgMax(Mat:T2DIntArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMaxExE, 'function exp_ArgMax(Mat:T2DExtArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMaxExD, 'function exp_ArgMax(Mat:T2DDoubleArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMaxExF, 'function exp_ArgMax(Mat:T2DFloatArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMinExB, 'function exp_ArgMin(Mat:T2DByteArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMinExI, 'function exp_ArgMin(Mat:T2DIntArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMinExE, 'function exp_ArgMin(Mat:T2DExtArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMinExD, 'function exp_ArgMin(Mat:T2DDoubleArray; B:TBox): TPoint; overload;');
  AddCommand(@exp_ArgMinExF, 'function exp_ArgMin(Mat:T2DFloatArray; B:TBox): TPoint; overload;');

  AddCommand(@exp_VarMultiB, 'function exp_VarMulti(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean):TByteArray;');
  AddCommand(@exp_VarMultiI, 'function exp_VarMulti(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TIntArray; overload;');
  AddCommand(@exp_VarMultiE, 'function exp_VarMulti(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TExtArray; overload;');
  AddCommand(@exp_VarMultiD, 'function exp_VarMulti(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TDoubleArray; overload;');
  AddCommand(@exp_VarMultiF, 'function exp_VarMulti(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TFloatArray; overload;');

  AddCommand(@exp_ArgMultiB, 'function exp_ArgMulti(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean): TPointArray;');
  AddCommand(@exp_ArgMultiI, 'function exp_ArgMulti(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
  AddCommand(@exp_ArgMultiE, 'function exp_ArgMulti(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
  AddCommand(@exp_ArgMultiD, 'function exp_ArgMulti(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
  AddCommand(@exp_ArgMultiF, 'function exp_ArgMulti(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TPointArray; overload;');
end;




procedure UnsetupCommands;
begin
  SetLength(commands, 0);
  CommandsLoaded := False;
end;


function GetPluginABIVersion: Integer; cdecl; export;
begin
  Result := 2;
end;

procedure SetPluginMemManager(MemMgr : TMemoryManager); cdecl; export;
begin
  if memisset then
    exit;
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(MemMgr);
  memisset := true;
end;



procedure OnDetach; cdecl; export;
begin
  SetMemoryManager(OldMemoryManager);
end;



function GetFunctionCount: Integer; cdecl; export;
begin
  if not commandsLoaded then
    SetupCommands;
  Result := Length(commands);
end;


function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; cdecl; export;
begin
  if ((x > -1) and InRange(x, Low(commands), High(commands))) then
  begin
    ProcAddr := commands[x].procAddr;
    StrPCopy(ProcDef, commands[x].procDef);
    if (x = High(commands)) then UnsetupCommands;
    Exit(x);
  end;
  Exit(-1);
end;



function GetTypeCount: Integer; cdecl; export;
begin
  Result := 8;
end;

function GetTypeInfo(x: Integer; var sType, sTypeDef: PChar): integer; cdecl; export;
begin
  case x of
    0:begin
        StrPCopy(sType, 'TAlignAlgo');
        StrPCopy(sTypeDef, '(AA_BOUNDS, AA_CHULL, AA_BBOX);');
      end;
    1:begin
        StrPCopy(sType, 'TThreshAlgo');
        StrPCopy(sTypeDef, '(TA_Mean, TA_MinMax);');
      end;
    2:begin
        StrPCopy(sType, 'TCenterAlgo');
        StrPCopy(sTypeDef, '(CA_BOUNDS, CA_BBOX, CA_MEAN, CA_MEDIAN);');
      end;
    3:begin
        StrPCopy(sType, 'TResizeAlgo');
        StrPCopy(sTypeDef, '(RA_NEAREST, RA_BILINEAR, RA_BICUBIC);');
      end;
    4:begin
       StrPCopy(sType, 'TCCorrMode');
       StrPCopy(sTypeDef, '(CC_EUCLID, CC_EUCLID_NORMED, CC_EUCLID_SQUARED, CC_CHEB, CC_CHEB_NORMED);');
      end;
    5:begin
        StrPCopy(sType, 'TComparator');
        StrPCopy(sTypeDef, '(__LT__, __GT__, __EQ__, __NE__, __GE__, __LE__);');
      end;
    6:begin
        StrPCopy(sType, 'TChars');
        StrPCopy(sTypeDef, 'Array of T2DIntegerArray;');
      end;
    7:begin
        StrPCopy(sType, 'TCharsArray');
        StrPCopy(sTypeDef, 'Array of TChars;');
      end;
  else
    x := -1;
  end;
  Result := x;
end;



exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetTypeCount;
exports GetTypeInfo;
exports GetFunctionCount;
exports GetFunctionInfo;
exports OnDetach;

end.
