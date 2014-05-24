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
  CornerDet;
  //_Tests;

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
  AddCommand(@exp_DistToLine,       'function exp_DistToLine(Pt, sA, sB: TPoint): Extended;');
  AddCommand(@exp_Modulo,	'function exp_Modulo(X,Y:Extended): Extended;');
  AddCommand(@exp_IModulo,	'function exp_IModulo(X,Y:Integer): Integer;');
  AddCommand(@exp_InCircle,	'function exp_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;');
  AddCommand(@exp_InEllipse,	'function exp_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;');
  AddCommand(@exp_InRect,	'function exp_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;');
  AddCommand(@exp_InPoly,	'function exp_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyR,	'function exp_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyW,	'function exp_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_DeltaAngle,	'function exp_DeltaAngle(DegA,DegB:Extended): Extended;');



  // Numeric.pas
  AddCommand(@exp_SumTBA,	'function exp_SumTBA(const Arr: TByteArray): Int64;');
  AddCommand(@exp_SumTIA,	'function exp_SumTIA(const Arr: TIntArray): Int64;');
  AddCommand(@exp_SumTEA,	'function exp_SumTEA(const Arr: TExtArray): Extended;');
  AddCommand(@exp_TIACombinations,	'procedure exp_TIACombinations(const Arr: TIntArray; Seq:Integer; var Result: T2DIntArray);');
  AddCommand(@exp_TEACombinations,	'procedure exp_TEACombinations(const Arr: TExtArray; Seq:Integer; var Result: T2DExtArray);');
  AddCommand(@exp_MinMaxTBA,	'procedure exp_MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte);');
  AddCommand(@exp_MinMaxTIA,	'procedure exp_MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer);');
  AddCommand(@exp_MinMaxTEA,	'procedure exp_MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended);');


  // Std.pas
  AddCommand(@exp_Slice1,	'procedure exp_Slice(Arr:TIntArray; Start,Stop,Step:Int32; var Result:TIntArray);');
  AddCommand(@exp_Slice2,	'procedure exp_Slice(Arr:TExtArray; Start,Stop,Step:Int32; var Result:TExtArray); overload;');
  AddCommand(@exp_Slice3,	'procedure exp_Slice(Arr:TPointArray; Start,Stop,Step:Int32; var Result:TPointArray); overload;');
  AddCommand(@exp_Slice4,	'procedure exp_Slice(Arr:TByteArray; Start,Stop,Step:Int32; var Result:TByteArray); overload;');
  AddCommand(@exp_Slice5,	'procedure exp_Slice(Arr:TBoxArray; Start,Stop,Step:Int32; var Result:TBoxArray); overload;');
  AddCommand(@exp_Slice6,	'procedure exp_Slice(Arr:String; Start,Stop,Step:Int32; var Result:String); overload;');
  AddCommand(@exp_Slice7,	'procedure exp_Slice(Arr:T2DIntArray; Start,Stop,Step:Int32; var Result:T2DIntArray); overload;');
  AddCommand(@exp_Slice8,	'procedure exp_Slice(Arr:T2DExtArray; Start,Stop,Step:Int32; var Result:T2DExtArray); overload;');
  AddCommand(@exp_Slice9,	'procedure exp_Slice(Arr:T2DPointArray; Start,Stop,Step:Int32; var Result:T2DPointArray); overload;');
  AddCommand(@exp_Slice10,	'procedure exp_Slice(Arr:T2DByteArray; Start,Stop,Step:Int32; var Result:T2DByteArray); overload;');
  AddCommand(@exp_Slice11,	'procedure exp_Slice(Arr:T2DBoxArray; Start,Stop,Step:Int32; var Result:T2DBoxArray); overload;');
  AddCommand(@exp_Slice12,	'procedure exp_Slice(Arr:TStringArray; Start,Stop,Step:Int32; var Result:TStringArray); overload;');

  AddCommand(@exp_Find1,	'function exp_Find(Arr:TIntArray; Seq:TIntArray): Int32;');
  AddCommand(@exp_Find2,	'function exp_Find(Arr:TExtArray; Seq:TExtArray): Int32; overload;');
  AddCommand(@exp_Find3,	'function exp_Find(Arr:TPointArray; Seq:TPointArray): Int32; overload;');
  AddCommand(@exp_Find4,	'function exp_Find(Arr:TByteArray; Seq:TByteArray): Int32; overload;');
  AddCommand(@exp_Find5,	'function exp_Find(Arr:TBoxArray; Seq:TBoxArray): Int32; overload;');
  AddCommand(@exp_Find6,	'function exp_Find(Arr:String; Seq:String): Int32; overload;');

  AddCommand(@exp_FindAll1,	'procedure exp_FindAll(Arr:TIntArray; Seq:TIntArray; var Result: TIntArray);');
  AddCommand(@exp_FindAll2,	'procedure exp_FindAll(Arr:TExtArray; Seq:TExtArray; var Result: TIntArray); overload;');
  AddCommand(@exp_FindAll3,	'procedure exp_FindAll(Arr:TPointArray; Seq:TPointArray; var Result: TIntArray); overload;');
  AddCommand(@exp_FindAll4,	'procedure exp_FindAll(Arr:TByteArray; Seq:TByteArray; var Result: TIntArray); overload;');
  AddCommand(@exp_FindAll5,	'procedure exp_FindAll(Arr:TBoxArray; Seq:TBoxArray; var Result: TIntArray); overload;');
  AddCommand(@exp_FindAll6,	'procedure exp_FindAll(Arr:String; Seq:String; var Result: TIntArray); overload;');
  

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
  AddCommand(@exp_MatchColorRGB,	'procedure exp_MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray);');
  AddCommand(@exp_MatchColorXYZ,	'procedure exp_MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray);');
  AddCommand(@exp_MatchColorLAB,	'procedure exp_MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray);');
  AddCommand(@exp_MatchColorLCH,	'procedure exp_MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray);');

  AddCommand(@exp_ImFindColorTolEx,	'function exp_ImFindColorTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorsTolEx,	'function exp_ImFindColorsTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLCH,	'function exp_ImFindColorTolExLCH(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLAB,	'function exp_ImFindColorTolExLAB(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');


  // PointTools.pas and related
  AddCommand(@exp_ScalePoint,	'function exp_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;');
  AddCommand(@exp_SumTPA,	    'function exp_SumTPA(const TPA: TPointArray): TPoint;');
  AddCommand(@exp_TPASplitAxis,	'procedure exp_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntegerArray);');
  AddCommand(@exp_TPAJoinAxis,	'procedure exp_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);');
  AddCommand(@exp_TPAFilter,	'procedure exp_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);');
  AddCommand(@exp_TPAFilterBounds,	'procedure exp_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);');
  AddCommand(@exp_ATPAFilter,	'procedure exp_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);');
  AddCommand(@exp_TPAExtremes,	'procedure exp_TPAExtremes(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_TPABBox,	    'procedure exp_TPABBox(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_TPACenter,	'function exp_TPACenter(const TPA: TPointArray; Method: TCenterAlgo; Inside:Boolean): TPoint;');
  AddCommand(@exp_GetAdjacent,	'procedure exp_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);');
  AddCommand(@exp_TPACircularity,	'function exp_TPACircularity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_TPAConvexity,		'function exp_TPAConvexity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_ReverseTPA,	'procedure exp_ReverseTPA(var TPA: TPointArray);');
  AddCommand(@exp_TPARemoveDupes,	'procedure exp_TPARemoveDupes(var TPA: TPointArray);');
  AddCommand(@exp_LongestPolyVector,	'procedure exp_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);');
  AddCommand(@exp_InvertTPA,	  'procedure exp_InvertTPA(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_RotateTPA,	'procedure exp_RotateTPA(const TPA: TPointArray; Radians: Extended; var Result:TPointArray);');
  AddCommand(@exp_TPAPartition,	'procedure exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer; var Result:T2DPointArray);');
  AddCommand(@exp_AlignTPA,	    'procedure exp_AlignTPA(const TPA:TPointArray; Method: TAlignAlgo; var Angle:Extended; var Result:TPointArray);');
  AddCommand(@exp_CleanSortTPA,	'procedure exp_CleanSortTPA(const TPA: TPointArray; var Result:TPointArray);');
  AddCommand(@exp_UniteTPA,	    'procedure exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean; var Result: TPointArray);');
  AddCommand(@exp_TPALine,	    'procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);');
  AddCommand(@exp_ConnectTPA,	'procedure exp_ConnectTPA(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_ConnectTPAEx,	'procedure exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended; var Result: TPointArray);');
  AddCommand(@exp_XagonPoints,	'procedure exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint; var Result:TPointArray);');
  AddCommand(@exp_TPAEllipseBase,	'procedure exp_TPAEllipseBase(const Center: TPoint; RadiusX, RadiusY:Integer; var Result:TPointArray);');
  AddCommand(@exp_TPAEllipse,	'procedure exp_TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean; var Result:TPointArray);');
  AddCommand(@exp_TPACircle,	'procedure exp_TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean;     var Result:TPointArray);');
  AddCommand(@exp_TPASimplePoly,'procedure exp_TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint; var Result:TPointArray);');
  AddCommand(@exp_ConvexHull,	'procedure exp_ConvexHull(const TPA:TPointArray; var Result: TPointArray);');
  AddCommand(@exp_FloodFillTPAEx,'procedure exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean; var Result: TPointArray);');
  AddCommand(@exp_FloodFillTPA,	'procedure exp_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray);');
  AddCommand(@exp_TPAOutline,	'procedure exp_TPAOutline(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_TPABorder,	'procedure exp_TPABorder(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_FloodFillPolygon,	'procedure exp_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean; var Result:TPointArray);');
  AddCommand(@exp_ClusterTPAEx,	'procedure exp_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean; var Result:T2DPointArray);');
  AddCommand(@exp_ClusterTPA,	'procedure exp_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean; var Result:T2DPointArray);');
  AddCommand(@exp_TPAEdges,	'procedure exp_TPAEdges(const TPA: TPointArray; var Result:TPointArray);');

  AddCommand(@exp_TPASkeleton,	'procedure exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer; var Result:TPointArray);');
  AddCommand(@exp_TPAReduce,	'procedure exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer; var Result:TPointArray);');
  AddCommand(@exp_TPAExpand,	'procedure exp_TPAExpand(const TPA:TPointArray; Iterations:Integer; var Result:TPointArray);');

  AddCommand(@exp_Spline,	'procedure exp_Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean; var Result: TPointArray);');


  // MatrixTools.pas
  AddCommand(@exp_NewMatrixEx,	'procedure exp_NewMatrixEx(W,H, Init:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_NewMatrix,	'procedure exp_NewMatrix(W,H:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatInsertTPA,	'procedure exp_MatInsertTPA(var Matrix:T2DIntegerArray; const TPA:TPointArray; Value:Integer);');
  AddCommand(@exp_TPAToMatrixEx,'procedure exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean; var Result:T2DIntegerArray);');
  AddCommand(@exp_TPAToMatrix,	'procedure exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean; var Result:T2DIntegerArray);');
  AddCommand(@exp_NormalizeMat,	'procedure exp_NormalizeMat(const Mat:T2DIntegerArray; Alpha, Beta:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetValues,	'procedure exp_MatGetValues(const Mat:T2DIntegerArray; const Indices:TPointArray; var Result:TIntegerArray);');
  AddCommand(@exp_MatGetCol,	'procedure exp_MatGetCol(const Mat:T2DIntegerArray; Column:Integer; var Result:TIntegerArray);');
  AddCommand(@exp_MatGetRow,	'procedure exp_MatGetRow(const Mat:T2DIntegerArray; Row:Integer; var Result:TIntegerArray);');
  AddCommand(@exp_MatGetCols,	'procedure exp_MatGetCols(const Mat:T2DIntegerArray; FromCol, ToCol:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetRows,	'procedure exp_MatGetRows(const Mat:T2DIntegerArray; FromRow, ToRow:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetArea,	'procedure exp_MatGetArea(const Mat:T2DIntegerArray; X1,Y1,X2,Y2:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatFromTIA,	'procedure exp_MatFromTIA(const Arr:TIntegerArray; Width,Height:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_PadMatrix,	'procedure exp_PadMatrix(var Matrix:T2DIntegerArray; HPad,WPad:Integer);');
  AddCommand(@exp_FloodFillMatrix,	'procedure exp_FloodFillMatrix(ImgArr:T2DIntegerArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray);');


  // Imaging.pas
  AddCommand(@exp_GaussKernel,	'procedure exp_GaussKernel(KernelRadius:Integer; Sigma:Single; var Result:T2DFloatArray);');
  AddCommand(@exp_ImBlurFilter,	'procedure exp_ImBlurFilter(ImgArr: T2DIntegerArray; Block:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImMedianFilter,	'procedure exp_ImMedianFilter(ImgArr: T2DIntegerArray; Block:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImBrighten,	'procedure exp_ImBrighten(ImgArr:T2DIntegerArray; Amount:Extended; Legacy:Boolean; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImEnhance,	'procedure exp_ImEnhance(ImgArr:T2DIntegerArray; Enhancement:Byte; C:Extended; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImThreshold,	'procedure exp_ImThreshold(const ImgArr:T2DIntegerArray; Threshold, Alpha, Beta:Byte; Invert:Boolean; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImThresholdAdaptive,	'procedure exp_ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImFindContours,	'procedure exp_ImFindContours(const ImgArr:T2DIntegerArray; Outlines:Boolean; var Result: T2DPointArray);');
  AddCommand(@exp_ImCEdges,	    'procedure exp_ImCEdges(const ImgArr: T2DIntegerArray; MinDiff: Integer; var Result: TPointArray);');
  AddCommand(@exp_ImSobel,	    'procedure exp_ImSobel(const ImgArr: T2DIntArray; var Result:T2DIntArray);');
  AddCommand(@exp_ImConvolve,	  'procedure exp_ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray; var Result:T2DIntArray);');
  AddCommand(@exp_ImGaussBlur,  'procedure exp_ImGaussBlur(const ImgArr: T2DIntArray; Radius: Integer; Sigma: Single; var Result:T2DIntArray);');
  AddCommand(@exp_ImResize,	    'procedure exp_ImResize(var ImgArr:T2DIntegerArray; NewW, NewH: Integer; Method:TResizeAlgo);');

  // SimpleOCR.pas
  AddCommand(@exp_ImGetText,     'function exp_ImGetText(ImgArr:T2DIntegerArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString;');


  // Randomize.pas
  AddCommand(@exp_RandomTPA,	'procedure exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer; var Result:TPointArray);');
  AddCommand(@exp_RandomCenterTPA,	'procedure exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer; var Result:TPointArray);');
  AddCommand(@exp_RandomTIA,	'procedure exp_RandomTIA(Amount:Integer; Low,Hi:Integer; var Result: TIntegerArray);');


  // StringTools.pas
  AddCommand(@exp_StrStrip,	    'function exp_StrStrip(const Text, Chars: String): String;');
  AddCommand(@exp_StrStripL,	    'function exp_StrStripL(const Text, Chars: String): String;');
  AddCommand(@exp_StrStripR,	    'function exp_StrStripR(const Text, Chars: String): String;');
  AddCommand(@exp_StrPosEx,	    'procedure exp_StrPosEx(const SubStr, Text:String; var Result:TIntegerArray);');
  AddCommand(@exp_StrPosL,          'function exp_StrPosL(const SubStr, Text: String): Integer;');
  AddCommand(@exp_StrPosR,          'function exp_StrPosR(const SubStr, Text: String): Integer;');
  AddCommand(@exp_StrReplace,       'function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;');
  AddCommand(@exp_StrExplode,       'procedure exp_StrExplode(const Text, Sep: String; var Result: TStringArray);');


  // CornerDet.pas
  AddCommand(@exp_CornerResponse,        'procedure exp_CornerResponse(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; var Result: T2DFloatArray);');
  AddCommand(@exp_FindCornerPoints,      'procedure exp_FindCornerPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer; var Result:TPointArray);');
  AddCommand(@exp_FindCornerMidPoints,   'procedure exp_FindCornerMidPoints(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer; var Result:TPointArray);');


  // MatrixOps.pas
  AddCommand(@exp_IndicesB, 'procedure exp_IndicesB(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesI, 'procedure exp_IndicesI(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesE, 'procedure exp_IndicesE(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesD, 'procedure exp_IndicesD(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesF, 'procedure exp_IndicesF(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesExB, 'procedure exp_IndicesExB(const Mat:T2DByteArray; B:TBox; Value: Integer; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesExI, 'procedure exp_IndicesExI(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesExE, 'procedure exp_IndicesExE(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesExD, 'procedure exp_IndicesExD(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator; var Result: TPointArray);');
  AddCommand(@exp_IndicesExF, 'procedure exp_IndicesExF(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator; var Result: TPointArray);');

  AddCommand(@exp_MinMaxB, 'procedure exp_MinMaxB(Mat:T2DByteArray; var Min, Max:Byte);');
  AddCommand(@exp_MinMaxI, 'procedure exp_MinMaxI(Mat:T2DIntArray; var Min, Max:Integer);');
  AddCommand(@exp_MinMaxE, 'procedure exp_MinMaxE(Mat:T2DExtArray; var Min, Max:Extended);');
  AddCommand(@exp_MinMaxD, 'procedure exp_MinMaxD(Mat:T2DDoubleArray; var Min, Max:Double);');
  AddCommand(@exp_MinMaxF, 'procedure exp_MinMaxF(Mat:T2DFloatArray; var Min, Max:Single);');

  AddCommand(@exp_ArgMaxB, 'function exp_ArgMaxB(Mat:T2DByteArray): TPoint;');
  AddCommand(@exp_ArgMaxI, 'function exp_ArgMaxI(Mat:T2DIntArray): TPoint;');
  AddCommand(@exp_ArgMaxE, 'function exp_ArgMaxE(Mat:T2DExtArray): TPoint;');
  AddCommand(@exp_ArgMaxD, 'function exp_ArgMaxD(Mat:T2DDoubleArray): TPoint;');
  AddCommand(@exp_ArgMaxF, 'function exp_ArgMaxF(Mat:T2DFloatArray): TPoint;');
  AddCommand(@exp_ArgMinB, 'function exp_ArgMinB(Mat:T2DByteArray): TPoint;');
  AddCommand(@exp_ArgMinI, 'function exp_ArgMinI(Mat:T2DIntArray): TPoint;');
  AddCommand(@exp_ArgMinE, 'function exp_ArgMinE(Mat:T2DExtArray): TPoint;');
  AddCommand(@exp_ArgMinD, 'function exp_ArgMinD(Mat:T2DDoubleArray): TPoint;');
  AddCommand(@exp_ArgMinF, 'function exp_ArgMinF(Mat:T2DFloatArray): TPoint;');
  AddCommand(@exp_ArgMaxExB, 'function exp_ArgMaxExB(Mat:T2DByteArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMaxExI, 'function exp_ArgMaxExI(Mat:T2DIntArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMaxExE, 'function exp_ArgMaxExE(Mat:T2DExtArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMaxExD, 'function exp_ArgMaxExD(Mat:T2DDoubleArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMaxExF, 'function exp_ArgMaxExF(Mat:T2DFloatArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMinExB, 'function exp_ArgMinExB(Mat:T2DByteArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMinExI, 'function exp_ArgMinExI(Mat:T2DIntArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMinExE, 'function exp_ArgMinExE(Mat:T2DExtArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMinExD, 'function exp_ArgMinExD(Mat:T2DDoubleArray; B:TBox): TPoint;');
  AddCommand(@exp_ArgMinExF, 'function exp_ArgMinExF(Mat:T2DFloatArray; B:TBox): TPoint;');


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
