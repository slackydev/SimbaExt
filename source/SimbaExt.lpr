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

  XT_Types,
  XT_Standard,
  XT_TPointList,
  XT_Sorting,
  XT_Math,
  XT_Matrix,
  XT_ColorMath,
  XT_HashTable,
  XT_Numeric,
  XT_Imaging,
  XT_Randomize,
  XT_Points,
  XT_Finder,
  XT_SimpleOCR,
  XT_CSpline,
  XT_Morphology,
  XT_DensityMap,
  XT_TPAExtShape,
  XT_Strings;

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
var
  l: Integer;
begin
  l := Length(commands);
  SetLength(commands, (l + 1));
  commands[l].procAddr := procAddr;
  commands[l].procDef := procDef;
end;


{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=  THIS GOES OUT OF OUR PLUGIN  =-=-=-=-=-=-=-=-=-=-=-=]
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
procedure SetupCommands;
begin
  // Math.pas
  AddCommand(@exp_DistManhattan,	'function exp_DistManhattan(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistEuclidean,	'function exp_DistEuclidean(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistChebyshev,	'function exp_DistChebyshev(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_DistOctagonal,	'function exp_DistOctagonal(pt1,pt2: TPoint): Extended;');
  AddCommand(@exp_Modulo,	'function exp_Modulo(X,Y:Extended): Extended;');
  AddCommand(@exp_InCircle,	'function exp_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean;');
  AddCommand(@exp_InEllipse,	'function exp_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean;');
  AddCommand(@exp_InRect,	'function exp_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean;');
  AddCommand(@exp_InPoly,	'function exp_InPoly(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyR,	'function exp_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_InPolyW,	'function exp_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean;');
  AddCommand(@exp_DeltaAngle,	'function exp_DeltaAngle(DegA,DegB:Extended): Extended;');
  
  
  // Numeric.pas
  AddCommand(@exp_SumTIA,	'function exp_SumTIA(const Arr: TIntegerArray): Integer;');
  AddCommand(@exp_SumTEA,	'function exp_SumTEA(const Arr: TExtendedArray): Extended;');
  AddCommand(@exp_TIACombinations,	'procedure exp_TIACombinations(const Arr: TIntegerArray; Seq:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_TEACombinations,	'procedure exp_TEACombinations(const Arr: TExtendedArray; Seq:Integer; var Result: T2DExtendedArray);');
  AddCommand(@exp_MinMaxTIA,	'procedure exp_MinMaxTIA(const Arr: TIntegerArray; var Min:Integer; var Max: Integer);');
  AddCommand(@exp_MinMaxTEA,	'procedure exp_MinMaxTEA(const Arr: TExtendedArray; var Min:Extended; var Max: Extended);');
  
  
  // Sorting.pas
  AddCommand(@exp_SortTIA,	'procedure exp_SortTIA(var Arr: TIntegerArray);');
  AddCommand(@exp_SortTEA,	'procedure exp_SortTEA(var Arr: TExtendedArray);');
  AddCommand(@exp_SortTPA,	'procedure exp_SortTPA(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAFrom,	'procedure exp_SortTPAFrom(var Arr: TPointArray; const From:TPoint);');
  AddCommand(@exp_SortTPAByRow,	'procedure exp_SortTPAByRow(var Arr: TPointArray);');
  AddCommand(@exp_SortTPAByColumn,	'procedure exp_SortTPAByColumn(var Arr: TPointArray);');
  
  
  // Finder.pas
  AddCommand(@exp_ImFindColorTolEx,	'function exp_ImFindColorTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorsTolEx,	'function exp_ImFindColorsTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLCH,	'function exp_ImFindColorTolExLCH(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');
  AddCommand(@exp_ImFindColorTolExLAB,	'function exp_ImFindColorTolExLAB(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;');

  
  // Points.pas and replated
  AddCommand(@exp_ScalePoint,	'function exp_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint;');
  AddCommand(@exp_SumTPA,	'function exp_SumTPA(const TPA: TPointArray): TPoint;');
  AddCommand(@exp_TPASplitAxis,	'procedure exp_TPASplitAxis(const TPA: TPointArray; var X:TIntegerArray; var Y:TIntegerArray);');
  AddCommand(@exp_TPAJoinAxis,	'procedure exp_TPAJoinAxis(const X:TIntegerArray; const Y:TIntegerArray; var TPA:TPointArray);');
  AddCommand(@exp_TPAFilter,	'procedure exp_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint);');
  AddCommand(@exp_TPAFilterBounds,	'procedure exp_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer);');
  AddCommand(@exp_ATPAFilter,	'procedure exp_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean);');
  AddCommand(@exp_TPAExtremes,	'procedure exp_TPAExtremes(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_TPABBox,	'procedure exp_TPABBox(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_TPACenter,	'function exp_TPACenter(const TPA: TPointArray; Method: TxCenterMethod; Inside:Boolean): TPoint;');
  AddCommand(@exp_GetAdjacent,	'procedure exp_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean);');
  AddCommand(@exp_TPACircularity,	'function exp_TPACircularity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_TPAConvexity,		'function exp_TPAConvexity(const TPA: TPointArray): Extended;');
  AddCommand(@exp_ReverseTPA,	'procedure exp_ReverseTPA(var TPA: TPointArray);');
  AddCommand(@exp_TPARemoveDupes,	'procedure exp_TPARemoveDupes(var TPA: TPointArray);');
  AddCommand(@exp_LongestPolyVector,	'procedure exp_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint);');
  AddCommand(@exp_InvertTPA,	'procedure exp_InvertTPA(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_RotateTPAEx,	'procedure exp_RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended; var Result:TPointArray);');
  AddCommand(@exp_TPAPartition,	'procedure exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer; var Result:T2DPointArray);');
  AddCommand(@exp_AlignTPA,	'procedure exp_AlignTPA(const TPA:TPointArray; Method: TxAlignMethod; var Angle:Extended; var Result:TPointArray);');
  AddCommand(@exp_CleanSortTPA,	'procedure exp_CleanSortTPA(const TPA: TPointArray; var Result:TPointArray);');
  AddCommand(@exp_UniteTPA,	'procedure exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean; var Result: TPointArray);');
  AddCommand(@exp_TPALine,	'procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint);');
  AddCommand(@exp_ConnectTPA,	'procedure exp_ConnectTPA(const TPA:TPointArray; var Result:TPointArray);');
  AddCommand(@exp_ConnectTPAEx,	'procedure exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended; var Result: TPointArray);');
  AddCommand(@exp_XagonPoints,	'procedure exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint; var Result:TPointArray);');
  AddCommand(@exp_TPAEllipse,	'procedure exp_TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer);');
  AddCommand(@exp_TPACircle,	'procedure exp_TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer);');
  AddCommand(@exp_TPASimplePoly,	'procedure exp_TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint);');
  AddCommand(@exp_ConvexHull,	'procedure exp_ConvexHull(const TPA:TPointArray; var Result: TPointArray);');
  AddCommand(@exp_FloodFillTPAEx,	'procedure exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean; var Result: TPointArray);');
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
  
  
  //Matrix.pas
  AddCommand(@exp_NewMatrixEx,	'procedure exp_NewMatrixEx(W,H, Init:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_NewMatrix,	'procedure exp_NewMatrix(W,H:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatrixSetTPA,	'procedure exp_MatrixSetTPA(var Matrix:T2DIntegerArray; const TPA:TPointArray; Value:Integer; const Offset:TPoint);');
  AddCommand(@exp_TPAToMatrixEx,	'procedure exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean; var Result:T2DIntegerArray);');
  AddCommand(@exp_TPAToMatrix,	'procedure exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean; var Result:T2DIntegerArray);');
  AddCommand(@exp_NormalizeMat,	'procedure exp_NormalizeMat(const Mat:T2DIntegerArray; Alpha, Beta:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetValues,	'procedure exp_MatGetValues(const Mat:T2DIntegerArray; const Indices:TPointArray; var Result:TIntegerArray);');
  AddCommand(@exp_MatCombine,	'procedure exp_MatCombine(var Mat:T2DIntegerArray; const Mat2:T2DIntegerArray; Value:Integer);');
  AddCommand(@exp_MatGetCol,	'procedure exp_MatGetCol(const Mat:T2DIntegerArray; Column:Integer; var Result:TIntegerArray);');
  AddCommand(@exp_MatGetRow,	'procedure exp_MatGetRow(const Mat:T2DIntegerArray; Row:Integer; var Result:TIntegerArray);');
  AddCommand(@exp_MatGetCols,	'procedure exp_MatGetCols(const Mat:T2DIntegerArray; FromCol, ToCol:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetRows,	'procedure exp_MatGetRows(const Mat:T2DIntegerArray; FromRow, ToRow:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatGetArea,	'procedure exp_MatGetArea(const Mat:T2DIntegerArray; X1,Y1,X2,Y2:Integer; var Result:T2DIntegerArray);');
  AddCommand(@exp_MatFromTIA,	'procedure exp_MatFromTIA(const Arr:TIntegerArray; Width,Height:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_PadMatrix,	'procedure exp_PadMatrix(var Matrix:T2DIntegerArray; HPad,WPad:Integer);');
  AddCommand(@exp_FloodFillMatrixEx,	'procedure exp_FloodFillMatrixEx(ImgArr:T2DIntegerArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray);');
  
  
  //Imaging.pas
  AddCommand(@exp_ImBlurFilter,	'procedure exp_ImBlurFilter(ImgArr: T2DIntegerArray; Block:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImMedianFilter,	'procedure exp_ImMedianFilter(ImgArr: T2DIntegerArray; Block:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImBrighten,	'procedure exp_ImBrighten(ImgArr:T2DIntegerArray; Amount:Extended; Legacy:Boolean; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImEnhance,	'procedure exp_ImEnhance(ImgArr:T2DIntegerArray; Enhancement:Byte; C:Extended; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImThreshold,	'procedure exp_ImThreshold(const ImgArr:T2DIntegerArray; Threshold, Alpha, Beta:Byte; Invert:Boolean; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImThresholdAdaptive,	'procedure exp_ImThresholdAdaptive(const ImgArr:T2DIntegerArray; Alpha, Beta: Byte; Invert:Boolean; Method:TxThreshMethod; C:Integer; var Result: T2DIntegerArray);');
  AddCommand(@exp_ImFindContours,	'procedure exp_ImFindContours(const ImgArr:T2DIntegerArray; Outlines:Boolean; var Result: T2DPointArray);');
  AddCommand(@exp_ImCEdges,	'procedure exp_ImCEdges(const ImgArr: T2DIntegerArray; MinDiff: Integer; var Result: TPointArray);');
  AddCommand(@exp_ImResize,	'procedure exp_ImResize(var ImgArr:T2DIntegerArray; NewW, NewH: Integer; Method:TxResizeMethod);');
  
  
  //Randomize.pas
  AddCommand(@exp_RandomTPA,	'procedure exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer; var Result:TPointArray);');
  AddCommand(@exp_RandomCenterTPA,	'procedure exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer; var Result:TPointArray);');
  AddCommand(@exp_RandomTIA,	'procedure exp_RandomTIA(Amount:Integer; Low,Hi:Integer; var Result: TIntegerArray);');


  //Strings.pas
  AddCommand(@exp_StrPosEx,	'procedure exp_StrPosEx(const Text, SubStr:String; var Result:TIntegerArray);');
  AddCommand(@exp_StrReplace,	'function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String;');
  AddCommand(@exp_StrExplode,   'procedure exp_StrExplode(const Text, Sep: String; var Result: TStringArray);');
  
  
  CommandsLoaded := True;
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
  Result := 6;
end;

function GetTypeInfo(x: Integer; var sType, sTypeDef: PChar): integer; cdecl; export;
begin
  case x of
    0:begin
        StrPCopy(sType, 'TxAlignMethod');
        StrPCopy(sTypeDef, '(amExtremes, amConvex, amBBox);');
      end;
    1:begin
        StrPCopy(sType, 'TxThreshMethod');
        StrPCopy(sTypeDef, '(tmMean, tmMinMax);');
      end;
    2:begin
        StrPCopy(sType, 'TxCenterMethod');
        StrPCopy(sTypeDef, '(cmBounds, cmBBox, cmMean, cmMedian);');
      end;
    3:begin
        StrPCopy(sType, 'TxResizeMethod');
        StrPCopy(sTypeDef, '(rmNearest, rmBilinear, rmBicubic);');
      end;
    4:begin
        StrPCopy(sType, 'TChars');
        StrPCopy(sTypeDef,'Array of T2DIntegerArray;');
      end;
    5:begin
        StrPCopy(sType, 'TCharsArray');
        StrPCopy(sTypeDef,'Array of TChars;');
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
