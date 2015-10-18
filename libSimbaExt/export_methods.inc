{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Holds all exported methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$I lpexports/exp_std.inc}
{$I lpexports/exp_math.inc}
{$I lpexports/exp_finder.inc}
{$I lpexports/exp_sorting.inc}
{$I lpexports/exp_imaging.inc}
{$I lpexports/exp_matrixops.inc}
{$I lpexports/exp_statistics.inc}
{$I lpexports/exp_pointtools.inc}
{$I lpexports/exp_stringtools.inc}


(* other methods *)
AddMethod(@exp_PopException,	'function SimbaExt.GetException(var Msg:String): LongBool;');
AddMethod(@TestResizeBI,	'function TestResizeBI(src:TIntMatrix; newW,newH:int32): TIntMatrix;');

AddMethod(@exp_TPAToMatrix,	'function SimbaExt.TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:LongBool=True): TIntMatrix;');
AddMethod(@exp_TPAToMatrixEx,	'function SimbaExt.TPAToMatrix(const TPA:TPointArray; Init, Value:Integer; Align:LongBool=True): TIntMatrix; overload;');
AddMethod(@exp_MatFromTIA,	'function SimbaExt.TIAToMatrix(const Arr:TIntArray; Width,Height:Integer): TIntMatrix;');
AddMethod(@exp_FloodFillMatrix,	'function SimbaExt.FloodFillMatrix(ImgArr:TIntMatrix; const Start:TPoint; EightWay:LongBool): TPointArray;');

AddMethod(@exp_RandomTPA,	'function SimbaExt.RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;');
AddMethod(@exp_RandomCenterTPA,	'function SimbaExt.RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray;');
AddMethod(@exp_RandomTIA,	'function SimbaExt.RandomTIA(Amount:Integer; Low,Hi:Integer): TIntArray;');

AddMethod(@exp_CornerResponse,      'function SimbaExt.CornerResponse(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer): T2DFloatArray;');
AddMethod(@exp_FindCornerPoints,    'function SimbaExt.FindCorners(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;');
AddMethod(@exp_FindCornerMidPoints,	'function SimbaExt.FindCornersMid(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;');

AddMethod(@exp_MarkTime, 'function SimbaExt.MarkTime(): Double;');

AddMethod(@TSlackTree_Init,        'procedure TSlackTree.Init(TPA:TPointArray);');
AddMethod(@TSlackTree_Free,        'procedure TSlackTree.Free();');
AddMethod(@TSlackTree_Find,        'function TSlackTree.Find(pt:TPoint): PSlackTree;');
AddMethod(@TSlackTree_Delete,      'procedure TSlackTree.Delete();');
AddMethod(@TSlackTree_Delete2,     'function TSlackTree.Delete(pt:TPoint): Boolean; overload;');
AddMethod(@TSlackTree_Nearest_N,   'function TSlackTree.Nearest_N(pt:TPoint; notEqual:Boolean=False): PSlackTree;');
AddMethod(@TSlackTree_Nearest,     'function TSlackTree.Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;');
AddMethod(@TSlackTree_RangeQuery_N,'function TSlackTree.RangeQuery_N(B:TBox): TSlackNodeArr;');
AddMethod(@TSlackTree_RangeQuery,  'function TSlackTree.RangeQuery(B:TBox; remove:ByteBool=False): TPointArray;');
AddMethod(@TSlackTree_RangeQuery2, 'function TSlackTree.RangeQuery(query:TPoint; xRad,yRad:Double; remove:ByteBool=False): TPointArray; overload;');

AddMethod(@ClusterTest, 'function ClusterTest(TPA:TPointArray; dist:Int32): T2DPointArray;');
AddMethod(@ApprCluster, 'function ApprCluster(TPA:TPointArray; xrad,yrad:Int32; fastAppr:Boolean=True): T2DPointArray;');
AddMethod(@GaussianBlur_, 'procedure GaussianBlur_(const image:T2DIntArray; var dest:T2DIntArray; radius:Int32; sigma:Single); cdecl;');


AddMethod(@exp_LoadLibrary,	'function SimbaExt.LoadLibrary(Lib:AnsiString): NativeInt;');
AddMethod(@exp_GetProcAddress,	'function SimbaExt.GetProcAddress(Handle:NativeInt; Proc:AnsiString): Pointer;');
AddMethod(@exp_FreeLibrary,	'procedure SimbaExt.FreeLibrary(Handle:NativeInt);');


