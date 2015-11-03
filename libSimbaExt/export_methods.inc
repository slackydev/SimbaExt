{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Holds all exported methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$I lpexports/exp_arrays.inc}
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
AddMethod(@MethodList,		'function SimbaExt.MethodList(): TStringArray;');
AddMethod(@FreeDeclarations,	'procedure SimbaExt.Free();');
AddMethod(@exp_PopException,	'function SimbaExt.GetException(var Msg:String): LongBool;');
AddMethod(@exp_MarkTime,	'function SimbaExt.MarkTime(): Double;');

AddMethod(@exp_RandomTPA,	'function SimbaExt.RandomTPA(count, MinX,MinY,MaxX,MaxY:Int32): TPointArray; native;');
AddMethod(@exp_RandomCenterTPA,	'function SimbaExt.RandomCenterTPA(count, CX,CY,RadX,RadY:Int32): TPointArray; native;');
AddMethod(@exp_RandomTIA,	'function SimbaExt.RandomTIA(count, low,hi:Int32): TIntArray;');

AddMethod(@exp_CornerResponse,		'function SimbaExt.CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Int32): T2DFloatArray; native;');
AddMethod(@exp_FindCornerPoints,	'function SimbaExt.FindCorners(const Mat:T2DIntArray; GaussDev:Single; KSize:Int32; Thresh:Single; Footprint:Int32): TPointArray; native;');
AddMethod(@exp_FindCornerMidPoints,	'function SimbaExt.FindCornersMid(const Mat:T2DIntArray; GaussDev:Single; KSize:Int32; Thresh:Single; MinDist:Int32): TPointArray; native;');


AddMethod(@TSlackTree_Init,        'procedure TSlackTree.Init(TPA:TPointArray);');
AddMethod(@TSlackTree_Free,        'procedure TSlackTree.Free();');
AddMethod(@TSlackTree_IndexOf,     'function TSlackTree.IndexOf(pt:TPoint): Int32;');
AddMethod(@TSlackTree_Find,        'function TSlackTree.Find(pt:TPoint): PSlackNode;');
AddMethod(@TSlackTree_Delete,      'procedure TSlackTree.Delete(idx:Int32);');
AddMethod(@TSlackTree_Delete2,     'function TSlackTree.Delete(pt:TPoint): LongBool; overload;');
AddMethod(@TSlackTree_Nearest_N,   'function TSlackTree.Nearest_N(pt:TPoint; notEqual:LongBool=False): PSlackNode;');
AddMethod(@TSlackTree_Nearest,     'function TSlackTree.Nearest(pt:TPoint; notEqual:LongBool=False): TPoint;');
AddMethod(@TSlackTree_kNearest_N,  'function TSlackTree.kNearest_N(pt:TPoint; k:Int32; notEqual:LongBool=False): TSlackRefArray;');
AddMethod(@TSlackTree_kNearest,    'function TSlackTree.kNearest(pt:TPoint; k:Int32; notEqual:LongBool=False): TPointArray;');
AddMethod(@TSlackTree_RangeQuery_N,'function TSlackTree.RangeQuery_N(B:TBox): TSlackRefArray;');
AddMethod(@TSlackTree_RangeQuery,  'function TSlackTree.RangeQuery(B:TBox; remove:LongBool=False): TPointArray;');
AddMethod(@TSlackTree_RangeQuery2, 'function TSlackTree.RangeQuery(query:TPoint; xRad,yRad:Double; remove:LongBool=False): TPointArray; overload;');
AddMethod(@TSlackTree_RefArray,    'function TSlackTree.RefArray: TSlackRefArray;');

{$IFDEF experiments}
AddMethod(@ClusterTest,   'function ClusterTest(TPA:TPointArray; dist:Int32): T2DPointArray;');
AddMethod(@ApprCluster,   'function ApprCluster(TPA:TPointArray; xrad,yrad:Int32; fastAppr:Boolean=True): T2DPointArray;');
AddMethod(@GaussianBlur_, 'procedure GaussianBlur_(const image:T2DIntArray; var dest:T2DIntArray; radius:Int32; sigma:Single); cdecl;');
AddMethod(@TestResizeBI,  'function TestResizeBI(src:TIntMatrix; newW,newH:int32): TIntMatrix;');
AddMethod(@TestLookupSpeed,  'procedure TestLookupSpeed(TPA:TPointArray);');
{$ENDIF}

AddMethod(@exp_LoadLibrary,	'function SimbaExt.LoadLibrary(Lib:AnsiString): NativeInt; native;');
AddMethod(@exp_GetProcAddress,	'function SimbaExt.GetProcAddress(Handle:NativeInt; Proc:AnsiString): Pointer; native;');
AddMethod(@exp_FreeLibrary,	'procedure SimbaExt.FreeLibrary(Handle:NativeInt); native;');









