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
AddMethod(@TestResizeBI,	'function __TEST_ResizeBI(Src:TIntMatrix; NewW,NewH:int32): TIntMatrix;');

AddMethod(@exp_TPAToMatrix,	'function SimbaExt.TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:LongBool=True): TIntMatrix;');
AddMethod(@exp_TPAToMatrixEx,	'function SimbaExt.TPAToMatrix(const TPA:TPointArray; Init, Value:Integer; Align:LongBool=True): TIntMatrix; overload;');
AddMethod(@exp_MatFromTIA,	'function SimbaExt.TIAToMatrix(const Arr:TIntArray; Width,Height:Integer): TIntMatrix;');
AddMethod(@exp_FloodFillMatrix,	'function SimbaExt.FloodFillMatrix(ImgArr:TIntMatrix; const Start:TPoint; EightWay:LongBool): TPointArray;');

AddMethod(@exp_ImGetText,	'function SimbaExt.ImGetText(ImgArr:TIntMatrix; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Int32; Range:AnsiString): AnsiString;');

AddMethod(@exp_RandomTPA,	'function SimbaExt.RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray;');
AddMethod(@exp_RandomCenterTPA,	'function SimbaExt.RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray;');
AddMethod(@exp_RandomTIA,	'function SimbaExt.RandomTIA(Amount:Integer; Low,Hi:Integer): TIntArray;');

AddMethod(@exp_CornerResponse,		'function SimbaExt.CornerResponse(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer): T2DFloatArray;');
AddMethod(@exp_FindCornerPoints,	'function SimbaExt.FindCorners(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray;');
AddMethod(@exp_FindCornerMidPoints,	'function SimbaExt.FindCornersMid(const Mat:T2DIntegerArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray;');

AddMethod(@exp_MarkTime,	'function SimbaExt.MarkTime(): Double;');

(*
AddMethod(@exp_GetMemManager,	'function SimbaExt.GetMemManager(): Pointer;');
AddMethod(@exp_LoadLibrary,	'function SimbaExt.LoadLibrary(Lib:AnsiString): NativeInt;');
AddMethod(@exp_GetProcAddress,	'function SimbaExt.GetProcAddress(Handle:NativeInt; Proc:AnsiString): Pointer;');
AddMethod(@exp_FreeLibrary,	'procedure SimbaExt.FreeLibrary(Handle:NativeInt);');
*)


