{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap all other methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}


{*-----------------------------------------------------------------------------|
| SimpleOCR.pas                                                                |
|-----------------------------------------------------------------------------*}
function exp_ImGetText(var SE:Pointer; ImgArr:T2DIntArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString; Cdecl;
begin
  Result := ImGetText(ImgArr, Font, MinCharSpace, MinSpace, TextPixTol, Range);
end;


{*-----------------------------------------------------------------------------|
| MatrixTools.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_TPAToMatrixEx(var SE:Pointer; const TPA:TPointArray; Init, Value:Integer; Align:LongBool): T2DIntArray; cdecl;
begin
  Result := TPAToMatrixEx(TPA,Init,Value,Align);
end;

function exp_TPAToMatrix(var SE:Pointer; const TPA:TPointArray; Value:Integer; Align:LongBool): T2DIntArray; cdecl;
begin
  Result := TPAToMatrix(TPA, Value, Align);
end;

function exp_MatFromTIA(var SE:Pointer; const Arr:TIntArray; Width,Height:Integer): T2DIntArray; cdecl;
begin
  Result := MatFromTIA(Arr, Width, Height);
end;

function exp_FloodFillMatrix(var SE:Pointer; ImgArr:T2DIntArray; const Start:TPoint; EightWay:LongBool): TPointArray; cdecl;
begin
  Result := FloodFillMatrix(ImgArr, Start, EightWay);
end;



{*-----------------------------------------------------------------------------|
| Randomize.pas                                                                 |
|-----------------------------------------------------------------------------*}
function exp_RandomTPA(var SE:Pointer; Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray; cdecl;
begin
  Result := RandomTPA(Amount,MinX,MinY,MaxX,MaxY);
end;

function exp_RandomCenterTPA(var SE:Pointer; Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; cdecl;
begin
  Result := RandomCenterTPA(Amount,CX,CY,RadX,RadY);
end;

function exp_RandomTIA(var SE:Pointer; Amount:Integer; Low,Hi:Integer): TIntArray; cdecl;
begin
  Result := RandomTIA(Amount,Low,Hi);
end;


{*-----------------------------------------------------------------------------|
| CornerDet.pas                                                                  |
|-----------------------------------------------------------------------------*}
function exp_CornerResponse(var SE:Pointer; const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray; cdecl;
begin
  Result := CornerResponse(Mat, GaussDev, KSize);
end;

function exp_FindCornerPoints(var SE:Pointer; const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray; cdecl;
begin
  Result := FindCornerPoints(Mat, GaussDev, KSize, Thresh, Footprint);
end;

function exp_FindCornerMidPoints(var SE:Pointer; const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray; cdecl;
begin
  Result := FindCornerMidPoints(Mat, GaussDev, KSize, Thresh, MinDist);
end;





{*-----------------------------------------------------------------------------|
| Just some other stuff                                                        |
|-----------------------------------------------------------------------------*}
(*
function exp_GetMemManager(var SE:Pointer): Pointer; cdecl;
begin
  Result := @NewMemoryManager;
end;

function exp_LoadLibrary(var SE:Pointer; Lib:AnsiString): NativeInt; cdecl;
begin
  Result := LoadLibrary(Lib);
end;

function exp_GetProcAddress(var SE:Pointer; Handle:NativeInt; Proc:AnsiString): Pointer; cdecl;
begin
  Result := GetProcAddress(Handle,Proc);
end;

procedure exp_FreeLibrary(var SE:Pointer; Handle:NativeInt); cdecl;
begin
  FreeLibrary(Handle);
end;
*)
