{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap all other methods.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function exp_MarkTime(var SE:SExt): Double; cdecl;
begin
  Result := TimeUtils.MarkTime(); 
end;

function exp_PopException(var SE:SExt; out Msg:String): LongBool; cdecl;
begin
  Result := ExceptionMgr.PopException(Msg);
end;


{*-----------------------------------------------------------------------------|
| Randomize.pas                                                                |
|-----------------------------------------------------------------------------*}
procedure exp_RandomTPA(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PPointArray(Result)^ := RandomTPA(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure exp_RandomCenterTPA(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PPointArray(Result)^ := RandomCenterTPA(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

function exp_RandomTIA(var se:SExt; Amount, Low,Hi:Int32): TIntArray; cdecl;
begin
  Result := RandomTIA(amount,low,hi);
end;

(*procedure exp_RandomTIA(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PIntArray(Result)^ := RandomTIA(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;*)


{*-----------------------------------------------------------------------------|
| CornerDet.pas                                                                |
|-----------------------------------------------------------------------------*}
procedure exp_CornerResponse(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  P2DFloatArray(Result)^ := CornerResponse(P2DIntArray(Params^[1])^, PSingle(Params^[2])^, PInt32(Params^[3])^);
end;

procedure exp_FindCornerPoints(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PPointArray(Result)^ := FindCornerPoints(P2DIntArray(Params^[1])^, PSingle(Params^[2])^, PInt32(Params^[3])^, PSingle(Params^[4])^, PInt32(Params^[5])^);
end;

procedure exp_FindCornerMidPoints(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PPointArray(Result)^ := FindCornerMidPoints(P2DIntArray(Params^[1])^, PSingle(Params^[2])^, PInt32(Params^[3])^, PSingle(Params^[4])^, PInt32(Params^[5])^);
end;



{*-----------------------------------------------------------------------------|
| Just some other stuff                                                        |
|-----------------------------------------------------------------------------*}
procedure exp_LoadLibrary(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PNativeInt(Result)^ := LoadLibrary(PAnsiString(Params^[1])^);
end;

procedure exp_GetProcAddress(Params: PParamArray; const Result:Pointer); cdecl; export;
begin
  PNativeInt(Result)^ := PtrInt(GetProcAddress(PNativeInt(Params^[1])^, PAnsiString(Params^[2])^));
end;

procedure exp_FreeLibrary(Params: PParamArray); cdecl; export;
begin
  FreeLibrary(PNativeInt(Params^[1])^);
end;

