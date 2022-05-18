{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap finder module (finder.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
function TFinder_Init(var se:SExt; ComparePreset:EColorDistance; NumThreads:Int8): TFinder; cdecl;
begin
  Result.Init(ComparePreset, NumThreads);
end;

procedure TFinder_Free(var Finder:TFinder); cdecl;
begin
  Finder.Free();
end;

procedure TFinder_SetComparePreset(var Finder:TFinder; ComparePreset:EColorDistance); cdecl;
begin
  Finder.SetComparePreset(ComparePreset);
end;

procedure TFinder_SetCompareInfo(var Finder:TFinder; CompareFunc:TColorDistFunc; ColorSpace:Int32); cdecl;
begin
  Finder.SetCompareInfo(CompareFunc,ColorSpace);
end;

procedure TFinder_GetCompareInfo(var Finder:TFinder; out CompareFunc:TColorDistFunc; out ColorSpace:Int32); cdecl;
begin
  Finder.GetCompareInfo(CompareFunc,ColorSpace);
end;

procedure TFinder_SetNumThreads(var Finder:TFinder; Threads:Int32); cdecl;
begin
  Finder.SetNumThreads(Threads);
end;

function TFinder_GetNumThreads(var Finder:TFinder): Int32; cdecl;
begin
  Result := Finder.GetNumThreads();
end;

function TFinder_MatchColor(var Finder:TFinder; var ImgArr:T2DIntArray; Color:Int32): T2DFloatArray; cdecl;
begin
  Result := Finder.MatchColor(ImgArr, Color);
end;

function TFinder_SafeMatchColor(var Finder:TFinder; var ImgArr:T2DIntArray; Color:Int32): T2DFloatArray; cdecl;
begin
  Result := Finder.SafeMatchColor(ImgArr, Color);
end;
