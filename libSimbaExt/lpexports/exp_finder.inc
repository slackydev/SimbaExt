{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports finder methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@TFinder_Init,		'function SimbaExt.InitFinder(ComparePreset:EColorDistance; NumThreads:Int8): TFinder;');
AddMethod(@TFinder_Free,		'procedure TFinder.Free();');
AddMethod(@TFinder_SetComparePreset,	'procedure TFinder.SetComparePreset(ComparePreset:EColorDistance);');
AddMethod(@TFinder_SetCompareInfo,	'procedure TFinder.SetCompareInfo(Cmp: TColorDistFunc; ColorSpace:Int32);');
AddMethod(@TFinder_GetCompareInfo,	'procedure TFinder.GetCompareInfo(out Cmp: TColorDistFunc; out ColorSpace:Int32);');
AddMethod(@TFinder_SetNumThreads,	'procedure TFinder.SetNumThreads(N:Int32);');
AddMethod(@TFinder_GetNumThreads,	'function TFinder.GetNumThreads(): Int32;');
AddMethod(@TFinder_SetUseLookup,	'procedure TFinder.SetUseLookup(Use:LongBool);');
AddMethod(@TFinder_GetUseLookup,	'function TFinder.GetUseLookup(): LongBool;');
AddMethod(@TFinder_MatchColor,		'function TFinder.MatchColor(var ImgArr:TIntMatrix; Color:Int32): TFloatMatrix;');
AddMethod(@TFinder_SafeMatchColor,	'function TFinder.SafeMatchColor(var ImgArr:TIntMatrix; Color:Int32): TFloatMatrix;');