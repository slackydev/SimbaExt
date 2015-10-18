Unit Finder;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline on}
     
interface
uses
  CoreTypes, SysUtils, Classes;

type
  TColorInfo = Pointer;
  PColorInfo = ^TColorInfo;
  TColorDistFunc = function(Color1:TColorInfo; Color2:Integer): Single; cdecl;
  PColorDistFunc = ^TColorDistFunc;
  
  PFinder = ^TFinder;
  TFinder = record
  private
    FCompareFunc:TColorDistFunc;
    FNumThreads: Int32;
    FColorInfo : TColorInfo;
    FColorSpace: Int32;
    
    procedure SetupColorInfo(Color:Int32); 
    procedure FreeColorInfo();   
  public
    procedure Init(CompareFunc:TColorDistFunc; ColorSpace:Int32; NumThreads:Int8);
    procedure Init(ComparePreset:EColorDistance; NumThreads:Int8); overload;
    procedure Free;

    procedure SetCompareInfo(CompareFunc:TColorDistFunc; ColorSpace:Int32);
    procedure GetCompareInfo(out CompareFunc:TColorDistFunc; out ColorSpace:Int32);
    procedure SetComparePreset(ComparePreset:EColorDistance);
    procedure SetNumThreads(NumThreads:Int8);
    function GetNumThreads(): Int32;
    
    function SafeMatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
    function MatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
  end;


//--------------------------------------------------
implementation

uses
  Math,
  CoreMath,
  Colormath,
  Colordist,
  ExceptionMgr,
  ThreadPool,
  TimeUtils;


procedure PresetToMethod(Preset:EColorDistance; out ColorSpace:Integer; out Method: TColorDistFunc);
begin
  case Preset of
    ECD_RGB:          Method := @Distance_RGB;
    ECD_RGB_SQRD:     Method := @Distance_RGB_Sqrd;
    ECD_RGB_NORMED:   Method := @Distance_RGB_Normed;
    ECD_HSV:          Method := @Distance_HSV;
    ECD_HSV_SQRD:     Method := @Distance_HSV_Sqrd;
    ECD_HSV_NORMED:   Method := @Distance_HSV_Normed;
    ECD_XYZ:          Method := @Distance_XYZ;
    ECD_XYZ_SQRD:     Method := @Distance_XYZ_Sqrd;
    ECD_XYZ_NORMED:   Method := @Distance_XYZ_Normed;
    ECD_LAB:          Method := @Distance_LAB;
    ECD_LAB_SQRD:     Method := @Distance_LAB_Sqrd;
    ECD_LAB_NORMED:   Method := @Distance_LAB_Normed;
    ECD_DELTAE:       Method := @Distance_DeltaE;
    ECD_DELTAE_NORMED:Method := @Distance_DeltaE_Normed;
  end;

  if Preset in RGB_Comparators then
     ColorSpace := 0
  else if Preset in HSV_Comparators then
     ColorSpace := 1
  else if Preset in XYZ_Comparators then
     ColorSpace := 2
  else if Preset in LAB_Comparators then
     ColorSpace := 3;
end;


procedure ColorCorrelation(params:PParamArray);
var
  x,y:Int32;
  info:TColorInfo;
  method:TColorDistFunc;
  data:P2DIntArray;
  dest:P2DFloatArray;
  box:TBox;
begin
  method := PColorDistFunc(Params^[0])^;
  info := PColorInfo(Params^[1])^;
  data := Params^[2];
  dest := Params^[3];
  box  := PBox(Params^[4])^;
  for y:=box.y1 to box.y2 do
    for x:=box.x1 to box.x2 do
      dest^[y,x] := method(info, data^[y,x]);
end;


(*----| TFinder |-------------------------------------------------------------*)
procedure TFinder.Init(CompareFunc:TColorDistFunc; ColorSpace:Int32; NumThreads:Int8);
begin
  FCompareFunc := CompareFunc;
  FColorSpace  := Max(0,ColorSpace);
  if FColorSpace > 3 then FColorSpace := 0;
  FNumThreads  := Max(1,NumThreads);
  FColorInfo   := nil;
end;


procedure TFinder.Init(ComparePreset:EColorDistance; NumThreads:Int8); overload;
begin
  FNumThreads  := Max(1,NumThreads);
  FColorInfo   := nil;
  PresetToMethod(ComparePreset, FColorSpace, FCompareFunc);
end;


procedure TFinder.Free;
begin
  if FColorInfo <> nil then FreeMem(FColorInfo);
  FCompareFunc := nil;
  FColorSpace  := 0;
  FNumThreads  := 1;
end;


(*----| Setup/Unsetup |-------------------------------------------------------*)

procedure TFinder.SetupColorInfo(Color:Int32);
begin
  Case FColorSpace of
    0:begin
        FColorInfo := AllocMem(SizeOf(ColorRGB));
        PColorRGB(FColorInfo)^ := ColorToRGB(Color);
      end;
    1:begin
        FColorInfo := AllocMem(SizeOf(ColorHSV));
        PColorHSV(FColorInfo)^ := ColorToHSV(Color);
      end;
    2:begin
        FColorInfo := AllocMem(SizeOf(ColorXYZ));
        PColorXYZ(FColorInfo)^ := ColorToXYZ(Color);
      end;
    3:begin
        FColorInfo := AllocMem(SizeOf(ColorLAB));
        PColorLAB(FColorInfo)^ := ColorToLAB(Color);
      end;
    //not implemented (yet):
    {4:begin
        FColorInfo := AllocMem(SizeOf(ColorHSL));
        PColorHSL(FColorInfo)^ := ColorToHSL(Color);
      end;
    5:begin
        FColorInfo := AllocMem(SizeOf(ColorLCH));
        PColorLCH(FColorInfo)^ := ColorToLCH(Color);
      end;}
  end;
end;


procedure TFinder.FreeColorInfo(); 
begin
  if FColorInfo <> nil then
    FreeMem(FColorInfo);
  FColorInfo := nil;
end;



(*----| Getters+Setters |-----------------------------------------------------*)

(*
  Get & Set the compare method used to compute the difference between
  the given color, and each color on the image.

  The ColorSpace _MUST_ be compatible with the CompareFunc
*)
procedure TFinder.SetCompareInfo(CompareFunc:TColorDistFunc; ColorSpace:Int32);
begin
  FCompareFunc := CompareFunc;
  FColorSpace  := Max(0,ColorSpace);
  if FColorSpace > 3 then FColorSpace := 0;
end;

procedure TFinder.GetCompareInfo(out CompareFunc:TColorDistFunc; out ColorSpace:Int32);
begin
  CompareFunc := FCompareFunc;
  ColorSpace  := FColorSpace;
end;

(*
  Set the compare method used to compute the difference between
  the given color, and each color on the image using a preset.
*)
procedure TFinder.SetComparePreset(ComparePreset:EColorDistance);
begin
  PresetToMethod(ComparePreset, FColorSpace, FCompareFunc);
end;


(*
  Get & Set the number of threads to be used
*)
procedure TFinder.SetNumThreads(numThreads:Int8);
begin
  FNumThreads := Max(1,numThreads);
end;

function TFinder.GetNumThreads(): Int32;
begin
  Result := FNumThreads;
end;


  
(*----| Methods |-------------------------------------------------------------*)
(*
  Threaded cross-correlate a color with an image
*)
function TFinder.SafeMatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
var
  W,H: Int32;
begin
  H := Length(ImgArr);
  if (H = 0) then NewException('Matrix must be initalized');
  W := Length(ImgArr[0]);
  Self.SetupColorInfo(Color);
  SetLength(Result,H,W);
  ThreadPool.MatrixFunc(@ColorCorrelation, [@FCompareFunc, @FColorInfo, @ImgArr, @Result], W,H, FNumThreads);
  Self.FreeColorInfo();
end;


(*
  Threaded cross-correlate a color with an image
  Sideaffect of using this method: Modifies the input image
*)
function TFinder.MatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
var 
  W,H: Int32;
begin
  H := Length(ImgArr);
  if (H = 0) then NewException('Matrix must be initialized');
  W := Length(ImgArr[0]);
  Self.SetupColorInfo(Color);
  ThreadPool.MatrixFunc(@ColorCorrelation, [@FCompareFunc, @FColorInfo, @ImgArr, @ImgArr], W,H, FNumThreads);
  Self.FreeColorInfo();
  Result := T2DFloatArray(ImgArr);
end;




end.
