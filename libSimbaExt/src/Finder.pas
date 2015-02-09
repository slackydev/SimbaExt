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
  TColorDistFunc = function(Color1:TColorInfo; Color2:Integer): Single; cdecl;
  
  TCorrThread = class(TThread)
  protected
    FMethod: TColorDistFunc;
    FData: P2DIntArray;
    FDest: P2DFloatArray;
    FLo, FHi:Int32;
    FInfo: TColorInfo;
    FLookup:PFloatArray;
    procedure Execute; override;
  public
    Exectued: Boolean;
    property Terminated;
    constructor Create(Method:TColorDistFunc; Info:TColorInfo; Lo,Hi: Int32);

    procedure SetIO(const Input: P2DIntArray; const Output: P2DFloatArray);
    procedure SetLookup(const Table: PFloatArray);
  end;

  TThreadPool = Array of TCorrThread;


  PFinder = ^TFinder;
  TFinder = record
  private
    FCompareFunc:TColorDistFunc;
    FNumThreads: Int32;
    FColorInfo : TColorInfo;
    FColorSpace: Int32;
    FUseLookup : Boolean;
    procedure SetupColorInfo(Color:Int32); 
    procedure FreeColorInfo();   
    procedure SetupLookup(Reset:Boolean=False);
    procedure FreeLookup();
  public
    FLookupTable : TFloatArray;
    procedure Init(CompareFunc:TColorDistFunc; ColorSpace:Int32; NumThreads:Int8);
    procedure Init(ComparePreset:EColorDistance; NumThreads:Int8); overload;
    procedure Free;

    procedure SetCompareInfo(CompareFunc:TColorDistFunc; ColorSpace:Int32);
    procedure GetCompareInfo(out CompareFunc:TColorDistFunc; out ColorSpace:Int32);
    procedure SetComparePreset(ComparePreset:EColorDistance);
    procedure SetNumThreads(NumThreads:Int8);
    function GetNumThreads(): Int32;
    procedure SetUseLookup(Use:Boolean);
    function GetUseLookup(): Boolean;
    
    function SafeMatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
    function MatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
  end;


(* old crap *)
function ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; Cdecl;


//--------------------------------------------------
implementation

uses
  Math, CoreMath, Colormath, Colordist, PointList, ExceptionMgr;


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
    ECD_XYZ_NORMED:	  Method := @Distance_XYZ_Normed;
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

(*----| WorkThread |----------------------------------------------------------*)

constructor TCorrThread.Create(Method:TColorDistFunc; Info:TColorInfo; Lo,Hi: Int32);
begin
  FreeOnTerminate := True;
  FLo := Lo;
  FHi := Hi;
  FInfo := Info;
  FMethod := Method;
  inherited Create(True);
end;

(*
  Sets the input and output image
*)
procedure TCorrThread.SetIO(const Input: P2DIntArray; const Output: P2DFloatArray);
begin
  FData := Input;
  FDest := Output;
end;

(*
  Adds a lookuptable (if wanted)
*)
procedure TCorrThread.SetLookup(const Table: PFloatArray);
begin
  FLookup := Table;
end;


(*
  Do work..
*)
procedure TCorrThread.Execute;
var
  W,X,Y:Integer;
  P:Single;
begin
  W := High(FData^[0]);
  if (Length(FLookup^) <> 0) then
    for y:=FLo to FHi do
      for x:=0 to W do
      begin
        if (FData^[y,x] < $1000000) and (FData^[y,x] > -1) then
        begin
          P := FLookup^[FData^[y,x]];
          if Int32(P) = -1 then
          begin
            P := FMethod(FInfo,FData^[y,x]);
            FLookup^[FData^[y,x]] := P;
          end;
          FDest^[y,x] := P;
        end else
          FDest^[y,x] := FMethod(FInfo,FData^[y,x]);
      end
  else
    for y:=FLo to FHi do
      for x:=0 to W do
        FDest^[y,x] := FMethod(FInfo,FData^[y,x]);

  Exectued := True;
end;





(*----| TFinder |-------------------------------------------------------------*)
procedure TFinder.Init(CompareFunc:TColorDistFunc; ColorSpace:Int32; NumThreads:Int8);
begin
  FCompareFunc := CompareFunc;
  FColorSpace  := Max(0,ColorSpace);
  if FColorSpace > 3 then FColorSpace := 0;
  FNumThreads  := Max(1,NumThreads);
  FColorInfo   := nil;
  FUseLookup   := False;
  SetLength(FLookupTable,0);
end;


procedure TFinder.Init(ComparePreset:EColorDistance; NumThreads:Int8); overload;
begin
  FNumThreads  := Max(1,NumThreads);
  FColorInfo   := nil;
  FUseLookup   := False;
  PresetToMethod(ComparePreset, FColorSpace, FCompareFunc);
end;


procedure TFinder.Free;
begin
  if FColorInfo <> nil then FreeMem(FColorInfo);
  if FLookupTable <> nil then SetLength(FLookupTable,0);
  FCompareFunc := nil;
  FColorSpace  := 0;
  FNumThreads  := 1;
  FUseLookup   := False;
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


procedure TFinder.SetupLookup(Reset:Boolean=False);
var
  Fill:Boolean;
  P:PSingle;
  H:PtrUInt;
begin
  if FUseLookup then
  begin
    Fill := (Length(FLookupTable) = 0);
    if Fill then
      SetLength(FLookupTable, 256**3);
    
    // Fill with NaN
    if Reset or Fill then
    begin
      P := PSingle(FLookupTable);
      H := PtrUInt(@FLookupTable[$FFFFFF]);
      while PtrUInt(P) <= H do 
      begin
        PInt32(P)^ := -1;
        Inc(P);
      end;
    end;
  end;
end;



procedure TFinder.FreeLookup(); 
begin
  if Length(FLookupTable) <> 0 then  
    SetLength(FLookupTable,0);
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
procedure TFinder.SetNumThreads(NumThreads:Int8);
begin
  FNumThreads := Max(1,NumThreads);
end;

function TFinder.GetNumThreads(): Int32;
begin
  Result := FNumThreads;
end;


(*
  Get & Set if we use want to use a large lookuptable for faster
  cross correlation (might be prefered if the images are LARGE)

  False by default
*)
procedure TFinder.SetUseLookup(Use:Boolean);
begin
  FUseLookup := Use;
  if FUseLookup then 
    Self.SetupLookup()
  else
    Self.FreeLookup();
end;

function TFinder.GetUseLookup(): Boolean;
begin
  Result := FUseLookup;
end;


  
(*----| Methods |-------------------------------------------------------------*)
(*
  Threaded cross-correlate a color with an image
*)
function TFinder.SafeMatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
var
  H,W,i,lo,hi,dev: Int32;
  nThreads:Int32;
  Pool: TThreadPool;
begin
  H := Length(ImgArr);
  if (H = 0) then NewException('Matrix must be initalized');
  W := Length(ImgArr[0]);

  Self.SetupLookup(True);
  Self.SetupColorInfo(Color);

  nThreads := Min(FNumThreads,H+1);
  SetLength(Pool, nThreads);

  SetLength(Result,H,W);

  lo := 0;
  dev := H div nThreads;
  for i:=1 to nThreads do
  begin
    hi := i * dev;
    if (i = nThreads) then hi := H-1;
    Pool[i-1] := TCorrThread.Create(FCompareFunc, FColorInfo, Lo,Hi);
    Pool[i-1].SetIO(@ImgArr, @Result);
    Pool[i-1].SetLookup(@(FLookupTable));
    Pool[i-1].Start;
    lo := hi + 1;
  end;

  for i:=0 to nThreads-1 do begin
    while not(Pool[i].Exectued) do Sleep(0);
    Pool[i].Terminate();
  end;
  
  Self.FreeColorInfo();
end;


(*
  Threaded cross-correlate a color with an image
  Sideaffect of using this method: Modifies the input image
*)
function TFinder.MatchColor(constref ImgArr:T2DIntArray; Color:Integer): T2DFloatArray;
var
  H,i,lo,hi,dev: Int32;
  nThreads:Int32;
  Pool: TThreadPool;
begin
  H := Length(ImgArr);
  if (H = 0) then NewException('Matrix must be initalized');

  nThreads := Min(FNumThreads,H+1);
  SetLength(Pool, nThreads);

  Self.SetupLookup(True);
  Self.SetupColorInfo(Color);

  lo := 0;
  dev := H div nThreads;
  for i:=1 to nThreads do
  begin
    hi := i * dev;
    if (i = nThreads) then hi := H-1;
    Pool[i-1] := TCorrThread.Create(FCompareFunc, FColorInfo, Lo,Hi);
    Pool[i-1].SetIO(@ImgArr, @ImgArr);
    Pool[i-1].SetLookup(@(FLookupTable));
    Pool[i-1].Start;
    lo := hi + 1;
  end;

  for i:=0 to nThreads-1 do
  begin
    while not(Pool[i].Exectued) do Sleep(0);
    Pool[i].Terminate();
  end;

  Self.FreeColorInfo();
  Result := T2DFloatArray(ImgArr);
end;







{*
 .... other
*}


// Find multiple matches of specified color.
function ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; cdecl;
var
  W,H,X,Y:Integer;
  C1,C2:ColorRGB;
  TPS: TPointList;
begin
  Result := True;
  W := High(ImgArr[0]);
  H := High(ImgArr);
  TPS.Init;
  C1 := ColorToRGB(Color);
  Tol := Sqr(Tol);
  for Y:=0 to H do
    for X:=0 to W do
    begin
      C2 := ColorToRGB(ImgArr[Y][X]);
      if ((Sqr(C1.R - C2.R) + Sqr(C1.G - C2.G) + Sqr(C1.B - C2.B)) <= Tol) then
        TPS.Append(X,Y);
    end;

  if TPS.GetHigh=0 then Exit(False);
  TPA := TPS.Clone;
  TPS.Free;
end;


end.
