//Define types used in SimbaExt 
type
  PInt8  = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;
  
  PUInt8  = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;

  Float32 = Single;
  Float64 = Double;
  Float80 = Extended;
  PFloat32 = ^Single;
  PFloat64 = ^Double;
  PFloat80 = ^Extended;
  
  
  //Arr
  TIntArray    = TIntegerArray;
  TFloatArray  = Array of Float32;
  TDoubleArray = Array of Float64;
  TExtArray    = Array of Float80;
  
  
  //Spesific names
  TInt8Array   = Array of Int8;
  TInt16Array  = Array of Int16;
  TInt32Array  = Array of Int32;
  TInt64Array  = Array of Int64;
  TFloat32Array  = Array of Float32;
  TFloat64Array  = Array of Float64;
  TFloat80Array  = Array of Float80;
  
  
  //Array of Array ..
  (*T2DIntArray     = Array of TIntArray;*)
  T2DExtArray     = Array of TExtArray;
  T2DFloatArray   = Array of TFloatArray;
  T2DDoubleArray  = Array of TDoubleArray;
  T2DBoxArray     = Array of TBoxArray;
  
  
  //Matrix ..
  TByteMatrix   = Array of TByteArray;
  TIntMatrix    = Array of TIntArray;
  TExtMatrix    = Array of TExtArray;
  TFloatMatrix  = Array of TFloatArray;
  TDoubleMatrix = Array of TDoubleArray;

  
  //Other ..
  TColorSpace = (_LAB_, _LCH_, _XYZ_, _RGB_);
  TWarningType = (ERR_DEPRECATED, ERR_WARNING, ERR_NOTICE);
  
  TSortKey = (sort_Default, sort_Magnitude, sort_ByColumn, sort_ByRow, sort_ByX,
              sort_ByY, sort_Length, sort_First, sort_Index, sort_Mean, sort_Lex, sort_Logical);
  

  (* Prefixes for SimbaExt modules *)
  //SE.***
  SimbaExt = type Pointer;
  //Math.***
  TObjMath = type Pointer;
  //Rand.***
  TObjRandom = type Pointer;
  //Time.***
  TObjTime = type Pointer;
  //OS.*** & OS.Path.***
  TObjOSPath = type Pointer;
  TObjOS = record path: TObjOSPath; end; 
var  
  SE: SimbaExt; 
  Math: TObjMath; 
  Rand: TObjRandom; 
  TimeUtils: TObjTime;
  OS: TObjOS;
  

  
//LoadLibrary from current folder 
{$IFNDEF CODEINSIGHT}
  {$loadlib \..\includes\simbaext\simbaext.dll}
  {$loadlib \..\includes\simbaext\seextra.dll}
  {$loadlib \..\includes\simbaext\matchTempl.dll}
{$ELSE}
//types are exported from SimbaExt.dll, but we must show am in codeinsight
  type TAlignAlgo  = (AA_BOUNDS, AA_CHULL, AA_BBOX);
  type TThreshAlgo = (TA_MEAN, TA_MINMAX);
  type TCenterAlgo = (CA_BOUNDS, CA_BBOX, CA_MEAN, CA_MEDIAN);
  type TResizeAlgo = (RA_NEAREST, RA_BILINEAR, RA_BICUBIC);
  type TCCorrMode  = (CC_EUCLID, CC_EUCLID_NORMED, CC_EUCLID_SQUARED, CC_CHEB, CC_CHEB_NORMED);
  type TComparator = (__LT__, __GT__, __EQ__, __NE__, __GE__, __LE__);
{$ENDIF}


// IntToBox is to long, and [x1,y1,x2,y2] fails whenever used with a overloaded method..
{$IFNDEF AeroLib}
function ToBox(x1,y1,x2,y2: Integer): TBox;
{$ELSE}
function ToBox(x1,y1,x2,y2: Integer): TBox; override;
{$ENDIF}
begin
  Result := [x1,y1,x2,y2];
end;


function ToBox(TopLeft, BtmRight:TPoint): TBox; overload;
begin
  Result := [TopLeft.x, TopLeft.y, BtmRight.x, BtmRight.y];
end;


{!DOCREF} {
  @method: procedure RaiseWarning(WarningMessage:String; Warn: TWarningType);
  @desc: Used internally to raise a warning-message
}
procedure RaiseWarning(WarningMessage:String; Warn: TWarningType);
begin
  {$IFNDEF ERR_HIDE_ALL}
  case Warn of
    ERR_DEPRECATED: 
      begin
        {$IFNDEF ERR_HIDE_DEPRECATED}
        WriteLn('DEPRECATED: ' + WarningMessage);
        {$ENDIF}
      end;
    ERR_WARNING: 
      begin
        {$IFNDEF ERR_HIDE_WARNINGS}
          {$IFDEF ERR_REAL_EXCEPTION}
            RaiseException(erException, 'WARNING: '+WarningMessage);
          {$ELSE}
            WriteLn('WARNING: ' + WarningMessage);
          {$ENDIF}
        {$ENDIF}
      end;
    ERR_NOTICE:  
      begin    
        {$IFNDEF ERR_HIDE_NOTICE}
        WriteLn('NOTICE: ' + WarningMessage);
        {$ENDIF}
      end;
  end;
  {$ENDIF}
end;