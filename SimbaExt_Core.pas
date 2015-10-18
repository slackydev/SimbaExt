//Define types
type
  PInt8  = ^Int8;
  PUInt8  = ^UInt8;
  PInt16 = ^Int16;
  PUInt16 = ^UInt16;
  PInt32 = ^Int32;
  PUInt32 = ^UInt32;
  PInt64 = ^Int64;
  PUInt64 = ^UInt64;

  Float32 = Single;
  Float64 = Double;
  Float80 = Extended;
  PFloat32 = ^Single;
  PFloat64 = ^Double;
  PFloat80 = ^Extended;
  
  
  //Array
  TIntArray    = TIntegerArray; 
  TFloatArray  = Array of Single;
  TDoubleArray = Array of Double;
  TExtArray    = Array of Extended;
  
  
  //Array of Array ..
  //T2DIntArray     = Array of TIntArray;
  T2DExtArray     = Array of TExtArray;
  T2DFloatArray   = Array of TFloatArray;
  T2DDoubleArray  = Array of TDoubleArray;
  T2DBoxArray     = Array of TBoxArray;
  T2DBoolArray    = Array of TBoolArray;
  
  //Matrix ..
  TByteMatrix   = Array of TByteArray;
  TIntMatrix    = Array of TIntArray;
  TExtMatrix    = Array of TExtArray;
  TFloatMatrix  = Array of TFloatArray;
  TDoubleMatrix = Array of TDoubleArray;

  
  //Other ..
  EWarningType = (ERR_DEPRECATED, ERR_WARNING, ERR_NOTICE, ERR_FATAL);
  
  ESortKey = (sort_Default, sort_Magnetude, sort_ByColumn, sort_ByRow, sort_ByX,
              sort_ByY, sort_Length, sort_First, sort_Index, sort_Mean, sort_Lex, sort_Logical);

  
{|=====| Prefixes for SimbaExt modules |=====}
type SimbaExt = type Pointer;   //se.method
  
//LoadLibrary from current folder 
{$loadlib \..\includes\simbaext_beta\simbaext.dll}
{$loadlib \..\includes\simbaext_beta\matchTempl.dll}

var 
  se: SimbaExt;
  finder: TFinder;
  
  

// IntToBox is to long, and [x1,y1,x2,y2] fails whenever used with a overloaded method..
function ToBox(x1,y1,x2,y2: Integer): TBox; {$IFDEF AeroLib}override;{$ENDIF}
begin
  Result := [x1,y1,x2,y2];
end;


function ToBox(TopLeft, BtmRight:TPoint): TBox; overload;
begin
  Result := [TopLeft.x, TopLeft.y, BtmRight.x, BtmRight.y];
end;


{!DOCREF} {
  @method: procedure RaiseWarning(message:String; eType: EWarningType);
  @desc: Used internally to raise a warning-message
}
procedure RaiseWarning(message:String; eType: EWarningType);
begin
  {$IFNDEF E_HIDE_ALL}
  case eType of
    ERR_DEPRECATED:
      {$IFNDEF E_HIDE_DEPRECATED}
      WriteLn('DEPRECATED: ' + message);
      {$ENDIF}
    ERR_WARNING:
      {$IFNDEF E_HIDE_WARNING}
      WriteLn('WARNING: ' + message);
      {$ENDIF}
    ERR_NOTICE:
      {$IFNDEF E_HIDE_NOTICE}
      WriteLn('NOTICE: ' + message);
      {$ENDIF}
    ERR_FATAL:
      {$IFNDEF E_HIDE_FATAL}
      WriteLn('FATAL: ' + message);
      {$ENDIF}
  end;
  {$ENDIF}
  
  if eType = ERR_FATAL then
    TerminateScript();
end;



function SimbaExt.GetException(): String; overload;
begin
  se.GetException(Result);
end;



procedure __SE_FREE_MEM;
begin
  Finder.Free();
end;

var
  cores:Int32;
begin
  //windows only
  cores := StrToInt64Def( GetEnvironmentVariable('NUMBER_OF_PROCESSORS'), 6);
  //linux cores := SysConf(83);
  Finder := se.InitFinder(ECD_RGB_NORMED, cores);
  AddOnTerminate('__SE_FREE_MEM');
end;
