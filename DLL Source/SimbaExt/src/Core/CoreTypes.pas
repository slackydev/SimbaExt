unit CoreTypes;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  SysUtils;

type
  //TPoint of Float
  TFPoint = packed record X,Y:Double; end;
  TFPointArray = Array of TFPoint;

  //Int
  TIntArray = array of Integer;
  T2DIntArray = array of TIntArray;
  T3DIntArray = array of T2DIntArray;

  //Byte
  TByteArray = array of Byte;
  T2DByteArray = array of TByteArray;
  T3DByteArray = array of T2DByteArray;  

  //Bool
  TBoolArray = array of Boolean;
  T2DBoolArray = array of TBoolArray;
  T3DBoolArray = array of T2DBoolArray;

  //Extended
  TExtArray = array of Extended;
  T2DExtArray = array of TExtArray;
  T3DExtArray = array of T2DExtArray;

  //Double
  TDoubleArray = array of Double;
  T2DDoubleArray = array of TDoubleArray;
  T3DDoubleArray = array of T2DDoubleArray;

  //Float (aka single)
  TFloatArray = array of Single;
  T2DFloatArray = array of TFloatArray;
  T3DFloatArray = array of T2DFloatArray;


  //String+Char
  TStringArray = array of String;
  TStrArray    = array of String;
  TCharArray   = array of Char;


  //TPoint
  TPoint = packed record
    X,Y: LongInt;
    function InBox(x1,y1,x2,y2:Int32): Boolean;
  end;

  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;
  T3DPointArray = array of T2DPointArray;


  //TBox
  TBox = packed record
    X1, Y1, X2, Y2: LongInt;
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    function Center: TPoint;
    procedure Expand(const SizeChange: Integer);
  end;
  TBoxArray = Array of TBox;
  T2DBoxArray = Array of TBoxArray;


  //Regular datatypes w/pointers
  Float32 = Single;
  Float64 = Double;
  Float80 = Extended;

  PFloat32 = ^Single;
  PFloat64 = ^Double;
  PFloat80 = ^Extended;
  PInt8  = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;
  PUInt8  = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;
  
  //
  TAlignAlgo     = (AA_BOUNDS, AA_CHULL, AA_BBOX);
  TThreshAlgo    = (TA_MEAN, TA_MINMAX);
  TCenterAlgo    = (CA_BOUNDS, CA_BBOX, CA_MEAN, CA_Median);
  TResizeAlgo    = (RA_NEAREST, RA_BILINEAR, RA_BICUBIC);

  //Color correlation algorithm
  TCCorrMode = (CC_EUCLID, CC_EUCLID_NORMED, CC_EUCLID_SQUARED, CC_CHEB, CC_CHEB_NORMED);
  
  //Comperison operator
  TComparator = (__LT__, __GT__, __EQ__, __NE__, __GE__, __LE__);
  
  TChars = Array of T2DIntArray;
  TCharsArray = Array of TChars;

  ColorLAB = packed record L, A, B: Single; end;
  ColorHSV = packed record H, S, V: Single; end;
  ColorRGB = packed record R, G, B, A: UInt8;  end;
  
  
  PRGB32 = ^TRGB32;
  TRGB32 = packed record B, G, R, A: UInt8; end;
  

function Box(const x1,y1,x2,y2:Integer): TBox; Inline;
function Point(const x,y:Integer): TPoint; Inline;
function FPoint(const x,y:Extended):TFPoint; Inline;
function TFPAToTPA(TFPA:TFPointArray): TPointArray; 
function TPAToTFPA(TPA:TPointArray): TFPointArray;


//-----------------------------------------------------------------------
implementation
uses math;

function TBox.GetWidth: Integer;
begin 
  Result := (X2-X1+1); 
end;

function TBox.GetHeight: Integer;
begin 
  Result := (Y2-Y1+1); 
end;

function TBox.Center: TPoint;
begin
  Result.X := Self.X1 + (GetWidth div 2);
  Result.Y := Self.Y1 + (GetHeight div 2);
end;

procedure TBox.Expand(const SizeChange: Integer);
begin
  Self.X1 := Self.X1 - SizeChange;
  Self.Y1 := Self.Y1 - SizeChange;
  Self.X2 := Self.X2 + SizeChange;
  Self.Y2 := Self.Y2 + SizeChange;
end;

function TPoint.InBox(x1,y1,x2,y2:Int32): Boolean;
begin
  Result := InRange(Self.x, x1, x2) and InRange(Self.y, y1, y2);
end;

function Box(const X1,Y1,X2,Y2:Integer): TBox; Inline;
begin
  Result.x1 := x1;
  Result.y1 := y1;
  Result.x2 := x2;
  Result.y2 := y2;
end;    
  
function Point(const X, Y: Integer): TPoint; Inline;
begin
  Result.X := X;
  Result.Y := Y;
end;  
  
function FPoint(const X,Y:Extended): TFPoint; Inline;
begin
  Result.X := X;
  Result.Y := Y;
end; 
 
 
function TFPAToTPA(TFPA:TFPointArray): TPointArray;
var i:Integer;
begin
  SetLength(Result, Length(TFPA));
  for i:=0 to High(TFPA) do
    Result[i] := Point(Round(TFPA[i].x), Round(TFPA[i].y));
end;


function TPAToTFPA(TPA:TPointArray): TFPointArray;
var i:Integer;
begin
  SetLength(Result, Length(TPA));
  for i:=0 to High(TPA) do
  begin
    Result[i].x := TPA[i].x;
    Result[i].y := TPA[i].y;
  end;
end;
 

end.
