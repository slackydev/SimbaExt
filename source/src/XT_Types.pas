unit XT_Types;
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
  TPoint = packed record X,Y: LongInt; end;
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;
  T3DPointArray = array of T2DPointArray;

  TFPoint = Packed record X,Y:Extended; end;
  TFPointArray = Array of TFPoint;
  
  TIntArray = array of Integer;
  T2DIntArray = array of TIntArray;
  T3DIntArray = array of T2DIntArray;

  TByteArray = array of Byte;
  T2DByteArray = array of TByteArray;
  T3DByteArray = array of T2DByteArray;  
  
  TBoolArray = array of Boolean;
  T2DBoolArray = array of TBoolArray;
  T3DBoolArray = array of T2DBoolArray;
  
  TExtArray = array of Extended;
  T2DExtArray = array of TExtArray;
  T3DExtArray = array of T2DExtArray;
  
  TStrArray = array of String;
  TCharArray= array of Char;
  
  (* TBox is not SCAR compatible - Don't export functions that returns this *)
  TBox = packed record
    X1: LongInt;
    Y1: LongInt;
    X2: LongInt;
    Y2: LongInt;
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
  
  //
  TxAlignMethod = (AM_Extremes, AM_Convex, AM_BBox);
  TxThreshMethod = (TM_Mean, TM_MinMax);
  TxCenterMethod = (CM_Bounds, CM_BBox, CM_Mean, CM_Median);
  TxResizeMethod = (RM_Nearest, RM_Bilinear, RM_Bicubic);
  
  TChars = Array of T2DIntArray;  
  TCharsArray = Array of TChars;
  
  {
  TCharNew = record 
    Pts: TPointArray;
    X1,Y1,X2,Y2: LongInt;
    Height: LongInt;
    Width: LongInt;
  end;
  TCharsNew = array of TCharN;}

  

function Box(const x1,y1,x2,y2:Integer): TBox; Inline;
function Point(const x,y:Integer): TPoint; Inline;
function FPoint(const x,y:Extended):TFPoint; Inline;
function TFPAToTPA(TFPA:TFPointArray): TPointArray; 
function TPAToTFPA(TPA:TPointArray): TFPointArray;


//-----------------------------------------------------------------------
implementation

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
  
function FPoint(const X,Y:Extended):TFPoint; Inline;
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
