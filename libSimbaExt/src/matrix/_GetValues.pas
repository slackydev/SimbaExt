{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Returns the values at the given points.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define GetValuesBody :=
  L := High(Indices);
  H := High(Mat);
  if H < 0 then
    NewException(exEmptyMatrix);

  W := High(Mat[0]);
  SetLength(Result, L+1);
  c := 0;
  for i:=0 to L do
    if (Indices[i].x >= 0) and (Indices[i].y >= 0) then
      if (Indices[i].x <= W) and (Indices[i].y <= H) then
      begin
        Result[c] := Mat[Indices[i].y][Indices[i].x];
        Inc(c);
      end;
  SetLength(Result, c);
}

function GetValues(const Mat:T2DByteArray; const Indices:TPointArray): CoreTypes.TByteArray; overload;
var i,W,H,c,L:Int32;
begin GetValuesBody end;


function GetValues(const Mat:T2DIntArray; const Indices:TPointArray): TIntArray; overload;
var i,W,H,c,L:Int32;
begin GetValuesBody end;


function GetValues(const Mat:T2DFloatArray; const Indices:TPointArray): TFloatArray; overload;
var i,W,H,c,L:Int32;
begin GetValuesBody end;


function GetValues(const Mat:T2DDoubleArray; const Indices:TPointArray): TDoubleArray; overload;
var i,W,H,c,L:Int32;
begin GetValuesBody end;


function GetValues(const Mat:T2DExtArray; const Indices:TPointArray): TExtArray; overload;
var i,W,H,c,L:Int32;
begin GetValuesBody end;
