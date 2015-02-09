{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{*
 Find the minimum and maximum values in the matrix.
*}
{$define MinMaxBody :=
  H := High(Mat);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Mat[0]);

  Min := Mat[0][0];
  Max := Mat[0][0];
  for Y:=0 to H do
    for X:=0 to W do
      if Mat[Y][X] > Max then
        Max := Mat[Y][X]
      else if Mat[Y][X] < Min then
        Min := Mat[Y][X];
}

procedure MinMax(Mat:T2DByteArray; var Min, Max:Byte); overload;
var X,Y,W,H: Integer;
begin MinMaxBody end;

procedure MinMax(Mat:T2DIntArray; var Min, Max:Integer); overload;
var X,Y,W,H: Integer;
begin MinMaxBody end;

procedure MinMax(Mat:T2DExtArray; var Min, Max:Extended); overload;
var X,Y,W,H: Integer;
begin MinMaxBody end;

procedure MinMax(Mat:T2DDoubleArray; var Min, Max:Double); overload;
var X,Y,W,H: Integer;
begin MinMaxBody end;

procedure MinMax(Mat:T2DFloatArray; var Min, Max:Single); overload;
var X,Y,W,H: Integer;
begin MinMaxBody end;




