{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Put all the values in to the matrix at given position.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$define PutValuesBody :=
  L := High(Indices);
  H := High(Matrix);
  if (H = -1) then NewException(exEmptyMatrix);
  W := High(Matrix[0]);

  if (Length(Values) <> 1) and (Length(Values) <> Length(Indices)) then
    NewException('Length of Values must be 1, or same length as Indices');

  c := 0;
  if (Length(Values) = 1) then begin
    for i := 0 to L do
      if (Indices[i].x >= 0) and (Indices[i].y >= 0) then
        if (Indices[i].x <= W) and (Indices[i].y <= H) then
          Matrix[Indices[i].y][Indices[i].x] := Values[0];
  end else
    for i := 0 to L do
      if (Indices[i].x >= 0) and (Indices[i].y >= 0) then
        if (Indices[i].x <= W) and (Indices[i].y <= H) then
        begin
          Matrix[Indices[i].y][Indices[i].x] := Values[c];
          inc(c);
        end;
}

procedure PutValues(var Matrix:T2DByteArray; const Indices:TPointArray; Values:CoreTypes.TByteArray); 
var i,W,H,c,L:Int32;
begin PutValuesBody end;


procedure PutValues(var Matrix:T2DIntArray; const Indices:TPointArray; Values:TIntArray); overload;
var i,W,H,c,L:Int32;
begin PutValuesBody end;


procedure PutValues(var Matrix:T2DFloatArray; const Indices:TPointArray; Values:TFloatArray); overload; 
var i,W,H,c,L:Int32;
begin PutValuesBody end;


procedure PutValues(var Matrix:T2DDoubleArray; const Indices:TPointArray; Values:TDoubleArray); overload; 
var i,W,H,c,L:Int32;
begin PutValuesBody end;


procedure PutValues(var Matrix:T2DExtArray; const Indices:TPointArray; Values:TExtArray); overload; 
var i,W,H,c,L:Int32;
begin PutValuesBody end;

