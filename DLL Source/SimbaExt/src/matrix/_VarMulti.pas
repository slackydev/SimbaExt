{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{*
 Find the N-minimum or N-maximum values in the matrix.
*}
function VarMulti(Mat:T2DByteArray; Count:Int32; HiLo:Boolean): CoreTypes.TByteArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArray;
begin
  H := High(Mat);
  if (length(mat) = 0) then 
    NewException('Matrix must be initalized');
    
  W := High(Mat[0]);
  width := w + 1;
  SetLength(Data, 0);
  case HiLo of
    True:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] > Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, True);
            hPush( data, mat[y,x], y*width+x, True);
          end;
    False:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] < Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, False);
            hPush( data, mat[y,x], y*width+x, False);
          end;
  end;

  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
    Result[i] := Data[i].value;
end;


function VarMulti(Mat:T2DIntArray; Count:Int32; HiLo:Boolean): TIntArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayI;
begin
  H := High(Mat);
  if (length(mat) = 0) then 
    NewException('Matrix must be initalized');
    
  W := High(Mat[0]);
  width := w + 1;
  SetLength(Data, 0);
  case HiLo of
    True:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] > Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, True);
            hPush( data, mat[y,x], y*width+x, True);
          end;
    False:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] < Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, False);
            hPush( data, mat[y,x], y*width+x, False);
          end;
  end;

  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
    Result[i] := Data[i].value;
end;


function VarMulti(Mat:T2DExtArray; Count:Int32; HiLo:Boolean): TExtArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArray;
begin
  H := High(Mat);
  if (length(mat) = 0) then 
    NewException('Matrix must be initalized');
    
  W := High(Mat[0]);
  width := w + 1;
  SetLength(Data, 0);
  case HiLo of
    True:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] > Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, True);
            hPush( data, mat[y,x], y*width+x, True);
          end;
    False:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] < Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, False);
            hPush( data, mat[y,x], y*width+x, False);
          end;
  end;

  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
    Result[i] := Data[i].value;
end;


function VarMulti(Mat:T2DDoubleArray; Count:Int32; HiLo:Boolean): TDoubleArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayD;
begin
  H := High(Mat);
  if (length(mat) = 0) then 
    NewException('Matrix must be initalized');
    
  W := High(Mat[0]);
  width := w + 1;
  SetLength(Data, 0);
  case HiLo of
    True:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] > Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, True);
            hPush( data, mat[y,x], y*width+x, True);
          end;
    False:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] < Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, False);
            hPush( data, mat[y,x], y*width+x, False);
          end;
  end;

  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
    Result[i] := Data[i].value;
end;



function VarMulti(Mat:T2DFloatArray; Count:Int32; HiLo:Boolean): TFloatArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayF;
begin
  H := High(Mat);
  if (length(mat) = 0) then 
    NewException('Matrix must be initalized');
    
  W := High(Mat[0]);
  width := w + 1;
  SetLength(Data, 0);
  case HiLo of
    True:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] > Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, True);
            hPush( data, mat[y,x], y*width+x, True);
          end;
    False:
      for y:=0 to H do
        for x:=0 to W do
          if (length(data) < count) or (mat[y,x] < Data[0].value) then
          begin
            if (length(data) = count) then hPop(data, False);
            hPush( data, mat[y,x], y*width+x, False);
          end;
  end;

  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
    Result[i] := Data[i].value;
end;




