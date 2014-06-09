{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{*
 Find the N-minimum or N-maximum values in the matrix, and return their indices.
*}
function ArgMulti(Mat:T2DByteArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArray;
begin
  H := High(Mat);
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

  W += 1;
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;

function ArgMulti(Mat:T2DIntArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayI;
begin
  H := High(Mat);
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

  W += 1;
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;

function ArgMulti(Mat:T2DExtArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArray;
begin
  H := High(Mat);
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

  W += 1;
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;

function ArgMulti(Mat:T2DDoubleArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayD;
begin
  H := High(Mat);
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

  W += 1;
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;


function ArgMulti(Mat:T2DFloatArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var
  W,H,i,y,x,width: Int32;
  data:THeapArrayF;
begin
  H := High(Mat);
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

  W += 1;
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;




