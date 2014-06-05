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
  W,H,i,size,l: Int32;
  tmp:CoreTypes.TByteArray;
  data:THeapArray;
begin
  H := High(Mat);
  W := High(Mat[0]);
  size := 0;
  for i:=0 to H do
  begin
    L := W+1;
    SetLength(tmp, size+L);
    Move(Mat[i][0], tmp[size], L*SizeOf(Byte));
    size += L;
  end;
  
  case HiLo of
    True:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] > Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, True);
          hPush( data, tmp[i], i, True);
        end;
    False:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] < Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, False);
          hPush( data, tmp[i], i, False);
        end;
  end;

  W += 1; 
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
  begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;


function ArgMulti(Mat:T2DIntArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var 
  W,H,i,size,l: Int32;
  tmp:TIntArray;
  data:THeapArrayI;
begin
  H := High(Mat);
  W := High(Mat[0]);
  size := 0;
  for i:=0 to H do
  begin
    L := W+1;
    SetLength(tmp, size+L);
    Move(Mat[i][0], tmp[size], L*SizeOf(Int32));
    size += L;
  end;
  
  case HiLo of
    True:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] > Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, True);
          hPush( data, tmp[i], i, True);
        end;
    False:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] < Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, False);
          hPush( data, tmp[i], i, False);
        end;
  end;

  W += 1; 
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
  begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;


function ArgMulti(Mat:T2DExtArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var 
  W,H,i,size,l: Int32;
  tmp:TExtArray;
  data:THeapArray;
begin
  H := High(Mat);
  W := High(Mat[0]);
  size := 0;
  for i:=0 to H do
  begin
    L := W+1;
    SetLength(tmp, size+L);
    Move(Mat[i][0], tmp[size], L*SizeOf(Extended));
    size += L;
  end;
  
  case HiLo of
    True:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] > Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, True);
          hPush( data, tmp[i], i, True);
        end;
    False:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] < Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, False);
          hPush( data, tmp[i], i, False);
        end;
  end;

  W += 1; 
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
  begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;


function ArgMulti(Mat:T2DDoubleArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var 
  W,H,i,size,l: Int32;
  tmp:TDoubleArray;
  data:THeapArrayD;
begin
  H := High(Mat);
  W := High(Mat[0]);
  size := 0;
  for i:=0 to H do
  begin
    L := W+1;
    SetLength(tmp, size+L);
    Move(Mat[i][0], tmp[size], L*SizeOf(Double));
    size += L;
  end;
  
  case HiLo of
    True:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] > Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, True);
          hPush( data, tmp[i], i, True);
        end;
    False:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] < Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, False);
          hPush( data, tmp[i], i, False);
        end;
  end;

  W += 1; 
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
  begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;



function ArgMulti(Mat:T2DFloatArray; Count:Int32; HiLo:Boolean): TPointArray; overload;
var 
  W,H,i,size,l: Int32;
  tmp:TFloatArray;
  data:THeapArrayF;
begin
  H := High(Mat);
  W := High(Mat[0]);
  size := 0;

  for i:=0 to H do
  begin
    L := W+1;
    SetLength(tmp, size+L);
    Move(Mat[i][0], tmp[size], L*SizeOf(Single));
    size += L;
  end;

  case HiLo of
    True:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] > Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, True);
          hPush( data, tmp[i], i, True);
        end;
    False:
      for i:=0 to High(tmp) do
        if (length(data) < count) or (tmp[i] < Data[0].value) then
        begin
          if (length(data) = count) then hPop(data, False);
          hPush( data, tmp[i], i, False);
        end;
  end;

  W += 1; 
  H += 1;
  SetLength(Result, Length(data));
  for i:=0 to High(Data) do
  begin
    Result[i].y := Data[i].index div W;
    Result[i].x := Data[i].index mod W;
  end;
end;




