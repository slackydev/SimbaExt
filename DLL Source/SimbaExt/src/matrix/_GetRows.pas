{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{*
  Returns a matrix containing a copy of all the values in the given rows.
*}
function GetRows(const Mat:T2DByteArray; FromRow, ToRow:Int32): T2DByteArray; overload;
var y,wid:Int32;
begin
  if Length(Mat) = 0 then NewException('Matrix must be initalized');
  Wid := Length(Mat[0]);
  ToRow := Min(ToRow, High(Mat));
  SetLength(Result, ToRow-FromRow+1);
  for y:=FromRow to ToRow do
  begin
    SetLength(Result[y-FromRow],Wid);
    Move(Mat[y,0], Result[y-FromRow,0], Wid*SizeOf(Byte));
  end;
end;

function GetRows(const Mat:T2DIntArray; FromRow, ToRow:Int32): T2DIntArray; overload;
var y,wid:Int32;
begin
  if Length(Mat) = 0 then NewException('Matrix must be initalized');
  Wid := Length(Mat[0]);
  ToRow := Min(ToRow, High(Mat));
  SetLength(Result, ToRow-FromRow+1);
  for y:=FromRow to ToRow do
  begin
    SetLength(Result[y-FromRow],Wid);
    Move(Mat[y,0], Result[y-FromRow,0], Wid*SizeOf(Int32));
  end;
end;


function GetRows(const Mat:T2DFloatArray; FromRow, ToRow:Int32): T2DFloatArray; overload;
var y,wid:Int32;
begin
  if Length(Mat) = 0 then NewException('Matrix must be initalized');
  Wid := Length(Mat[0]);
  ToRow := Min(ToRow, High(Mat));
  SetLength(Result, ToRow-FromRow+1);
  for y:=FromRow to ToRow do
  begin
    SetLength(Result[y-FromRow],Wid);
    Move(Mat[y,0], Result[y-FromRow,0], Wid*SizeOf(Single));
  end;
end;


function GetRows(const Mat:T2DDoubleArray; FromRow, ToRow:Int32): T2DDoubleArray; overload;
var y,wid:Int32;
begin
  if Length(Mat) = 0 then NewException('Matrix must be initalized');
  Wid := Length(Mat[0]);
  ToRow := Min(ToRow, High(Mat));
  SetLength(Result, ToRow-FromRow+1);
  for y:=FromRow to ToRow do
  begin
    SetLength(Result[y-FromRow],Wid);
    Move(Mat[y,0], Result[y-FromRow,0], Wid*SizeOf(Double));
  end;
end;

function GetRows(const Mat:T2DExtArray; FromRow, ToRow:Int32): T2DExtArray; overload;
var y,wid:Int32;
begin
  if Length(Mat) = 0 then NewException('Matrix must be initalized');
  Wid := Length(Mat[0]);
  ToRow := Min(ToRow, High(Mat));
  SetLength(Result, ToRow-FromRow+1);
  for y:=FromRow to ToRow do
  begin
    SetLength(Result[y-FromRow],Wid);
    Move(Mat[y,0], Result[y-FromRow,0], Wid*SizeOf(Extended));
  end;
end;
