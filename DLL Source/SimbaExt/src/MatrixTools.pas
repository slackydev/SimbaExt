Unit MatrixTools;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}

interface
uses
  CoreTypes, Math;

//General ----------->
function NewMatrix(W,H:Integer): T2DIntArray; 
function NewMatrixEx(W,H,Init:Integer): T2DIntArray; 
function TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray; 
function TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray;
function MatFromTIA(const TIA:TIntArray; Width,Height:Integer): T2DIntArray; 
procedure PadMatrix(var Matrix:T2DIntArray; HPad, WPad:Integer); 
function FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean): TPointArray; 
procedure DrawMatrixLine(var Mat:T2DIntArray; P1, P2: TPoint; Val:Integer); Inline;


//--------------------------------------------------
implementation

uses 
  PointTools, PointList;
  
  
{*
 Create a integer matrix of the size given W,H.
*}
function NewMatrix(W,H:Integer): T2DIntArray; 
begin
  SetLength(Result, H, W);
end;

  
{*
 Integer matrix of the size given my W,H, and initalize it with `init`.
*}
function NewMatrixEx(W,H,Init:Integer): T2DIntArray; 
var X,Y:Integer;
begin
  SetLength(Result, H, W);
  for Y:=0 to H-1 do
    for X:=0 to W-1 do
      Result[Y][X] := Init;
end;


{*
 Create a integer matrix filled with the points given by TPA, align the points to [0][0] if needed.
*}
function TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray; 
var
  Y,Width,Height,H,i:Integer;
  Area:TBox;
begin
  H := High(TPA);
  Area := TPABounds(TPA);
  Width := (Area.X2 - Area.X1) + 1;  //Width
  Height := (Area.Y2 - Area.Y1) + 1;  //Height

  case Align of
    True:
      begin
        SetLength(Result, Height, Width);
        for i:=0 to H do
          Result[TPA[i].y-Area.y1][TPA[i].x-Area.x1] := Value;
      end;
    False:
      begin
        SetLength(Result, Area.Y2+1);
        for Y:=0 to Area.Y2 do
          SetLength(Result[Y], Area.X2+1);
        for i:=0 to H do
          Result[TPA[i].y][TPA[i].x] := Value;
      end;
  end;
end;


{*
 Create integer matrix filled with the points given by TPA, align the points to [0][0] if needed.
 Initalizes it with the given initalizer.
*}
function TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray; 
var
  X,Y,Width,Height,H,i:Integer;
  Area:TBox;
begin
  H := High(TPA);
  Area := TPABounds(TPA);
  Width := (Area.X2 - Area.X1) + 1;  //Width
  Height := (Area.Y2 - Area.Y1) + 1;  //Height

  case Align of
    True:
      begin
        SetLength(Result, Height, Width);
        for Y:=0 to Height-1 do
          for X:=0 to Width-1 do
            Result[Y][X] := Init;
        for i:=0 to H do
          Result[TPA[i].y-Area.y1][TPA[i].x-Area.x1] := Value;
      end;
    False:
      begin
        SetLength(Result, Area.Y2+1, Area.X2+1);
        for Y:=0 to Area.Y2 do
          for X:=0 to Area.X2 do
            Result[Y][X] := Init;
        for i:=0 to H do
          Result[TPA[i].y][TPA[i].x] := Value;
      end;
  end;
end;


{*
  ...
*}
function MatFromTIA(const TIA:TIntArray; Width,Height:Integer): T2DIntArray; 
var y:Integer;
begin
  SetLength(Result, Height,Width);
  for y:=0 to Height-1 do
    Move(TIA[y*width], Result[y][0], Width*SizeOf(Integer));
end;


{*
  Pads the matrix with "empty" from all sides.
*}
procedure PadMatrix(var Matrix:T2DIntArray; HPad, WPad:Integer); 
var 
  y,oldw,oldh,w,h:Integer;
  Temp:T2DIntArray;
begin
  OldW := Length(Matrix[0]);
  OldH := Length(Matrix);
  H := HPad+OldH+HPad;
  W := WPad+OldW+WPad;
  SetLength(Temp, H, W);  
  for y:=0 to OldH-1 do
    Move(Matrix[y][0], Temp[y+HPad][WPad], OldW*SizeOf(Integer));  
  
  SetLength(Matrix, 0);
  Matrix := Temp;
end;


{*
 FloodFills the ImgArr, and returns the floodfilled points.
*}
function FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean): TPointArray; 
var
  color,i,x,y,W,H,fj:Integer;
  face:TPointArray;
  Queue,Res: TPointList;
begin
  W := High(ImgArr[0]);
  H := High(ImgArr);

  fj := 3;
  if EightWay then fj := 7;
  SetLength(Face, fj+1);

  Queue.Init;
  Res.Init;
  Color := ImgArr[Start.y][Start.x];
  Queue.Append(Start);
  Res.Append(Start);
  while Queue.NotEmpty do
  begin
    GetAdjacent(Face, Queue.FastPop, EightWay);
    for i:=0 to fj do
    begin
      x := face[i].x;
      y := face[i].y;
      if ((x >= 0) and (y >= 0) and (x <= W) and (y <= H)) then
      begin
        if ImgArr[y][x] = color then
        begin
          ImgArr[y][x] := -1;
          Queue.Append(face[i]);
          Res.Append(face[i]);
        end;
      end;
    end;
  end;
  Queue.Free;
  SetLength(Face, 0);
  Result := Res.Clone;
  Res.Free;
end;



{*
 Creates a line from P1 to P2. 
 Algorithm is based on Bresenham's line algorithm.
 @note, it draws the line to a 2D Integer Matrix. Used internally.
*}
procedure DrawMatrixLine(var Mat:T2DIntArray; P1, P2: TPoint; Val:Integer); Inline;
var
  dx,dy,step,I: Integer;
  rx,ry,x,y: Extended;
begin
  Mat[P1.y][P1.x] := Val;
  if (p1.x = p2.x) and (p2.y = p1.y) then
    Exit;

  dx := (P2.x - P1.x);
  dy := (P2.y - P1.y);
  if (Abs(dx) > Abs(dy)) then step := Abs(dx)
  else step := Abs(dy);

  rx := dx / step;
  ry := dy / step;
  x := P1.x;
  y := P1.y;
  for I:=1 to step do
  begin
    x := x + rx;
    y := y + ry;
    Mat[Round(y)][Round(x)] := Val;
  end;
end; 


end.





