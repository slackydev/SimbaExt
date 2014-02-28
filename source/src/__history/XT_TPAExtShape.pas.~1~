Unit XT_TPAExtShape;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface
uses
  XT_Types, Math, SysUtils;

  
function __SplitBoxesGetBounds(const TPA:TPointArray; BoxRad:Extended): TPointArray;
function TPAExtractShape(const PTS:TPointArray; Distance, EstimateRad:Integer): TPointArray; StdCall;

//--------------------------------------------------
implementation

uses 
  XT_Points, XT_Matrix;
  
{*
 Used to create an estimate of points which we will draw lines trough.
 In general it splits the TPA in to boxes of `BoxRad` size. Then it returns
 each middle point of each box as a TPA.
*}
function __SplitBoxesGetBounds(const TPA:TPointArray; BoxRad:Extended): TPointArray;
var
  i,x,y,id,l,cols,rows:Integer;
  tmp: T2DPointArray;
  Area,B:TBox;
begin
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 1;
  Area.Y2 := (Area.Y2 - Area.Y1) + 1;
  cols := (Ceil(Area.X2 / BoxRad));
  rows := (Ceil(Area.Y2 / BoxRad));

  SetLength(tmp, (cols+1)*(rows+1));
  for i:=0 to High(TPA) do
  begin
    x := Floor((TPA[i].x-Area.x1) / BoxRad);
    y := Floor((TPA[i].y-Area.y1) / BoxRad);
    id := (y*cols)+x;
    L := Length(tmp[id]);
    SetLength(tmp[id], L+1);
    tmp[id][L] :=  TPA[i];
  end;
  l := 0;
  SetLength(Result, (cols+1)*(rows+1));
  for x:=0 to cols-1 do
    for y:=0 to rows-1 do
    begin
      if Length(tmp[(y * cols + x)]) >= 1 then
      begin
        B := TPABounds(tmp[(y * cols + x)]);
        l := l + 4;
        SetLength(Result, l);
        Result[l-1] := Point(B.x1,B.y1);
        Result[l-2] := Point(B.x2,B.y2);
        Result[l-3] := Point(B.x1,B.y2);
        Result[l-4] := Point(B.x2,B.y1);
      end;
    end;
end;


{*
 This function creates a TPA representing the shape of the given TPA.
 There are two important paramters: 
 >> Distanse (The max distance between points in the TPA, no lines will be created if it's longer distance)
 >> EstimateRad, the size of the box for each estimate point.
 
 The shape can be used together with (WN)InPoly yo check if a point is inside the TPA area.
*}
function TPAExtractShape(const PTS:TPointArray; Distance, EstimateRad:Integer): TPointArray; StdCall;
var
  i,j,h,x,y,lx,hx,ly,hy,l,hit,QSize:Integer;
  TPA: TPointArray;
  Matrix: T2DIntArray;
  Area: TBox;
  pt,start,prev,endpt:TPoint;
  adj:TPointArray;
begin
  if (EstimateRad > (Distance div 2)) and (Distance >= 2) then
    EstimateRad := (Distance div 2);
  if (EstimateRad > 4) then begin
    TPA := __SplitBoxesGetBounds(PTS, EstimateRad);
  end else
    TPA := PTS;
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 1;  //Width
  Area.Y2 := (Area.Y2 - Area.Y1) + 1;  //Height
  H := High(TPA);
  SetLength(adj, 8);
  
  Matrix := IntMatrixNil(Area.X2+1,Area.Y2+1);

  start := Point(Area.X2, Area.Y2);
  for i:=0 to H do
  begin
    x := (TPA[i].x-Area.X1);
    y := (TPA[i].y-Area.Y1);
    Matrix[y][x] := 1;
    if y < Start.y then
      start := Point(x,y);
  end;

  //Draw possible lines between points that are within a given distance from each other.
  for i:=0 to H do
  begin
    pt.x := TPA[i].x-Area.X1;
    pt.y := TPA[i].y-Area.Y1;
    if Matrix[pt.y][pt.x] = 1 then
    begin
      Matrix[pt.y][pt.x] := 0;
      lx := Max(pt.x - Distance, 0);
      hx := Min(pt.x + Distance, Area.X2-1);
      ly := Max(pt.y - Distance, 0);
      hy := Min(pt.y + Distance, Area.Y2-1);
      for x:=lx to hx do
        for y:=ly to hy do
          if (Matrix[y][x] = 1) then
          begin
            DrawMatrixLine(Matrix, pt, Point(x,y), 5);
            Matrix[y][x] := 1;
          end;
    end;
  end;

  //Roooooollin': http://www.youtube.com/watch?v=qCRae5mRoRE
  //... around the edge points.
  l := (Area.x2*Area.y2);
  endpt := start;
  prev := Point(start.x, start.y-1);
  QSize := 1;
  SetLength(Result, QSize);
  hit := 0;
  H := 0;
  for i:=0 to L do 
  begin
    if ((endpt.x = prev.x) and (endpt.y = prev.y) and (i>1)) then begin
      if hit = 1 then Break;
      Inc(hit);
    end;
    RotatingAdjecent(adj, start, prev);
    for j:=0 to 7 do begin
      x := adj[j].x;
      y := adj[j].y;
      if (x > -1) and (x < Area.X2) and
         (y > -1) and (y < Area.Y2) then
        if Matrix[y][x] >= 1 then
        begin
          prev := start;
          start := adj[j];
          if Matrix[y][x] < 300 then
          begin
            Inc(H);
            if (QSize <= H) then begin
              QSize := QSize+QSize;
              SetLength(Result, QSize);
            end;
            //Instead of the line we can return the points with the usage of 2 matrices...
            Result[H-1] := Point((Start.x+Area.x1), (Start.y+Area.y1));
            Matrix[y][x] := 300;
          end;
          break;
        end;
    end;
  end;
  SetLength(Result, H);
  SetLength(Matrix, 0);
  SetLength(Adj, 0);
end;


end.
