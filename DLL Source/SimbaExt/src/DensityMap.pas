Unit DensityMap;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface
uses
  CoreTypes, Math, SysUtils;


procedure __AreaSum(Matrix:T2DExtArray; W, H, Radius:Integer); Inline;
function __DensityMap(const TPA:TPointArray; Radius, Passes:Integer; Area:TBox): T2DExtArray;
function DensityMap(const TPA:TPointArray; Radius, Passes:Integer): T2DExtArray; 
function DensityMapNormed(const TPA:TPointArray; Radius, Passes, Beta:Integer): T2DIntArray; 
procedure TPADensitySort(var Arr: TPointArray; Radius, Passes:Integer); 


//--------------------------------------------------
implementation

uses
  PointTools;


procedure __AreaSum(Matrix:T2DExtArray; W, H, Radius:Integer); Inline;
var
  table: T2DExtArray;
  y0,x0,y1,x1,x,y,RW,RH: Integer;
begin
  SetLength(Table, H+1, W+1);
  RW := W-1;
  RH := H-1;
    
  for y:=0 to RH do
    for x:=0 to RW do
      Table[y+1][x+1] := Table[y+1][x] + Table[y][x+1] - Table[y][x] + Matrix[y][x];

  for y:=0 to RH do
  begin
    y0 := Max(0, y - radius);
    y1 := Min(h, y + radius + 1);
    for x:=0 to RW do
    begin
      x0 := Max(0, x - radius);
      x1 := Min(w, x + radius + 1);
      Matrix[y][x] := Table[y0][x0] + Table[y1][x1] - Table[y1][x0] - Table[y0][x1];
    end;
  end;
  SetLength(Table, 0);
end;


function __DensityMap(const TPA:TPointArray; Radius, Passes:Integer; Area:TBox): T2DExtArray;
var
  W,H,i:Integer;
begin
  if Length(TPA) <= 0 then Exit;
  Radius := Min(Radius, 500);
  Passes := Min(Passes, 10);
  W := (Area.x2 - Area.x1) + 1;
  H := (Area.y2 - Area.y1) + 1;

  SetLength(Result, H, W);
  for i:=0 to High(TPA) do
  begin
    Result[(TPA[i].y-Area.y1)][(TPA[i].x-Area.x1)] := 1;
  end;

  for i:=1 to Passes do
    __AreaSum(Result, W, H, Radius);
end;


function DensityMap(const TPA:TPointArray; Radius, Passes:Integer): T2DExtArray; 
begin
  if Length(TPA) <= 0 then Exit;
  Result := __DensityMap(TPA, Radius, Passes, TPABounds(TPA));
end;


function DensityMapNormed(const TPA:TPointArray; Radius, Passes, Beta:Integer): T2DIntArray; 
var
  x,y,H,W: Integer;
  k,mx: Extended;
  Mat: T2DExtArray;
begin
  if Length(TPA) <= 0 then Exit;
  Mat := __DensityMap(TPA, Radius, Passes, TPABounds(TPA));
  W := High(Mat[0]);
  H := High(Mat);
  mx := 0;
  for y:=0 to H do
    for x:=0 to W do
      if (Mat[y][x] > mx) then
        mx := Mat[y][x];
  k := 0.0;
  if (mx > 0) then
    k := (Beta / mx);
  
  SetLength(Result, H+1,W+1);
  for y:=0 to H do
    for x:=0 to W do
      Result[y][x] := Round(Mat[y][x]*k);
  SetLength(Mat, 0);
end;


procedure TPADensitySort(var Arr: TPointArray; Radius, Passes:Integer); 
var
  gap,Cur,Tmp,Len: Integer;
  Matrix:T2DExtArray;
  Area:TBox;
  tmppt,pt1,pt2:TPoint;
begin
  len := Length(Arr);
  if len <= 0 then Exit;
  Area := TPABounds(Arr);
  Matrix := __DensityMap(Arr, Radius, Passes, Area);
  
  gap := 0;
  while gap < (len div 3) do
    gap := (gap * 3) + 1;
  
  while (gap >= 1) do
  begin
    for cur := gap to (len - 1) do
    begin
      tmp := cur;
      while (tmp >= gap) do
      begin
        pt1.x := (Arr[tmp].x - Area.x1);  pt2.x := (Arr[tmp-gap].x - Area.x1);
        pt1.y := (Arr[tmp].y - Area.y1);  pt2.y := (Arr[tmp-gap].y - Area.y1);
        if (Matrix[pt1.y][pt1.x] <= Matrix[pt2.y][pt2.x]) then
          Break;
        tmppt := arr[tmp];
        arr[tmp] := arr[(tmp - gap)];
        arr[(tmp - gap)] := tmppt;
        tmp := (tmp - gap);
      end;
    end;
    gap := (gap div 3);
  end;     
  SetLength(Matrix, 0);
end;

end.