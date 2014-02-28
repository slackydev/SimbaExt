Unit XT_Points;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface

uses
  XT_Types, XT_Math, Math;
  
function SamePoints(P1, P2:TPoint):Boolean; Inline;
procedure GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean); Inline; StdCall;
procedure RotatingAdjecent(var Adj:TPointArray;const Curr:TPoint; const Prev:TPoint); Inline;
function ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint; Inline; StdCall;
function SumTPA(Arr: TPointArray): TPoint; Inline; StdCall;
procedure TPASplitAxis(const TPA: TPointArray; var X:TIntArray; var Y:TIntArray); StdCall;
procedure TPAJoinAxis(const X:TIntArray; const Y:TIntArray; var TPA:TPointArray); StdCall;
function TPAMax(const TPA: TPointArray): TPoint;
function TPABounds(const TPA: TPointArray): TBox; Inline;
function TPACenter(const TPA: TPointArray; Method: TCenterMethod; Inside:Boolean): TPoint; StdCall;
function TPAExtremes(const TPA:TPointArray): TPointArray; StdCall; 
function TPABBox(const TPA:TPointArray): TPointArray; StdCall;
procedure TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer); StdCall;
procedure TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const From:TPoint); StdCall;
procedure ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean); StdCall;
procedure ReverseTPA(var TPA: TPointArray); StdCall;
procedure MoveTPA(var TPA: TPointArray; SX,SY:Integer); StdCall;
procedure TPARemoveDupes(var TPA: TPointArray); StdCall;
procedure LongestPolyVector(const Poly:TPointArray; var A,B:TPoint); StdCall;
function InvertTPA(const TPA:TPointArray): TPointArray; StdCall;
function RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended): TPointArray; StdCall;
function TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray; StdCall;
function AlignTPA(const TPA:TPointArray; Method: TAlignMethod; var Angle:Extended): TPointArray; StdCall;
function CleanSortTPA(const TPA: TPointArray): TPointArray; StdCall;
function UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray; StdCall;
procedure TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint); Inline; StdCall;
function ConnectTPA(const TPA:TPointArray): TPointArray; Inline; StdCall;
function ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray; Inline; StdCall;
function XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; Inline; StdCall;
procedure TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer); StdCall;
procedure TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer); StdCall;
procedure TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint); StdCall;
function ConvexHull(const TPA:TPointArray): TPointArray; StdCall;
function FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean): TPointArray; StdCall;
function FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray; StdCall;
function TPAOutline(const TPA:TPointArray): TPointArray; StdCall;
function TPABorder(const TPA:TPointArray): TPointArray; StdCall;
function FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray; StdCall;
function ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray; StdCall;
function ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray; StdCall;
function TPAEdges(const TPA: TPointArray): TPointArray; StdCall;


//--------------------------------------------------
implementation

uses 
  XT_CSpline, XT_Matrix, XT_Sorting, XT_TPointList;

{*
 Compares two TPoints, to se if they are the same or not.
*}
function SamePoints(P1, P2:TPoint):Boolean; Inline;
begin
  Result := ((P1.x = P2.x) and (P1.y = P2.y));
end;

{*
 Return the neighbours of the given TPoint defined by `n`.
*}
procedure GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean); Inline; StdCall;
begin
  adj[0] := Point(n.x-1,n.y);
  adj[1] := Point(n.x,n.y-1);
  adj[2] := Point(n.x+1,n.y);
  adj[3] := Point(n.x,n.y+1);
  if EightWay then 
  begin
    adj[4] := Point(n.x-1,n.y-1);
    adj[5] := Point(n.x+1,n.y+1);
    adj[6] := Point(n.x-1,n.y+1);
    adj[7] := Point(n.x+1,n.y-1);
  end;
end;


{*
 Walk around current, from previous. It's 8way.
*}    
procedure RotatingAdjecent(var Adj:TPointArray;const Curr:TPoint; const Prev:TPoint); Inline;
var
  i: Integer;
  dx,dy,x,y:Single;
begin
  x := Prev.x; y := Prev.y;
  adj[7] := Prev;
  for i:=0 to 6 do
  begin
    dx := x - Curr.x;
    dy := y - Curr.y;
    x := ((dy * 0.7070) + (dx * 0.7070)) + Curr.x;
    y := ((dy * 0.7070) - (dx * 0.7070)) + Curr.y;
    adj[i] := Point(Round(x),Round(y));
  end;
end;


{*
 Scales the point, while keeping the angle from the center.
*}
function ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint; Inline; StdCall;
var
  dx,dy: Integer;
  Angle: Single;
begin
  dx := (Center.X - Pt.X);
  dy := (Center.Y - Pt.Y);
  Angle := ArcTan2(dy, dx) + PI;
  Result.x := Round(Radius * Cos(Angle)) + Center.X;
  Result.y := Round(Radius * Sin(Angle)) + Center.Y;
end;


{*
 Calculates the total sum of the TPA.
*}
function SumTPA(Arr: TPointArray): TPoint; Inline; StdCall;
var i:Integer;
begin
  Result := Point(0,0);
  for i:=Low(Arr) to High(Arr) do
  begin
    Result.x := Result.x + Arr[i].x;
    Result.y := Result.y + Arr[i].y;
  end;
end;


{*
  Splits the TPA in to two TIAs: X- and Y-Axis.
*}
procedure TPASplitAxis(const TPA: TPointArray; var X:TIntArray; var Y:TIntArray); StdCall;
var i,H:Integer;
begin
  H := High(TPA);
  SetLength(X, H+1);
  SetLength(Y, H+1);
  for i:=0 to H do
  begin
    X[i] := TPA[i].x;
    Y[i] := TPA[i].y;
  end;
end;


{*
  Given two TIAs this function will join them in to one TPA.
*}
procedure TPAJoinAxis(const X:TIntArray; const Y:TIntArray; var TPA:TPointArray); StdCall;
var i,H:Integer;
begin
  H := Min(High(X), High(Y));
  SetLength(TPA, H+1);
  for i:=0 to H do
    TPA[i] := Point(X[i], Y[i]);
end;


{*
 Return the largest numbers for x, and y-axis in TPA.
*}
function TPAMax(const TPA: TPointArray): TPoint;
var
  I,L : Integer;
begin;
  L := High(TPA);
  if (l < 0) then Exit;
  Result.x := TPA[0].x;
  Result.y := TPA[0].y;
  for I:=0 to L do
  begin
    if TPA[i].x > Result.x then
      Result.x := TPA[i].x;
    if TPA[i].y > Result.y then
      Result.y := TPA[i].y;
  end;
end;


{*
 Return the largest and the smallest numbers for x, and y-axis in TPA.
*}
function TPABounds(const TPA: TPointArray): TBox; Inline;
var
  I,L : Integer;
begin
  FillChar(Result, SizeOf(TBox), 0);
  L := High(TPA);
  if (l < 0) then Exit;
  Result.x1 := TPA[0].x;
  Result.y1 := TPA[0].y;
  Result.x2 := TPA[0].x;
  Result.y2 := TPA[0].y;
  for I:= 1 to L do
  begin
    if TPA[i].x > Result.x2 then
      Result.x2 := TPA[i].x
    else if TPA[i].x < Result.x1 then
      Result.x1 := TPA[i].x;
    if TPA[i].y > Result.y2 then
      Result.y2 := TPA[i].y
    else if TPA[i].y < Result.y1 then
      Result.y1 := TPA[i].y;
  end;
end;


{*
 Returns the most outer points in the TPA, requres a tpa of atleast 4 points.
 Similar to TPABounds, except it returns the actuall points.
*}
function TPAExtremes(const TPA:TPointArray): TPointArray; StdCall;
var
  I,L : Integer;
begin
  L := High(TPA);
  if (l < 3) then Exit; 
  SetLength(Result, 4);
  Result[0] := TPA[0];
  Result[1] := TPA[0];
  Result[2] := TPA[0];
  Result[3] := TPA[0];
  for I:= 1 to L do
  begin
    if TPA[i].x > Result[0].x then
      Result[0] := TPA[i] 
    else if TPA[i].x < Result[2].x then
      Result[2] := TPA[i]; 
    if TPA[i].y > Result[1].y then
      Result[1] := TPA[i]
    else if TPA[i].y < Result[3].y then
      Result[3] := TPA[i]; 
  end;
end;



{*
 Mean, Median, and two diffrent ways to define "centers"..
 @params
 | Method: CM_Mean | CM_Median | CM_Bounds | CM_BBox.
 | Inside: True | False .. If True then the result is always inside the shape.
*}
function TPACenter(const TPA: TPointArray; Method: TCenterMethod; Inside:Boolean): TPoint; StdCall;
var
  Len, Mid: Integer;
  TMP:TPointArray;
  X,Y:TIntArray;
  Area: Tbox;
begin
  Len := Length(TPA);
  if (Len <= 0) then Exit(Point(0, 0));
  if (Len = 1) then Exit(TPA[0]);
  case Method of
  CM_Bounds:
    begin
      Area := TPABounds(TPA);
      Result.X := Round((Area.X2 + Area.X1) / 2);
      Result.Y := Round((Area.Y2 + Area.Y1) / 2);
    end;
  CM_BBox:
    begin
      TMP := TPABBox(TPA);
      Result.x := Round((TMP[0].x + TMP[2].x) / 2);
      Result.y := Round((TMP[1].y + TMP[3].y) / 2);
      SetLength(TMP, 0);
    end;
  CM_Mean:
    begin
      Result := SumTPA(TPA);
      Result.x := Round(Result.X / Len);
      Result.y := Round(Result.Y / Len);
    end;
  CM_Median:
    begin
      TPASplitAxis(TPA, X,Y);
      SortTIA(X);
      SortTIA(Y);
      Mid := Len div 2;
      if (Len mod 2) = 0 then begin
        Result := Point(X[Mid], Y[Mid]);
      end else begin
        Result.x := Round((X[Mid] + X[Mid+1]) / 2);
        Result.y := Round((Y[Mid] + Y[Mid+1]) / 2);
      end;
    end;
  end;
  if Inside then
  begin
    TMP := Copy(TPA);
    SortTPAFrom(TMP, Result);
    Result := TMP[0];
    SetLength(TMP, 0);
  end;
end;



{*
 Returns the minimum bounding rectangle around the given TPA. 
 The function is a little clumsy written, but it does the trick.
*}
function TPABBox(const TPA:TPointArray): TPointArray; StdCall;
var
  L,i,j,v,c,edge_x,edge_y,w,h:Integer; 
  halfpi,X,Y,cosA,cosAP,CosAM: Extended;
  xl,yl,xh,yh,Area,Angle:Extended; 
  Shape:TPointArray;
  Angles,BBox:TExtArray;
  added:Boolean;
  pt:TPoint;
begin
  SetLength(Result, 4); 
  if Length(TPA) <= 1 then Exit;

  Shape := ConvexHull(TPA);
  L := High(Shape);
  halfpi := PI / 2;
  SetLength(angles, L);
  
  j := 0;    
  for i:=0 to (L-1) do
  begin
    Angles[j] := PI; //Init with number greater then halfpi
    Added := False;
    edge_x := Shape[i+1].x - Shape[i].x;
    edge_y := Shape[i+1].y - Shape[i].y; 
    Angle := Abs(Modulo(ArcTan2(edge_y, edge_x), halfpi));
    for c:=0 to j do
      if (Angles[c] = Angle) then Added := True;
    if not(Added) then begin 
      Angles[j] := Angle;
      Inc(j);
    end;     
  end;

  SetLength(angles, j); 
  SetLength(BBox, 6);
  BBox[1] := MaxInt;
  for i:=0 to j-1 do
  begin  
    CosA := Cos(Angles[i]);
    CosAP := Cos(Angles[i]+halfpi);
    CosAM := Cos(Angles[i]-halfpi);
    xl := (CosA*Shape[0].x) + (CosAM*Shape[0].y);
    yl := (CosAP*Shape[0].x) + (CosA*Shape[0].y);
    xh := xl;
    yh := yl;
    for v:=0 to L do
    begin
      pt := Shape[v];
      x  := (CosA*pt.x) + (CosAM*pt.y);
      y  := (CosAP*pt.x) + (CosA*pt.y);
      if (x > xh) then xh := x
      else if (x < xl) then xl := x;
      if (y > yh) then yh := y
      else if (y < yl) then yl := y;
    end;
    Area := (xh-xl)*(yh-yl);
    if (Area < bbox[1]) then begin
      BBox[0] := Angles[i];
      BBox[1] := Area;
      BBox[2] := xl;
      BBox[3] := xh;
      BBox[4] := yl;
      BBox[5] := yh;
    end;
  end;

  Angle := bbox[0];   
  cosA  := Cos(Angle);
  cosAP := Cos(Angle+halfpi);
  cosAM := Cos(Angle-halfpi);
  xl := bbox[2];
  xh := bbox[3];
  yl := bbox[4];
  yh := bbox[5];
  Result[0] := Point(Round((cosAP*yl) + (cosA*xh)), Round((cosA*yl) + (cosAM*xh)));
  Result[1] := Point(Round((cosAP*yl) + (cosA*xl)), Round((cosA*yl) + (cosAM*xl)));
  Result[2] := Point(Round((cosAP*yh) + (cosA*xl)), Round((cosA*yh) + (cosAM*xl)));
  Result[3] := Point(Round((cosAP*yh) + (cosA*xh)), Round((cosA*yh) + (cosAM*xh)));

  SetLength(angles, 0); 
  SetLength(BBox, 0);
  SetLength(Shape, 0);
end;


{*
  Removes the points from the TPA that is NOT in the given shape.
  `From` is used if the shape is not at the same position as the TPA, it will
  add or subsract the values (x,y) to/from the points in TPA.
*}
procedure TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const From:TPoint); StdCall;
var 
  i,j,size,W,H,x,y:Integer;
  Matrix:T2DBoolArray;
  Area:TBox;
begin
  Size := High(TPA);
  if Size<0 then Exit;
  Area := TPABounds(TPA);
  W := (Area.x2 - Area.x1) + 1;
  H := (Area.y2 - Area.y1) + 1;
  SetLength(Matrix, H,W);
  for i:=0 to Size do
    Matrix[TPA[i].y - Area.y1][TPA[i].x - Area.x1] := True;

  j := 0;
  for i:=0 to High(Shape) do
  begin
    x := (Shape[i].x - Area.X1) + From.x;
    y := (Shape[i].y - Area.Y1) + From.y;
    if (x>=0) and (x<W) and (y>=0) and (y<H) then
      if Matrix[y][x] then
      begin
        TPA[j] := Shape[i];
        Inc(j);
      end;
  end;
  SetLength(TPA, j);
  SetLength(Matrix, 0);
end;


{*
  Removes the points outside the bound.
*}
procedure TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer); StdCall;
var i,j,H:Integer;
begin
  H := High(TPA);
  j := 0;
  for i:=0 to H do
    if (TPA[i].x>=x1) and (TPA[i].x<=x2) and
       (TPA[i].y>=y1) and (TPA[i].y<=y2) then
    begin
      TPA[j] := TPA[i];
      Inc(j);
    end;
  SetLength(TPA, j);
end;


{*
  Filter the ATPA by it's dimensions and Length.
  if Align is set, then the W will always be the longest side: It will be rotation insensitive!
  ^ Uses AlignTPA.
*}
procedure ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean); StdCall;
var 
  i,j,H:Integer;
  b:TBox;
  a:Extended;
  TPA:TPointArray;
begin
  H := High(ATPA);
  j := 0;
  for i:=0 to H do
    if Length(ATPA[i]) >= MinLength then
    begin
      if Align then TPA := AlignTPA(ATPA[i], AM_BBox, a)
      else          TPA := ATPA[i];
      B := TPABounds(TPA);
      if (B.Width >= MinW) and (B.Height >= MinH) and 
         (B.Width <= MaxW) and (B.Height <= MaxH) then
      begin
        ATPA[j] := ATPA[i];
        Inc(j);
      end;
    end;
  SetLength(ATPA, j);
end;




{*
 Reverses the TPointArray / flips it (Self note: list[::-1]).
*}
procedure ReverseTPA(var TPA: TPointArray); StdCall; 
var 
  i, Hi, Mid: Integer;
  tmp:TPoint;
begin
  Hi := High(TPA);
  if (Hi < 0) then Exit;
  Mid := Hi div 2;
  for i := 0 to Mid do begin
    tmp := TPA[Hi-i];
    TPA[Hi-i] := TPA[i];
    TPA[i] := tmp;
  end;
end;


{*
 Moves the TPA by SX, and SY points.
*}
procedure MoveTPA(var TPA: TPointArray; SX,SY:Integer); StdCall;
var
  I,L : Integer;
begin;
  L := High(TPA);
  if (L < 0) then Exit;
  for I:=0 to L do begin
    TPA[i].x := TPA[i].x + SX;
    TPA[i].y := TPA[i].y + SY;
  end;
end;


{*
 Removing all duplicates in the TPA.
*}
procedure TPARemoveDupes(var TPA: TPointArray); StdCall;
var
  i, j, H: Integer;
  Matrix: T2DBoolArray;
  b: TBox;
begin;
  H := High(TPA);
  if (H <= 0) then Exit;
  b := TPABounds(TPA);
  Matrix := BoolMatrixNil((b.X2 - b.X1) + 1, (b.Y2 - b.Y1) + 1);
  j := 0;
  for i:=0 to H do
    if Matrix[(TPA[i].Y - b.Y1)][(TPA[i].X - b.X1)] <> True then
    begin
      Matrix[(TPA[i].Y - b.Y1)][(TPA[i].X - b.X1)] := True;
      TPA[j] := TPA[i];
      Inc(j);
    end;
  SetLength(TPA, j);
  SetLength(Matrix, 0);
end;


{*
 Given a Polygon defined by atleast two points, this function will find the longest side.
*}
procedure LongestPolyVector(const Poly:TPointArray; var A,B:TPoint); StdCall;
var
  I,j,L: Integer;
  Dist,tmp: Single;
begin
  L := Length(Poly);
  if (l <= 2) then Exit;
  A := Poly[0];
  B := Poly[1];
  Dist := Sqr(A.x - B.x) + Sqr(A.y - B.y);
  for I:= 0 to (L-1) do
  begin    
    j := (i+1) mod L;
    tmp := Sqr(Poly[j].x - Poly[i].x) + Sqr(Poly[j].y - Poly[i].y);
    if Tmp > Dist then
    begin
      A := Poly[i];
      B := Poly[j];
      Dist := tmp;
    end;
  end;
end;


{*
 Returns the points not in the TPA within the bounds of the TPA.
*}
function InvertTPA(const TPA:TPointArray): TPointArray; StdCall;
var
  Matrix: T2DBoolArray;
  i,h,x,y: Integer;
  Area: TBox;
begin
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2-Area.X1);
  Area.Y2 := (Area.Y2-Area.Y1);
  Matrix := BoolMatrixNil(Area.X2+1, Area.Y2+1);
  
  H := High(TPA);
  for i:=0 to H do
    Matrix[TPA[i].y-Area.y1][TPA[i].x-Area.x1] := True;

  SetLength(Result, (Area.X2+1)*(Area.Y2+1));
  i := 0;
  for y:=0 to Area.Y2 do
    for x:=0 to Area.X2 do
      if Matrix[y][x] <> True then
      begin
        Result[i] := Point(x+Area.x1,y+Area.y1);
        Inc(i);
      end;
  SetLength(Result, i);
  SetLength(Matrix, 0);
end;


{*
 Unlike RotateTPA found in SCAR-Divi this function tries to keep the TPA filled even after rotation.
 The function is simply adding the surrounding pixels for each point to the result.
 then it will filter out duplicates. The result may then be 1px larger then original in any direction.
 
 //Future: Should look in to rotating the TPA first, then filling all small holes. Will be better.
*}
function RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended): TPointArray;StdCall;
var
   I, L,cx,cy,h: Integer;
   CosA,SinA,x,y: extended;
begin
  L := High(TPA);
  cx := Center.x;
  cy := Center.y;
  SetLength(Result, (L+1)*3);
  CosA := Cos(Radians);
  SinA := Sin(Radians);
  H := 0;
  for I := 0 to L do
  begin
    H := H+3;
    X := (CosA * (TPA[i].x - cX)) - (SinA * (TPA[i].y - cY)) + cX;
    Y := (SinA * (TPA[i].x - cX)) + (CosA * (TPA[i].y - cY)) + cY;
    Result[h-3] := Point(Trunc(x), Trunc(y));
    Result[h-2] := Point(Trunc(x)-1, Trunc(y));
    Result[h-1] := Point(Round(x), Ceil(y)-1);
  end;
  TPARemoveDupes(Result);
end;


{*
 Partitions a TPA, by splitting it in to boxes of `BoxWidth` and `BoxHeight`
 The result is the ATPA containing all the area TPAs.
*}
function TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray; StdCall;
var
  i,x,y,id,l,cols,rows,h:Integer;
  Area:TBox;
begin
  H := High(TPA);
  if (H < 0) then Exit;
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 1;  //Width
  Area.Y2 := (Area.Y2 - Area.Y1) + 1;  //Height
  Cols := Ceil(Area.X2 / BoxWidth);
  Rows := Ceil(Area.Y2 / BoxHeight);
  SetLength(Result, (Cols+1)*(Rows+1));
  for i:=0 to H do
  begin
    X := (TPA[i].x-Area.x1) div BoxWidth;
    Y := (TPA[i].y-Area.y1) div BoxHeight;
    ID := (Y*Cols)+X;
    L := Length(Result[ID]);
    SetLength(Result[ID], L+1);
    Result[ID][L] := TPA[i];
  end;
end; 


{*
 This function should align the TPA by the longest side to the X-Axis.
*}
function AlignTPA(const TPA:TPointArray; Method: TAlignMethod; var Angle:Extended): TPointArray; StdCall;
var 
  Shape:TPointArray;
  A,B:TPoint;
begin
  case Method of
    AM_Extremes:Shape := TPAExtremes(TPA);
    AM_Convex:  Shape := ConvexHull(TPA);
    AM_BBox:    Shape := TPABBox(TPA);
  end;
  LongestPolyVector(Shape, A,B);
  Angle := ArcTan2(-(B.y-A.y),(B.x-A.x));
  Result := RotateTPAEx(TPA, TPACenter(TPA, CM_Bounds, False), Angle);
  SetLength(Shape, 0);
  Angle := Modulo(Degrees(Angle), 360);  //Always in range of 0 and 359 dgr!
end;


{*
 Removes duplicates, and sorts the TPA by Column.
 Uses a Matrix, so it limited, but should be fast for High density TPAs.
*}
function CleanSortTPA(const TPA: TPointArray): TPointArray; StdCall;
var
  Matrix: T2DBoolArray;
  i, C, H, idx, x, y: Integer;
  Area: TBox;
begin
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2-Area.X1);
  Area.Y2 := (Area.Y2-Area.Y1);
  H := High(TPA);
  Matrix := BoolMatrixNil(Area.X2+1, Area.Y2+1);

  C := 0;
  for I:=0 to H do
  begin
    if Matrix[(TPA[i].y - Area.Y1)][(TPA[i].x - Area.X1)] = True then
      Continue;
    Matrix[(TPA[i].y - Area.Y1)][(TPA[i].x - Area.X1)] := True;
    Inc(C);
  end;

  SetLength(Result, C);
  idx := 0;
  for y := 0 to Area.Y2 do
    for x := 0 to Area.X2 do
      if Matrix[y][x] = True then
      begin
        Result[idx] := Point((X+Area.X1), (Y+Area.Y1));
        Inc(idx);
        if (idx >= C) then
          Exit;
      end;
  SetLength(Matrix, 0);
end;


{*
 Unite two TPAs into one
 ... While also removing all duplicates if `RemoveDupes` is set, so it wont be any overlapping.
*}
function UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray; StdCall;
var
  Matrix: T2DBoolArray;
  i, j: Integer;
  Area: TBox;
begin
  SetLength(Result, High(TPA1) + High(TPA2) + 2);
  Move(TPA1[Low(TPA1)], Result[Low(Result)], Length(TPA1)*SizeOf(TPA1[0]));
  Move(TPA2[Low(TPA2)], Result[High(TPA1)+1], Length(TPA2)*SizeOf(TPA2[0]));

  if RemoveDupes then
  begin
    Area := TPABounds(Result);
    Matrix := BoolMatrixNil((Area.X2-Area.X1)+1, (Area.Y2-Area.Y1)+1);
    j := 0;
    for I:=Low(Result) to High(Result) do
    begin
      if Matrix[(Result[i].y - Area.Y1)][(Result[i].x - Area.X1)] = True then
        Continue;
      Matrix[(Result[i].y - Area.Y1)][(Result[i].x - Area.X1)] := True;
      Result[j] := Result[i];
      Inc(j);
    end;
    SetLength(Result, j);
  end;
  SetLength(Matrix, 0);
end; 


{*
 Quickly creates a line from P1 to P2. 
 Algorithm is based on Bresenham's line algorithm.
 
 @note: it extends `var TPA` with the line.
*}
procedure TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint); Inline; StdCall;
var 
  dx,dy,step,I,H: Integer;
  rx,ry,x,y: Extended;
begin
  H := Length(TPA);
  if SamePoints(p1, p2) then
  begin
    SetLength(TPA, H+1);
    TPA[H] := P1; 
    Exit;
  end;
  
  dx := (P2.x - P1.x);
  dy := (P2.y - P1.y);
  if (Abs(dx) > Abs(dy)) then step := Abs(dx)
  else step := Abs(dy);
  SetLength(TPA, (H+step+1));
  
  rx := dx / step; 
  ry := dy / step;
  x := P1.x;
  y := P1.y;
  
  TPA[H] := Point(P1.x, P1.y); 
  for I:=1 to step do
  begin
    x := x + rx;
    y := y + ry;
    TPA[(H+i)] := Point(Round(x),Round(y));
  end;
end;


{*
 Quickly creates a line from from each point in the TPA, to the next point.
 Uses TPALine, found above.
*}
function ConnectTPA(const TPA:TPointArray): TPointArray; Inline; StdCall;
var
  i,j,h: Integer;
  f,t:TPoint;
begin
  H := High(TPA);
  for i:=0 to H do
  begin
    j := i+1;
    if i=h then
      j:=0;
    f := TPA[i];
    t := TPA[j]; 
    if (SamePoints(f, t) = False) then
      TPALine(Result, f, t)
    else
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := f;
    end;
  end;
end;


{*
 ConnectTPAEx is the same as ConnectTPA (above function) except that it uses
 Spline so the result is more "curvy", and it's not as fast..
*}
function ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray; Inline; StdCall;
var
  FPts: TFPointArray;
  TMP: TPointArray;
  i,j,h: Integer;
  f,t:TPoint;
begin
  TPARemoveDupes(TPA);
  FPts := CSplineTFPA(TPAToTFPA(TPA), Tension);
  TMP := TFPAToTPA(FPts);
  H := High(TMP);
  for i:=0 to H do
  begin
    j := i+1;
    if i=h then
      j:=0;
    f := TMP[i];
    t := TMP[j]; 
    if (SamePoints(f, t) = False) then
      TPALine(Result, f, t)
    else
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := f;
    end;
  end;
  SetLength(TMP, 0);
  SetLength(FPts, 0); 
end;


{*
 Creates all the points needed to define a simple polygon.
*}
function XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; Inline; StdCall;
var
  i,j: Integer;
  dx,dy,ptx,pty,SinR,CosR:Extended;  
  pt : TPoint;
begin
  SetLength(Result,Sides);
  ptx := Dir.x;
  pty := Dir.y;
  SinR := Sin(Radians(360.0/Sides));
  CosR := Cos(Radians(360.0/Sides)); 
  j := 1;
  Result[0] := Point(Round(ptx),Round(pty));
  for i:=1 to Sides-1 do
  begin
    dx := ptx - Center.x;
    dy := pty - Center.y;
    ptx := (dy * SinR) + (dx * CosR) + Center.x;
    pty := (dy * CosR) - (dx * SinR) + Center.y;
    pt := Point(Round(ptx),Round(pty));
    if SamePoints(Result[j-1], pt) = False then 
    begin
      Result[j] := pt; 
      Inc(j);
    end;
  end;
  SetLength(result, j);
end;


{*
 Creates all the points needed to define a Ellipse.
 Algorithm is based on Bresenham's circle algorithm, tho might be more similr to MidPoint-Circle.
*}
procedure TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer); StdCall;
var
  RadXSQ,RadYSQ,TwoSQX,TwoSQY,p,x,y,px,py,H:Integer;
  Stack: TPointList;
begin
  RadXSQ := Sqr(RadX);
  RadYSQ := Sqr(RadY);
  twoSQX := 2 * RadXSQ;
  twoSQY := 2 * RadYSQ;
  x := 0;
  y := RadY;
  px := 0;
  py := twoSQX * y;
  
  Stack.InitWith(TPA);
  Stack.Append(Point(Center.x + x, Center.y + y));
  Stack.Append(Point(Center.x - x, Center.y + y));
  Stack.Append(Point(Center.x + x, Center.y - y));
  Stack.Append(Point(Center.x - x, Center.y - y));

  {* Region 1 *}
  p := Round(RadYSQ - (RadXSQ * RadY) + (0.25 * RadXSQ));
  while (px < py) do
  begin
    Inc(x);
    px := px + twoSQY;
    if (p < 0) then
      p := p + (RadYSQ + px)
    else begin
      Dec(y);
      py := py - twoSQX;
      p := p + (RadYSQ + px - py);
    end;
    Stack.Append(Point(Center.x + x, Center.y + y));
    Stack.Append(Point(Center.x - x, Center.y + y));
    Stack.Append(Point(Center.x + x, Center.y - y));
    Stack.Append(Point(Center.x - x, Center.y - y));
  end;

  {* Region 2 *}
  P := Round(RadYSQ * (x+0.5) * (x+0.5) + RadXSQ * (y-1) * (y-1) - RadXSQ * RadYSQ);
  while (y > 0) do
  begin
    Dec(y);
    py := py - twoSQX;
    if (p > 0) then
      p := p + (RadXSQ - py)
    else begin
      Inc(x);
      px := px + twoSQY;
      p := p + (RadXSQ - py + px);
    end;
    Stack.Append(Point(Center.x + x, Center.y + y));
    Stack.Append(Point(Center.x - x, Center.y + y));
    Stack.Append(Point(Center.x + x, Center.y - y));
    Stack.Append(Point(Center.x - x, Center.y - y));
  end;
  TPA := Stack.Clone;
  Stack.Free;
end;


{*
 Creates all the points needed to define a Circle.
 Algorithm is based on Bresenham's circle algorithm, tho might be more similr to MidPoint-Circle.
*}
procedure TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer); StdCall;
begin
  TPAEllipse(TPA, Center, Radius, Radius);
end;


{*
 Uses `SimplePolyPoints` combined with ConnectTPA to draw a line trough each
 point given by `SimplePolyPoints`. So we get a "proper polygon".
*}
procedure TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint); StdCall;
begin
  TPA := ConnectTPA(XagonPoints(Center, Sides, Dir));
end;


{*
 A 2D-implementation of ConvexHull. ConvexHull can be explained with simple words:
 |> Given a Array of Points, imagine that you where to put a rubber band around am...
 |> The points which strech the rubber band are the points returned by this algorithm.

 Complexity: `m*n` (M as in Width, and N as in Height) before sorting the points..
 Once sorted and cleaned it's close to `n` (as in len(tpa)).

 It can be made worst case `n log n` with the use of a fast sorting-algorithm over `CleanSortTPA(TPA)`.
*}

{*__PRIVATE__
  Given a triangle (3 points): Q, P and R. We check if QP -> QR forms a right turn. 
  If the result is less then 0 then it forms a right turn, greater then 0 then it forms a left turn.
  @note: This is mainly just used in ConvexHull, but it can be used for more.
*}
function __VectorTurn(const p, q, r: TPoint): Boolean; Inline;
begin
  Result := (((q.x*r.y + p.x*q.y + r.x*p.y) - (q.x*p.y + r.x*q.y + p.x*r.y)) < 0);
end;
 
function ConvexHull(const TPA:TPointArray): TPointArray; StdCall;
var
  Pts, Lower: TPointArray;
  LH,H,I,UH:Integer;
begin
  if High(TPA) < 0 then Exit;
  // Get a local list copy of the points, and remove dupes.
  Pts := CleanSortTPA(TPA);
  H := High(Pts);
  if H <= 2 then
  begin
    Result := Pts;
    Exit;
  end;

  (* Upper half.. *)
  UH := 2;
  SetLength(Result, H+1);
  Result[0] := Pts[0];
  Result[1] := Pts[1];
  for i:=2 to H do
  begin
    Result[UH] := Pts[i];
    Inc(UH);
    while (UH > 2) do
    begin
      if __VectorTurn(Result[UH-3], Result[UH-2], Result[UH-1]) then Break;
      Dec(UH);
      Result[UH-1] := Result[UH];
    end;
  end;

  (* Lower half.. *)
  LH := 2;
  SetLength(Lower, H+1);
  Lower[0] := Pts[H];
  Lower[1] := Pts[H-1];
  for i:=2 to H do
  begin
    Lower[LH] := Pts[H-i];
    Inc(LH);
    while (LH > 2) do
    begin
      if __VectorTurn(Lower[LH-3], Lower[LH-2], Lower[LH-1]) then Break;
      Dec(LH);
      Lower[LH-1] := Lower[LH];
    end;
  end;

  Dec(LH);
  SetLength(Result, UH+LH);
  for i:=UH to (UH+LH)-1 do
    Result[i] := Lower[i-UH];

  SetLength(Lower, 0);
  SetLength(Pts, 0);
end;


{*
 Fills the resulting TPA with the given shape (TPA), and all the points within it.
 It requires you to give starting point, which is from where the floodfill is going to start.

 It's also recomended that you start within the shape.
*}
function FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean): TPointArray; StdCall;
var
  I,S,j,x,y,H,fj:Integer;
  face:TPointArray;
  Matrix:T2DBoolArray;
  Area: TBox;
  Queue: TPointList;
begin
  Area := TPABounds(TPA);
  Area.x2 := (Area.x2 - Area.x1) + 1;
  Area.y2 := (Area.y2 - Area.y1) + 1;
  Matrix := BoolMatrixNil(Area.x2+1, Area.y2+1);
  H := High(TPA);
  SetLength(Result, ((Area.x2+1)*(Area.y2+1))+H+1);

  for I:=0 to H do
  begin
    Matrix[(TPA[i].y - Area.y1)][(TPA[i].x - Area.x1)] := True;
    if KeepEdges then
      Result[i] := TPA[i];
  end;

  I := 0;
  if KeepEdges then I := H+1;
  fj := 3;
  if EightWay then fj := 7;
  SetLength(Face, fj+1);

  Queue.Init;
  Queue.Append(Point((Start.x-Area.x1), (Start.y-Area.y1)));
  while Queue.NotEmpty do
  begin
    GetAdjacent(Face, Queue.FastPop, EightWay);
    for j:=0 to fj do
    begin
      x := face[j].x;
      y := face[j].y;
      if ((x >= 0) and (y >= 0) and (x <= Area.x2) and (y <= Area.y2)) then
      begin
        if Matrix[y][x] <> True then
        begin
          Matrix[y][x] := True;
          Queue.Append(face[j]);
          Result[i] := Point((x + Area.x1), (y + Area.y1));
          Inc(I);
        end;
      end;
    end;
  end;
  Queue.Free;
  SetLength(Face, 0);
  SetLength(Matrix, 0);
  SetLength(Result, I);
end;


function FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray; StdCall;
begin
  if High(TPA) < 0 then Exit;
  Result := FloodFillTPAEx(TPA,Start,EightWay,False);
end;


{*
 Returns the outer points/contours of a shape with no gaps.
 If a shape has gaps then I suggest using TPAExtractShape and maybe Combined with ClusterTPA..
*}
function TPAOutline(const TPA:TPointArray): TPointArray; StdCall;
var
  i,j,h,x,y,hit:Integer;
  Matrix: T2DIntArray;
  adj: TPointArray;
  start,prev,endpt:TPoint;
  Area: TBox;
  List: TPointList;
begin
  H := High(TPA);
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 1;  //Width
  Area.Y2 := (Area.Y2 - Area.Y1) + 1;  //Height

  Matrix := IntMatrixNil(Area.X2+1, Area.Y2+1);

  start := Point(Area.X2, Area.Y2);
  for i:=0 to H do
  begin
    x := (TPA[i].x-Area.X1);
    y := (TPA[i].y-Area.Y1);
    Matrix[y][x] := 1;
    if y < Start.y then
      Start := Point(x,y);
  end;
  
  H := H*4;
  endpt := start;
  prev := Point(start.x, start.y-1);
  hit := 0;
  List.Init;
  List.Append(Point((Start.x+Area.x1), (Start.y+Area.y1)));
  SetLength(adj, 8);
  for i:=0 to H do
  begin
    if (SamePoints(endpt, prev) and (i>1)) then begin
      if hit = 1 then Break;
      Inc(hit);
    end;
    RotatingAdjecent(adj, start, prev);
    for j:=0 to 7 do begin
      x := adj[j].x;
      y := adj[j].y;
      if (x >= 0) and (x < Area.X2) and
         (y >= 0) and (y < Area.Y2) then
        if Matrix[y][x] >= 1 then
        begin
          prev := start;
          start := adj[j];
          if Matrix[y][x]=1 then
          begin
            List.Append(Point((Start.x+Area.x1), (Start.y+Area.y1)));
            Matrix[y][x] := 2;
          end;
          break;
        end;
    end;
  end;
  Result := List.Clone;
  List.Free;
  SetLength(Adj, 0);
  SetLength(Matrix, 0);
end;


{*
 Returns the border outside your shape.
 For multiple shapes, I would suggest ClusterTPA(Dist = 1, 8way=False) first..
 then grab borders of the shapes you want.
*}
function TPABorder(const TPA:TPointArray): TPointArray; StdCall;
var
  i,j,h,x,y,hit:Integer;
  Matrix: T2DIntArray;
  adj: TPointArray;
  start,prev,endpt:TPoint;
  Area: TBox;
  isset:Boolean;
  List: TPointList;
begin
  H := High(TPA);
  Area := TPABounds(TPA);
  Area.X2 := (Area.X2 - Area.X1) + 3;  //Width
  Area.Y2 := (Area.Y2 - Area.Y1) + 3;  //Height
  Area.X1 := Area.X1 - 1;
  Area.Y1 := Area.Y1 - 1;

  Matrix := IntMatrixNil(Area.X2+1, Area.Y2+1);

  start := Point(Area.X2, Area.Y2);
  for i:=0 to H do
    Matrix[(TPA[i].y-Area.Y1)][(TPA[i].x-Area.X1)] := 1;

  //find FIRST starting y coord.
  Isset := False;
  Start := Point(Area.X2, Area.Y2);
  for y:=0 to Area.Y2-1 do begin
    for x:=0 to Area.X2-1 do
      if Matrix[y][x] <> 0 then
      begin
        Start := Point(x,y);
        Isset := True;
        Break;
      end;
    if Isset then Break;
  end;

  H := H*4;
  endpt := Start;
  prev := Point(start.x, start.y-1);
  hit := 0;
  List.Init;

  SetLength(adj, 8);
  for i:=0 to H do
  begin
    if (SamePoints(endpt, start) and (i>1)) then begin
      if hit = 1 then Break;
      Inc(hit);
    end;
    RotatingAdjecent(adj, start, prev);
    for j:=0 to 7 do begin
      x := adj[j].x;
      y := adj[j].y;
      if (x >= 0) and (x < Area.X2) and
         (y >= 0) and (y < Area.Y2) then
        if Matrix[y][x] <= 0 then begin
          if Matrix[y][x] = 0 then
          begin
            List.Append(Point((adj[j].x+Area.x1), (adj[j].y+Area.y1)));
            Dec(Matrix[y][x]);
          end;
        end else if Matrix[y][x] >= 1 then
        begin
          prev := start;
          start := adj[j];
          Break;
        end;
    end;
  end;
  Result := List.Clone;
  List.Free;
  SetLength(Adj, 0);
  SetLength(Matrix, 0);
end;


{*
 FloodFills a _Polygon_, the result in other words are all the points on and in the edges of the polygon.
 Should be stable.
*}
function FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray; StdCall;
begin
  if High(Poly) < 0 then Exit;
  Result := FloodFillTPAEx(TPABorder(ConnectTPA(Poly)), Poly[0], EightWay, False);
end;


{*
 ClusterTPA is a `complex` function, it's action is the same as SplitTPA(Ex) seen in
 Simba, and SCAR (Macro-programs), but unlike those, this one performce in O(n)-time, while
 SplitTPA(ex) has a time-complexity of O(n^2).
 
 In short this algorithm uses a 2D-Matrix to cluster together the points that are 
 within a given distance (Distx,Disty) from each other. It then returns 2D TPoint Array (T2DPointArray).
*}
function ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray; StdCall;
var
  W,H,i,x,y,rw,rh,x1,y1:Integer;
  R,qsize,fsize,Count,S,j,L:Integer;
  Area:TBox;
  Matrix,Table:T2DIntArray;
  face:TPointArray;
  Queue: TPointList;
  pt,adj,testpt:TPoint;
begin
  Area := TPABounds(TPA);
  Area.x1 := Area.x1 - 3;
  Area.y1 := Area.y1 - 3;
  W := (Area.x2 - Area.x1) + 1;
  H := (Area.y2 - Area.y1) + 1;
  if Distx > W then Distx := W;
  if Disty > H then Disty := H;
  RH := H-1;
  RW := W-1;
  Count := 0;
  L := High(TPA);
  SetLength(Matrix, H+2, W+2);

  //-----------
  //Method depends on a lot of things, tho I just estimate it..
  case (((RW*RH)*8) < (L*(Distx+Disty))) of
   False:
    begin
      for i:=0 to L do
      begin
        pt.x := (TPA[i].x - Area.X1);
        pt.y := (TPA[i].y - Area.Y1);
        x1 := Min(RW, pt.x + DistX - 1);
        y1 := Min(RH, pt.y + DistY - 1);
        for x:=pt.x to x1 do begin
          Matrix[pt.y][x] := -2;
          Matrix[y1][x] := -2;
        end;
        for y:=pt.y to y1 do begin
          Matrix[y][pt.x] := -2;
          Matrix[y][x1] := -2;
        end;
      end;
    end;

   True:
    begin
      SetLength(Table, (H+2), (W+2));
      for i:=0 to L do
      begin
        x := (TPA[i].x - Area.X1);
        y := (TPA[i].y - Area.Y1);
        Matrix[y][x] := 1;
        Table[y][x+1] := 1;
      end;
      for y:=0 to RH do
        for x:=0 to RW do
          Table[y+1][x+1] := (Table[y+1][x] + Table[y][x+1] - Table[y][x] + Matrix[y][x]);
      for y:=1 to RH do begin
        y1 := Min(H, y + DistY);
        for x:=1 to RW do begin
          x1 := Min(W, x + DistX);
          R := (Table[y][x] + Table[y1][x1] - Table[y1][x] - Table[y][x1]);
          if R > 0 then begin
            Matrix[y][x] := -2;
          end else
            Matrix[y][x] := -99;
        end;
      end;
      SetLength(Table, 0);
    end;
  end;


  //--------------
  //Simply floodfill the resulting boxes.
  qsize := L;
  Queue.Init;

  fsize := 7;
  if EightWay = False then fsize := 3;
  SetLength(Face, fsize+1);

  for i:=0 to L do
  begin
    pt.x := (TPA[i].x - Area.X1);
    pt.y := (TPA[i].y - Area.Y1);
    if Matrix[pt.y][pt.x] = -2 then
    begin
      Matrix[pt.y][pt.x] := Count;
      Queue.Append(pt);
      while Queue.NotEmpty do
      begin
        GetAdjacent(Face, Queue.FastPop, EightWay);
        for j:=0 to fsize do
        begin
          adj := face[j];
          if Matrix[adj.y][adj.x] = -2 then
          begin
            Matrix[adj.y][adj.x] := Count;
            Queue.Append(Adj);
          end;
        end;
      end;
      Count := Count + 1;
    end;
  end;
  SetLength(Face, 0);
  Queue.Free;

  //-----------
  // Creating the result.
  SetLength(Result, Count);
  SetLength(Table, Count, 2);
  for i:=0 to L do
  begin
    pt := TPA[i];
    J := Matrix[(pt.y-Area.Y1)][(pt.x-Area.X1)];
    if J >= 0 then
    begin
      S := Table[J][0];
      Inc(Table[J][1]);
      R := Table[J][1];
      if S <= R then
      begin
        Table[J][0] := R+R;
        SetLength(Result[J], R+R+1);
      end;
      Result[J][R-1] := PT;
    end;
  end;
  for i:=0 to Count-1 do
    SetLength(Result[I], Table[i][1]);

  SetLength(Table, 0);
  SetLength(Matrix, 0);
end;


//-------------------------------------
function ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray; StdCall;
begin
  Result := ClusterTPAEx(TPA, Distance,Distance, EightWay);
end;


{*
 Returns all points that are not completely surrounded. It checks the 4 neighbour points.
 This points can easily be defined as edge-points.
*}
function TPAEdges(const TPA: TPointArray): TPointArray; StdCall;
var
  i,j,x,y,H:Integer;
  Matrix: T2DBoolArray;
  face:TPointArray;
  List: TPointList;
  adj:TPoint;
  Area: TBox;
begin
  H := High(TPA);
  Area := TPABounds(TPA);
  SetLength(Matrix, Area.Height, Area.Width);
  for i:=0 to H do
    Matrix[TPA[i].Y - Area.Y1][TPA[i].X - Area.X1] := True;
  
  List.Init;
  SetLength(face, 4);
  for i:=0 to H do
  begin
    x := TPA[i].x - Area.X1;
    y := TPA[i].y - Area.Y1;
    GetAdjacent(Face, Point(x,y), False); 
    for j:=0 to 3 do
    begin
      adj := Face[j];
      if (adj.x < 0) or (adj.x >= Area.Width) or (adj.y < 0) or (adj.y >= Area.Height) then
      begin
        List.Append(TPA[i]);
        Break;
      end
      else if not(Matrix[adj.y][adj.x]) then
      begin
        List.Append(TPA[i]);
        Break;
      end;
    end;
  end;
  
  Result := List.Clone;
  List.Free;
end;

end.
