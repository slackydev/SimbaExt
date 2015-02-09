Unit Morphology;
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
  CoreTypes, SysUtils;
 
function TPASkeleton(const TPA:TPointArray; FMin,FMax:Int32): TPointArray;
function TPAExpand(const TPA:TPointArray; Iterations:Int32): TPointArray;
function TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Int32): TPointArray;
function DistanceTransform(const binIm:TIntArray; m,n:Int32; distanceUnit:EDistUnit): T2DIntArray;
function DistanceTransform(const TPA:TPointArray; distanceUnit:EDistUnit): T2DIntArray; overload;


//--------------------------------------------------
implementation
uses 
  PointList, PointTools, Math;


{* __PRIVATE__ *}
function __TransitCount(p2,p3,p4,p5,p6,p7,p8,p9:Int32): Int32; Inline;
begin
  Result := 0;
  if ((p2 = 0) and (p3 = 1)) then Inc(Result);
  if ((p3 = 0) and (p4 = 1)) then Inc(Result);
  if ((p4 = 0) and (p5 = 1)) then Inc(Result);
  if ((p5 = 0) and (p6 = 1)) then Inc(Result);
  if ((p6 = 0) and (p7 = 1)) then Inc(Result);
  if ((p7 = 0) and (p8 = 1)) then Inc(Result);
  if ((p8 = 0) and (p9 = 1)) then Inc(Result);
  if ((p9 = 0) and (p2 = 1)) then Inc(Result);
end;


{*
 @TPASkeleton: 
 Given a set of points, this function should thin the TPA down to it's bare Skeleton.
 It also takes two modifiers which allow you to change the outcome.
 By letting eather FMin, or FMax be -1 then it will be set to it's defaults which are 2 and 6.
*}
function TPASkeleton(const TPA:TPointArray; FMin,FMax:Int32): TPointArray;
var
  j,i,x,y,h,transit,sumn,MarkHigh,hits: Int32;
  p2,p3,p4,p5,p6,p7,p8,p9:Int32;
  Change, PTS: TPointArray;
  Matrix: T2DByteArray;
  iter : Boolean;
  Area: TBox;
begin
  H := High(TPA);
  if (H = -1) then Exit;
  Area := TPABounds(TPA);
  Area.x1 := Area.x1 - 2;
  Area.y1 := Area.y1 - 2;
  Area.x2 := (Area.x2 - Area.x1) + 2;
  Area.y2 := (Area.y2 - Area.y1) + 2;
  SetLength(Matrix, Area.y2, Area.x2);
  if (FMin = -1) then FMin := 2;
  if (FMax = -1) then FMax := 6;

  SetLength(PTS, H + 1);
  for i:=0 to H do
  begin
    x := (TPA[i].x-Area.x1);
    y := (TPA[i].y-Area.y1);
    PTS[i] := Point(x,y);
    Matrix[y][x] := 1;
  end;
  j := 0;
  MarkHigh := H;
  SetLength(Change, H+1);
  repeat
    iter := (J mod 2) = 0;
    Hits := 0;
    i := 0;
    while i < MarkHigh do begin
      x := PTS[i].x;
      y := PTS[i].y;
      p2 := Matrix[y-1][x];
      p4 := Matrix[y][x+1];
      p6 := Matrix[y+1][x];
      p8 := Matrix[y][x-1];

      if (Iter) then begin
        if (((p4 * p6 * p8) <> 0) or ((p2 * p4 * p6) <> 0)) then begin
          Inc(i);
          Continue;
        end;
      end else if ((p2 * p4 * p8) <> 0) or ((p2 * p6 * p8) <> 0) then
      begin
        Inc(i);
        Continue;
      end;

      p3 := Matrix[y-1][x+1];
      p5 := Matrix[y+1][x+1];
      p7 := Matrix[y+1][x-1];
      p9 := Matrix[y-1][x-1];
      Sumn := (p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9);
      if (SumN >= FMin) and (SumN <= FMax) then begin
        Transit := __TransitCount(p2,p3,p4,p5,p6,p7,p8,p9);
        if (Transit = 1) then begin
          Change[Hits] := PTS[i];
          Inc(Hits);
          PTS[i] := PTS[MarkHigh];
          PTS[MarkHigh] := Point(x,y);
          Dec(MarkHigh);
          Continue;
        end;
      end;
      Inc(i);
    end;

    for i:=0 to (Hits-1) do
      Matrix[Change[i].y][Change[i].x] := 0;

    inc(j);
  until ((Hits=0) and (Iter=False));

  SetLength(Result, (MarkHigh + 1));
  for i := 0 to MarkHigh do
    Result[i] := Point(PTS[i].x+Area.x1, PTS[i].y+Area.y1);

  SetLength(PTS, 0);
  SetLength(Change, 0);
  SetLength(Matrix, 0);
end;


{*
 Inversed skeletonizing, it adds a border of points to the given tpa..
 Border-width is desided by the given number of iterations.
*}
function TPAExpand(const TPA:TPointArray; Iterations:Int32): TPointArray;
var
  H,i,j: Int32;
  Matrix: T2DBoolArray;
  QueueA, QueueB: TPointList;
  face:TPointArray;
  pt:TPoint;
  B: TBox;
begin
  H := High(TPA);
  if (H = -1) or (Iterations=0) then Exit;
  B := TPABounds(TPA);
  B.x1 := B.x1 - Iterations - 1;
  B.y1 := B.y1 - Iterations - 1;
  B.x2 := (B.x2 - B.x1) + Iterations + 1;
  B.y2 := (B.y2 - B.y1) + Iterations + 1;
  SetLength(Matrix, B.y2, B.x2);
  for i:=0 to H do
    Matrix[TPA[i].Y - B.Y1][TPA[i].X - B.X1] := True;

  SetLength(face,4);
  QueueA.InitWith(TPAEdges(TPA));
  QueueA.Offset(-B.X1,-B.Y1);
  QueueB.Init;
  j := 0;
  repeat
    case (J mod 2) = 0 of
    True:
      while QueueA.NotEmpty do 
      begin
        GetAdjacent(face, QueueA.FastPop, False);
        for i:=0 to 3 do
        begin
          pt := face[i];
          if not(Matrix[pt.y][pt.x]) then
          begin
            Matrix[pt.y][pt.x] := True;
            QueueB.Append(pt);
            Inc(H);
          end;
        end;
      end;
      
    False:
      while QueueB.NotEmpty do 
      begin
        GetAdjacent(face, QueueB.FastPop, False);
        for i:=0 to 3 do
        begin
          pt := face[i];
          if not(Matrix[pt.y][pt.x]) then
          begin
            Matrix[pt.y][pt.x] := True;
            QueueA.Append(pt);
            Inc(H);
          end;
        end;
      end;
    end;
    Inc(j);
  until (j >= Iterations);
  QueueA.Free;
  QueueB.Free;

  SetLength(Result, H+1);
  for I:=0 to B.y2-1 do
    for j:=0 to B.x2-1 do
    begin
      if H<0 then Break;
      if Matrix[i][j] then begin
        Result[H] := Point(j+B.x1,i+B.y1);
        Dec(H);
      end;
    end;

  SetLength(Matrix, 0);
  SetLength(Face, 0);
end;


{*
 //Based on TPASkeleton (might be changed/modified).
 TPAReduce does steps of skeletonizing. So in other words it continiously
 removes the most outer points from tha TPA, that is until it has done the
 given amount of iterations, or the shapes can no longer be thinned.
*}
function TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Int32): TPointArray;
var
  j,i,x,y,h,transit,sumn,MarkHigh,hits: Int32;
  p2,p3,p4,p5,p6,p7,p8,p9:Int32;
  Change, PTS: TPointArray;
  Matrix: T2DByteArray;
  iter : Boolean;
  Area: TBox;
begin
  H := High(TPA);
  if (H = -1) then Exit;
  Area := TPABounds(TPA);
  Area.x1 := Area.x1 - 2;
  Area.y1 := Area.y1 - 2;
  Area.x2 := (Area.x2 - Area.x1) + 2;
  Area.y2 := (Area.y2 - Area.y1) + 2;
  SetLength(Matrix, Area.y2, Area.x2);

  if (FMin = -1) then FMin := 2;
  if (FMax = -1) then FMax := 6;

  SetLength(PTS, H + 1);
  for i:=0 to H do
  begin
    x := (TPA[i].x-Area.x1);
    y := (TPA[i].y-Area.y1);
    PTS[i] := Point(x,y);
    Matrix[y][x] := 1;
  end;
  j := 0;
  MarkHigh := H;
  SetLength(Change, H+1);
  repeat
    iter := (J mod 2) = 0;
    Hits := 0;
    i := 0;
    while i < MarkHigh do begin
      x := PTS[i].x;
      y := PTS[i].y;
      p2 := Matrix[y-1][x];
      p4 := Matrix[y][x+1];
      p6 := Matrix[y+1][x];
      p8 := Matrix[y][x-1];

      if (Iter) then begin
        if (p4+p6+p8=3) or (p2+p4+p6=3) then 
        begin
          Inc(i);
          Continue;
        end;
      end else if (p2+p4+p8=3) or  (p2+p6+p8=3) then
      begin
        Inc(i);
        Continue;
      end;

      p3 := Matrix[y-1][x+1];
      p5 := Matrix[y+1][x+1];
      p7 := Matrix[y+1][x-1];
      p9 := Matrix[y-1][x-1];
      Sumn := (p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9);
      if (SumN >= FMin) and (SumN <= FMax) then begin
        Transit := __TransitCount(p2,p3,p4,p5,p6,p7,p8,p9);
        if (Transit = 1) then begin
          Change[Hits] := PTS[i];
          Inc(Hits);
          PTS[i] := PTS[MarkHigh];
          PTS[MarkHigh] := Point(x,y);
          Dec(MarkHigh);
          Continue;
        end;
      end;
      Inc(i);
    end;

    for i:=0 to (Hits-1) do
      Matrix[Change[i].y][Change[i].x] := 0;

    inc(j);
  until ((Hits=0) or (j>=Iterations)) and (Iter=False);

  SetLength(Result, (MarkHigh + 1));
  for i := 0 to MarkHigh do
    Result[i] := Point(PTS[i].x+Area.x1, PTS[i].y+Area.y1);

  SetLength(PTS, 0);
  SetLength(Change, 0);
  SetLength(Matrix, 0);
end;



{*
 Distance transform
*}

function dtEucDist(x1,x2:Int32): Int32; inline;
begin
  Result := Sqr(x1) + Sqr(x2);
end;

function dtEucSep(i,j, ii,jj:Int32): Int32; inline;
begin
  Result := Round((sqr(j) - sqr(i) + sqr(jj) - sqr(ii))/(2*(j-i)));
end;


function dtMtnDist(x1, x2: Int32): Int32; inline;	
begin
  Result := Abs(x1) + x2	
end;

function dtMtnSep(i,j, ii, jj: Int32): Int32; inline;
begin
  if (jj >= (ii + j - i)) then
    Exit($FFFFFF);
  if (ii > (jj + j - i)) then
    Exit(-$FFFFFF);
  Result := (jj-ii+j+i) shr 1;
end;

function dtChbDist(x1, x2: Int32): Int32; inline;
begin
  Result := Max(Abs(x1), x2);
end;


function dtChbSep(i,j, ii,jj: Int32): Int32; inline;
begin
  if (ii <= jj) then
    Result := Max(i+jj, Trunc((i+j) / 2))
  else
    Result := Min(j-ii, Trunc((i+j) / 2));
end;


function DistanceTransform(const binIm:TIntArray; m,n:Int32; distanceUnit:EDistUnit): T2DIntArray;
type
  TSepFunc = function(i,j, ii,jj: Int32): Int32;
  TDistFunc = function(x1,x2: Int32): Int32;
var
  x,y,h,w,i,wid,dist:Int32;
  tmp,s,t:TIntArray;
  sf: TSepFunc;
  df: TDistFunc;
begin
  // first pass
  SetLength(tmp, m*n);
  h := n-1;
  w := m-1;
  for x:=0 to w do
  begin
    if binIm[x] <> 0 then
      tmp[x] := 0
    else
      tmp[x] := m+n;

    for y:=1 to h do
      if (binIm[y*m+x] <> 0) then
        tmp[y*m+x] := 0
      else
        tmp[y*m+x] := 1 + tmp[(y-1)*m+x];

    for y:=h-1 downto 0 do
      if (tmp[(y+1)*m+x] < tmp[y*m+x]) then
        tmp[y*m+x] := 1 + tmp[(y+1)*m+x]
  end;


  case distanceUnit of
    duEuclidean:
    begin
      df := @dtEucDist;
      sf := @dtEucSep;
    end;
    duManhatten:
    begin
      df := @dtMtnDist;
      sf := @dtMtnSep;
    end;
    duChebyshev:
    begin
      df := @dtChbDist;
      sf := @dtChbSep;
    end;
  end;

  // second pass
  SetLength(Result,n,m);
  SetLength(s,m);
  SetLength(t,m);
  wid := 0;
  for y:=0 to h do
  begin
    i := 0;
    s[0] := 0;
    t[0] := 0;

    for x:=1 to W do
    begin
      while (i >= 0) and (df(t[i]-s[i], tmp[y*m+s[i]]) > df(t[i]-x, tmp[y*m+x])) do
        Dec(i);
      if (i < 0) then
      begin
        i := 0;
        s[0] := x;
      end else
      begin
        wid := 1 + sf(s[i], x, tmp[y*m+s[i]], tmp[y*m+x]);
        if (wid < m) then
        begin
          Inc(i);
          s[i] := x;
          t[i] := wid;
        end;
      end;
    end;

    for x:=W downto 0 do
    begin
      dist := df(x-s[i], tmp[y*m+s[i]]);
      if distanceUnit = duEuclidean then dist := Round(Sqrt(dist));

      Result[y,x] := dist;
      if (x = t[i]) then Dec(i);
    end;
  end;
end;


function DistanceTransform(const TPA:TPointArray; distanceUnit:EDistUnit): T2DIntArray; overload;
var
  data:TIntArray;
  w,h,n,i:Int32;
  area:TBox;
begin
  n := Length(TPA);
  if (n = 0) then Exit;
  area := TPABounds(TPA);
  w := (area.x2 - area.x1) + 1;
  h := (area.y2 - area.y1) + 1;
  SetLength(data, h*w);
  for i:=0 to n-1 do
    with TPA[i] do
      data[(y-area.y1)*w+(x-area.x1)] := 1;
      
  Result := DistanceTransform(data,w,h,distanceUnit);
end;

end.








