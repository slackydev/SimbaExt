unit XT_Tests;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
interface

uses CoreTypes, Sysutils;

function SplitTPA(const TPA:TPointArray; Dist:Integer): T2DPointArray; Cdecl;
function SplitTPA2(const TPA:TPointArray; Dist:Integer): T2DPointArray; Cdecl;

//-----------------------------------------------------------------------
implementation

uses  PointTools, Sorting, Math;


(*
 Returns every row from the TPA.
 @note:
       It also outputs the empty rows!
*)
function TPAAllRows(const TPA:TPointArray; Bounds:TBox; InOrder:Boolean): T2DPointArray;
var
  arr:TPointArray;
  i,idx: Integer;
begin
  Arr := Copy(TPA);
  if InOrder then SortTPAByY(Arr);
  SetLength(Result, Bounds.Width);
  for i:=0 to High(Arr) do
  begin
    idx := Arr[i].x-Bounds.x1;
    SetLength(Result[idx], Length(Result[idx])+1);
    Result[idx][High(Result[idx])] := Arr[i];
  end;
end;


(*
 Returns all the points within range of Point `PT`, within the given distance.
 Hyp = Dist*Dist
 Rows = Lookup array (represents all the points in TPA (from SplitTPA))
 LoX, HiX = Lower and higher X-bounds (Index-range for where to look in Rows)
*)
function BisecNeighbors(const Rows:T2DPointArray; PT:TPoint; dist, hyp, loX,hiX: Integer;
                        var R:TPointArray; out count:Integer): Boolean;
var
  i,j,mid,lo,hi,len: Int32;
begin
  count := 0;
  for i:=LoX to HiX do
  begin
    len := High(Rows[i]);
    // Lower bounds
    hi := len;
    mid := 0;
    lo := 0;
    while (lo < hi) do begin
      Mid := (Hi+Lo) shr 1;
      if (Rows[i][mid].y >= (PT.Y-dist)) then
        Hi := Mid
      else
        Lo := Mid+1;
    end;

    // Extract good pts
    for j:=Mid to Len do
    begin
      if (Rows[i][j].y > PT.Y+dist) then Break;
      if (Sqr(Rows[i][j].x - PT.x) + Sqr(Rows[i][j].y - PT.y)) <= Hyp then
      begin
        R[count] := Point(i,j);
        Inc(count);
      end;
    end;
  end;
  Result := Count > 0;
end;


(*
 SplitTPA function with a time complixity in the range of n*log(n).
 Memory overhead (I think) in the order of:
   BytTP = SizeOf(TPoint)
   BytInt = SizeOf(int32)
   Mem = (3 * Length(TPA) * SizTP) + (TPA.Width * BytInt)
   + Result size ofcourse...

 Should be able to compute TPA = [Point(-9999999, -9999999), Point(9999999,9999999)]
 But that would result in having to set
 Tho due to the large spread that would probably use around 70MB RAM (Row-count would be like 20mill).
*)
function SplitTPA(const TPA:TPointArray; Dist:Integer): T2DPointArray; Cdecl;
var
  lo,hi,j,i,ts,c,w,jj,ii,n,sqdist,len,wid: Integer;
  Rows: T2DPointArray;
  R: TPointArray;
  Curr,p,f:TPoint;
  B: TBox;
begin
  Len := Length(TPA);
  if Len = 0 then Exit;

  B := TPABounds(TPA);
  Rows := TPAAllRows(TPA, B, True);
  Wid := B.Width - 1;

  sqdist := Trunc(Sqr(Dist + 0.5));
  dist := dist + 1;

  SetLength(Result, len);
  SetLength(R, len);

  i := 0;
  j := 0;
  while (len > 0) do
  begin
    while Length(Rows[j]) = 0 do Inc(j);

    SetLength(Result[i], 1);
    Result[i][0] := Rows[j][High(Rows[j])];
    SetLength(Rows[j], High(Rows[j]));

    ts := 1;
    c := 0;
    w := 0;
    while c <= w do
    begin
      Curr := Result[i][c];
      Lo := max((Curr.x-B.x1)-dist, j);
      Hi := min((Curr.x-B.x1)+dist, Wid);
      n := 0;

      if BisecNeighbors(Rows, curr, dist, sqdist, lo, hi, R, n) then
      begin
        f := R[0];
        jj := 0;
        for ii:=0 to n-1 do
        begin
          p := R[ii];
          SetLength(Result[i], ts+1);
          Result[i][ts] := Rows[p.x][p.y];
          inc(ts);

          if (f.x = p.x) then
            Inc(jj)
          else begin //move Y-block, and reset counter + 1
            Move(Rows[f.x][f.y+jj], Rows[f.x][f.y], (Length(Rows[f.x])-(f.y+jj)) * SizeOf(TPoint));
            SetLength(Rows[f.x], Length(Rows[f.x])-jj);
            f := p;
            jj := 1;
          end;
        end;
        w := w+n;
        Move(Rows[f.x][f.y+jj], Rows[f.x][f.y], (Length(Rows[f.x])-(f.y+jj)) * SizeOf(TPoint));
        SetLength(Rows[f.x], Length(Rows[f.x])-jj);
      end;
      inc(c);
    end;
    len := len - ts;
    inc(i);
  end;
  SetLength(Result, i);
end;










//------------------------- EVEN LESS MEMORY USAGE ---------------------------\\
// To bad this concept sucks ass, performance wice...

(*
 Returns each row in the TPA as a separate TPA (Result = T2DPointArray)
*)
function TPARows(Arr:TPointArray): T2DPointArray;
var
  i,j,hi,x,rows: Integer;
  TPA:TPointArray;
begin
  if Length(Arr) = 0 then Exit;
  TPA := Copy(Arr);
  SortTPAByColumn(TPA);

  x := TPA[0].y;
  SetLength(Result, 1);
  rows := 1;
  hi := 0;
  j := 0;
  for i:=0 to High(TPA) do
  begin
    if TPA[i].x = x then
    begin
      SetLength(Result[j], hi+1);
      Result[j][hi] := TPA[i];
      Inc(hi);
    end else
    begin
      hi := 0;
      x := TPA[i].x;
      inc(j);
      SetLength(Result, j+1);
      SetLength(Result[j], hi+1);
      Result[j][hi] := TPA[i];
      Inc(hi);
    end;
  end;
end;


procedure ATPADelete(var ATPA:T2DPointArray; Index: Integer);
var
  i,hi: Integer;
begin
  Hi := High(ATPA);
  for i:=Index to Hi-1 do
    ATPA[i] := ATPA[i+1];
  SetLength(ATPA, Hi);
end;


//Set-like TIA.. Always sorted, and duplicates is impossible.
procedure TIAAdd(var TIA:TIntArray; val:Int32);
var mid,lo,i,hi,l: Integer;
begin
  l := high(TIA);
  if (l = -1) then begin
    SetLength(TIA, 1);
    TIA[0] := val;
    Exit;
  end;
  hi := l;
  mid := 0;
  lo := 0;
  while (lo < hi) do begin
    Mid := (Hi+Lo) div 2;
    if (TIA[mid] >= val) then Hi := Mid
    else                      Lo := Mid+1;
  end;
  if (TIA[lo] = val) then Exit;
  if (TIA[lo] < val) then Inc(lo);
  SetLength(TIA, l + 2);
  Move(TIA[lo], TIA[lo+1], l-lo+1);
  TIA[lo] := val;
end;



function LookupArea(const Rows:T2DPointArray; PT:TPoint; Dist, Hyp: Int32;
                     var R: TPointArray; out n: Int32): Boolean;
var
  hi,lo,mid,len,i,j:Int32;
  B: TBox;
begin
  len := High(Rows);
  n := 0;
  B := Box(pt.x-(dist+1), pt.y-(dist+1), pt.x+(dist+1), pt.y+(dist+1));

  //First find corresponding x to B.x1;
  hi := len;
  mid := 0;
  lo := 0;
  while (lo < hi) do begin
    Mid := (Hi+Lo) div 2;
    if (Rows[mid][0].x >= B.x1) then
      Hi := Mid
    else
      Lo := Mid+1;
  end;

  //Iterate from Lo
  for i:=Lo to len do
  begin
    if (Rows[i][0].x > B.x2) then Break;

    //Find corresponding y to B.y1
    hi := High(Rows[i]);
    mid := 0;
    lo := 0;
    while (lo < hi) do begin
      Mid := (Hi+Lo) div 2;
      if (Rows[i][mid].y >= B.y1) then
        Hi := Mid
      else
        Lo := Mid+1;
    end;
    // Extract good pts
    for j:=Mid to High(Rows[i]) do
    begin
      if (Rows[i][j].y > B.y2) then Break;
      if (Sqr(Rows[i][j].x - PT.x) + Sqr(Rows[i][j].y - PT.y)) <= Hyp then
      begin
        R[n] := Point(i,j); //Returns indexs (Row, column)
        inc(n);
      end;
    end;
  end;
  Result := n > 0;
end;


function SplitTPA2(const TPA:TPointArray; Dist:Integer): T2DPointArray; Cdecl;
var
  hi,j,i,h,ts,c,w,jj,ii,n,sqdist,len,del_n: Integer;
  Rows: T2DPointArray;
  R: TPointArray;
  del: TIntArray;
  Curr,p,f:TPoint;
  B: TBox;
begin
  B := TPABounds(TPA);
  Rows := TPARows(TPA); //Size = n

  h := High(rows);
  len := Length(TPA);
  sqdist := Trunc(Sqr(Dist + 0.5));
  dist := dist + 1;

  SetLength(Result, len);
  SetLength(R, len);

  i := 0;
  j := 0;
  while (len > 0) do
  begin
    while Length(Rows[j]) = 0 do ATPADelete(Rows, j);

    SetLength(Result[i], 1);
    hi := High(Rows[j]);
    Result[i][0] := Rows[j][hi];

    if hi = 0 then ATPADelete(Rows, j)
    else SetLength(Rows[j], hi);

    ts := 1;
    c := 0;
    w := 0;
    while c <= w do
    begin
      Curr := Result[i][c];

      if LookupArea(Rows, curr, dist, sqdist, R, n) then
      begin
        f := R[0];
        jj := 0;
        del_n := 0;
        for ii:=0 to n-1 do
        begin
          p := R[ii];
          SetLength(Result[i], ts+1);

          Result[i][ts] := Rows[p.x][p.y];
          inc(ts);
          if (f.x = p.x) then
            Inc(jj)
          else begin //remove every used point
            Hi := Length(Rows[f.x])-jj;
            if Hi = 0 then TIAAdd(del, f.x); //Row to delete list
            Move(Rows[f.x][f.y+jj], Rows[f.x][f.y], (Hi-f.y) * SizeOf(TPoint));
            SetLength(Rows[f.x], Hi);
            f := p;
            jj := 1;
          end;
        end;
        w := w+n;

        Hi := Length(Rows[f.x])-jj;
        if Hi = 0 then TIAAdd(del, f.x);  //Row to delete list

        Move(Rows[f.x][f.y+jj], Rows[f.x][f.y], (Hi-f.y) * SizeOf(TPoint));
        SetLength(Rows[f.x], Hi);

        if Length(del) <> 0 then
        begin
          for ii:=High(del) downto 0 do
            ATPADelete(Rows, del[ii]);
          SetLength(del, 0);
        end;
      end;
      inc(c);
    end;
    len := len - ts;
    inc(i);
  end;
  SetLength(Result, i);
end;

      


end.
