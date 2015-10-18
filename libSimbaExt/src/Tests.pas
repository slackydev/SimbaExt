Unit Tests;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Experimental stuff.. don't use, as it might not work.. At all.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$inline on}

interface

uses
  CoreTypes, SysUtils;
 
function ClusterTest(TPA:TPointArray; dist:Int32): T2DPointArray; cdecl;
function ApprCluster(pts:TPointArray; xrad,yrad:Int32; fastAppr:Boolean): T2DPointArray; cdecl;
procedure GaussianBlur_(const image:T2DIntArray; var dest:T2DIntArray; radius:Int32; sigma:Single);cdecl;

//--------------------------------------------------
implementation
uses 
  Sorting, Math, CoreMath, CoreMisc, Trees, PointTools, PointList, imaging, timeutils;
  

function AsGray(mat:T2DIntArray): TFloatArray;
var
  x,y,w,h,color,i:Int32;
begin
  w := High(Mat[0]);
  h := High(Mat);
  SetLength(Result, (H+1)*(W+1));
  i:=0;
  for y:=0 to h do
    for x:=0 to w do
    begin
      color := Mat[y,x];
      result[i] := ((Color and $FF) + ((Color shr 8) and $FF) + ((Color shr 16) and $FF)) div 3;
      Inc(i);
    end;
end;

//test - in gray-scale.
procedure GaussianBlur_(const image:T2DIntArray; var dest:T2DIntArray; radius:Int32; sigma:Single); cdecl;
var
  lo,hi,i,x,y,wid,hei,xx,yy,offset,dia,size:Int32;
  data,temp,kernel:TFloatArray;
  time:Double;
  tptr,ptr:PSingle;
begin
  dia := radius*2;
  wid := Length(image[0]);
  hei := Length(image);
  SetLength(dest, hei,wid);

  kernel := GaussKernel1D(radius, sigma);

  data := AsGray(image); //test only on 1 channel
  SetLength(temp, length(data));
  size := wid*hei;

  (* the actual processing... Which is what I'm timing *)
  time := MarkTime();
  // x direction
  for offset:=0 to dia do
  begin
    xx := offset-radius;
    if xx < 0 then lo := abs(xx) else lo := 0;
    if xx > 0 then hi := (size-1)-xx else hi := size-1;
    for i:=lo to hi do
      temp[i] += data[i+xx] * kernel[offset];
  end;
  FillByte(data[0], length(data)*SizeOf(data[0]), 0);

  // y direction
  for offset:=0 to dia do
  begin
    yy := (offset-radius)*wid;
    if yy < 0 then lo := abs(yy) else lo := 0;
    if yy > 0 then hi := (size-1)-yy else hi := size-1;
    ptr := @data[lo];
    tptr := @temp[lo+yy];
    hi := PtrUInt(ptr)+(hi*SizeOf(Single));
    while PtrUInt(ptr) < hi do
    begin
      ptr^ += tptr^ * kernel[offset];
      Inc(ptr);
      Inc(tptr);
    end;
  end;
  WriteLn(Round(MarkTime() - time));

  (* build the result *)
  i := 0;
  for y:=0 to hei-1 do
    for x:=0 to wid-1 do
    begin
      dest[y,x] := ftoi(data[i]);
      Inc(i);
    end;
end;
  
procedure FillNodeArray(var root:TSlackTree; var Arr:TSlackNodeArr);
var
  i:Int32;
  procedure Fill(node:PSlackTree);
  begin
    Arr[i] := node;
    Inc(i);
    //WriteLn(i);
    if Node^.R <> nil then Fill(node^.R);
    if Node^.L <> nil then Fill(node^.L);
  end;
begin
  i := 0;
  Fill(@root);
end;

//meh... It was worth a try
function ClusterTest(TPA:TPointArray; dist:Int32): T2DPointArray; cdecl;
var
  i,hi,curr,rc,n,group:Int32;
  range,tmp:TPointArray;
  tree:TSlackTree;
  pts:TSlackNodeArr;
  pt:TPoint;
begin
  // Builds structure
  tree.Init(TPA);
  SetLength(pts, Length(TPA));
  FillNodeArray(tree,pts);
  
  // prepare for war
  hi := High(pts);
  SetLength(Result, hi+1);
  group := 0;
  for i:=0 to hi do
  begin
    if pts[i]^.deleted then continue;

    rc := 1;
    SetLength(Result[group], hi+1);
    Result[group][rc-1] := pts[i]^.split;
    curr := 0;
    while (curr < rc) do
    begin
      pt := Result[group][curr];
      range := tree.RangeQuery(pt, dist,dist, True);
      n := Length(range);
      if n > 0 then
      begin
        tmp := Result[group];
        Move(range[0], tmp[rc], n*SizeOf(TPoint));
        Inc(rc, n);
      end;
      Inc(curr);
    end;
    SetLength(Result[group], rc);
    Inc(group);
  end;
  SetLength(result, group);
  Tree.Free();
end; 



//---------- Broken without fastAppr
function InEllipse(constref pt,center:TPoint; yrad, xrad, sqxy: Single): Boolean; Inline;
var x,y: Int32;
begin
  x := pt.x - center.x;
  y := pt.y - center.y;
  Result := (Sqr(X)*yrad)+(Sqr(Y)*xrad) <= sqxy;
end; 

function ApprCluster(pts:TPointArray; xrad,yrad:Int32; fastAppr:Boolean): T2DPointArray; cdecl;
var
  size,count,i,j:Int32;
  n,wid,hei,idx,len:Int32;
  temp,adjArr:TPointArray;
  groups:T2DPointArray;
  stack,means:TPointList;
  id,adj,curr,mean,gmean:TPoint;
  B:TBox;
  sqrx,sqry,sqxy:Single;
begin
  pts := Copy(pts);
  B := TPABounds(pts);
  OffsetTPA(PTS, -B.x1,-B.y1);
  groups := TPAPartition(pts, xrad,yrad);

  wid := Ceil((B.x2-B.x1+1) / xrad);
  hei := Ceil((B.y2-B.y1+1) / yrad);
  len := Length(pts);
  sqrx := Sqr(xrad*Sqrt(2));
  sqry := Sqr(yrad*Sqrt(2));
  sqxy := sqrx*sqry;
  
  count := 0;
  SetLength(temp,len);
  SetLength(adjArr, 8);
  Stack.Init();
  Means.Init();
  for i:=0 to len-1 do
  begin
    id.x := pts[i].x div xrad;
    id.y := pts[i].y div yrad;
    idx := id.y*wid+id.x;
    size := Length(Groups[idx]);
    if (size = 0) then continue;

    Move(groups[idx][0], temp[0], size*SizeOf(TPoint));
    Stack.Append(id);
    if not fastAppr then Means.Append(TPACenter(groups[idx], ECA_Mean));
    SetLength(Groups[idx], 0);
    
    repeat
      if not fastAppr then mean := means.FastPop();
      curr := stack.FastPop();

      GetAdjacent(adjArr,curr,True);
      for j:=0 to 7 do
      begin
        adj := adjArr[j];
        if InRange(adj.x, 0, wid-1) and InRange(adj.y, 0, hei-1) then
        begin
          idx := adj.y*wid+adj.x;
          n := Length(groups[idx]);
          if (n > 0) then
          begin
            if fastAppr then
            begin
              Stack.Append(adj);
              Move(groups[idx][0], temp[size], n*SizeOf(TPoint));
              SetLength(groups[idx], 0);
              Inc(size, n);
            end else
            begin
              gmean := TPACenter(groups[idx], ECA_Mean);
              if InEllipse(gmean, mean, sqrx, sqry, sqxy) then
              begin
                Stack.Append(adj);
                Means.Append(gmean);

                Move(groups[idx][0], temp[size], n*SizeOf(TPoint));
                SetLength(groups[idx], 0);
                Inc(size, n);
                SetLength(groups[idx], 0);
              end;
            end;
          end;
        end;
      end;
    until stack.IsEmpty();

    SetLength(result, count+1);
    result[count] := temp;
    SetLength(result[count], size);
    OffsetTPA(result[count], B.x1,B.y1);
    Inc(count);
  end;
end; 
  
end.
