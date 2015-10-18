Unit Trees;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
{$modeswitch advancedrecords}

interface

uses
  CoreTypes, SysUtils;
 
type
  (*
    SlackTree is a static tree for quick "spatial lookups".
    Allows quick nearest neighbor lookup, and range-query. 
    
    Might add more features in the future, and do further optimizations.
  *)

  PSlackTree = ^TSlackTree;
  TSlackNodeArr = Array of PSlackTree;
  TSlackTree = packed record
    split: TPoint;
    l, r : PSlackTree;
    deleted: ByteBool;
    
    function AllocBranch(): PSlackTree; inline;
    procedure Init(TPA:TPointArray);
    procedure Free();
    
    function Find(p:TPoint; depth:Int32=0): PSlackTree;

    procedure Delete(); inline;
    function Delete(pt:TPoint): Boolean; overload;

    function Nearest_N(pt:TPoint; notEqual:Boolean=False): PSlackTree;
    function Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;

    function kNearest_N(pt:TPoint; k:Int32; notEqual:Boolean=False): TSlackNodeArr;
    function kNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TPointArray;

    function RangeQuery_N(B:TBox): TSlackNodeArr;
    function RangeQuery(B:TBox; remove:ByteBool=False): TPointArray;
    function RangeQuery(query:TPoint; xRad,yRad:Double; remove:ByteBool=False): TPointArray; overload;
  end;


procedure TSlackTree_Init(var root:TSlackTree; TPA:TPointArray); cdecl;
procedure TSlackTree_Free(var root:TSlackTree); cdecl;
function TSlackTree_Find(var root:TSlackTree; p:TPoint): PSlackTree; cdecl;
procedure TSlackTree_Delete(var root:TSlackTree); cdecl;
function TSlackTree_Delete2(var root:TSlackTree; pt:TPoint): Boolean; cdecl;
function TSlackTree_Nearest_N(var root:TSlackTree; pt:TPoint; notEqual:Boolean=False): PSlackTree; cdecl;
function TSlackTree_Nearest(var root:TSlackTree; pt:TPoint; notEqual:Boolean=False): TPoint; cdecl;
function TSlackTree_RangeQuery_N(var root:TSlackTree; B:TBox): TSlackNodeArr; cdecl;
function TSlackTree_RangeQuery(var root:TSlackTree; B:TBox; remove:ByteBool=False): TPointArray;  cdecl;
function TSlackTree_RangeQuery2(var root:TSlackTree; query:TPoint; xRad,yRad:Double; remove:ByteBool=False): TPointArray; cdecl;
  

//--------------------------------------------------
implementation
uses 
  Sorting, Math;


function TSlackTree.AllocBranch(): PSlackTree; inline;
begin
  Result := GetMem(SizeOf(TSlackTree));
  Result^.L := nil;
  Result^.R := nil;
  Result^.deleted := False;
end;

procedure TSlackTree.Init(TPA:TPointArray);
  procedure Build(var node:TSlackTree; var TPA:TPointArray; left, right:Int32; depth:UInt8=0);
  var mid: Int32;
  begin
    if (right-left < 0) then Exit();     // just back up..

    mid := (right+left) shr 1;
    node.split := TPASelectNth_Axis(TPA, mid, left, right, depth and 1);

    if mid-left > 0 then begin           //lower half
      node.L := node.AllocBranch();
      Build(node.L^, TPA, left,mid-1, depth+1);
    end;

    if right-mid > 0 then begin          //upper half
      node.R := node.AllocBranch();
      Build(node.R^, TPA, mid+1,right, depth+1);
    end;
  end;
begin
  Self.Deleted := False;
  Build(Self, TPA, 0, High(TPA)); 
end;


procedure TSlackTree.Free();
begin
  if Self.R <> nil then
  begin
    Self.R^.Free();
    FreeMem(Self.R);
  end;
  if Self.L <> nil then
  begin
    Self.L^.Free();
    FreeMem(Self.L);
  end;
end;

function TSlackTree.Find(p:TPoint; depth:Int32=0): PSlackTree;
begin
  if UInt64(self.Split) = UInt64(p) then
    Exit(@self);

  if (Depth and 1 = 0) then
    case self.Split.x > p.x of
      True:
        if self.L <> nil then Exit(self.L^.Find(p,depth+1));
      False:
        if self.R <> nil then Exit(self.R^.Find(p,depth+1));
    end
  else
    case self.Split.y > p.y of
      True:
        if self.L <> nil then Exit(self.L^.Find(p,depth+1));
      False:
        if self.R <> nil then Exit(self.R^.Find(p,depth+1));
    end;
  Result := nil;
end;

    
procedure TSlackTree.Delete();
begin
  Self.deleted := True;
end;

function TSlackTree.Delete(pt:TPoint): Boolean; overload;
var node:PSlackTree;
begin
  node := self.Find(pt);
  Result := node <> nil;
  if result then
    node^.deleted := True;
end;

function TSlackTree.Nearest_N(pt:TPoint; notEqual:Boolean=False): PSlackTree;
var
  resDist:Int32;
  resNode:PSlackTree;

  procedure __nearest(var root:TSlackTree; query:TPoint; depth:UInt8=0);
  var
    dist,delta:Int32;
    test:PSlackTree = nil;
  begin
    if depth and 1 = 0 then
      delta := root.split.x - query.x
    else
      delta := root.split.y - query.y;

    if not root.deleted then
    begin
      dist := Sqr(root.split.x - query.x) + Sqr(root.split.y - query.y);
      if (dist < resDist) and not((dist = 0) and notEqual) then
      begin
        resDist := dist;
        resNode := @root;
      end;
      if resDist = 0 then Exit();
    end;

    if delta > 0 then test := root.l else test := root.r;
    if (test <> nil) then
      __nearest(test^, query, depth+1);

    if (Sqr(delta) >= resDist) then Exit();

    if delta > 0 then test := root.r else test := root.l;
    if (test <> nil) then
      __nearest(test^, query, depth+1);
  end;

begin
  resDist := High(Int32);
  resNode := nil;
  __nearest(self, pt);
  Result := resNode;
end;


function TSlackTree.Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;
begin
  Result := self.Nearest_N(pt, notEqual)^.split;
end;


function TSlackTree.kNearest_N(pt:TPoint; k:Int32; notEqual:Boolean=False): TSlackNodeArr;
var i,c:Int32;
begin
  SetLength(Result, k);
  c := 0;
  while c < k do
  begin
     Result[c] := self.Nearest_N(pt, notEqual);
     if Result[c] = nil then break;
     Result[c]^.deleted := True;
     inc(c);
  end;

  for i:=0 to c do Result[i]^.deleted := False;
  SetLength(Result, c);
end;


function TSlackTree.kNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TPointArray;
var
  arr:TSlackNodeArr;
  i:Int32;
begin
  arr := kNearest_N(pt,k,notEqual);
  SetLength(result, length(arr));
  for i:=0 to High(arr) do result[i] := arr[i]^.split;
end;



function TSlackTree.RangeQuery_N(B:TBox): TSlackNodeArr;
var
  res_len, res_count: Int32;
  procedure __query(var root:TSlackTree; var query:TBox; var res:TSlackNodeArr; depth:Int32=0);
  var
    goright:ByteBool = False;
    goleft: ByteBool = False;
  begin
    if depth and 1 = 0 then begin
      goleft  := query.x1 <= root.split.x;
      goright := query.x2 >= root.split.x;
    end else begin
      goleft  := query.y1 <= root.split.y;
      goright := query.y2 >= root.split.y;
    end;

    if (not root.deleted) and (query.x1 <= root.split.x) and (query.x2 >= root.split.x) and
       (query.y1 <= root.split.y) and (query.y2 >= root.split.y) then
    begin
      res[res_count] := @root;
      inc(res_count);

      if res_count = res_len then
      begin
        res_len := res_len*2;
        Setlength(res, res_len);
      end;
    end;

    if goleft and (root.l <> nil) then
      __query(root.l^, query, res, depth+1);

    if goright and (root.r <> nil) then
      __query(root.r^, query, res, depth+1);
  end;
begin
  res_len := 1024;
  res_count := 0;
  SetLength(result, res_len);
  __query(self, B, result);
  SetLength(result, res_count);
end;


function TSlackTree.RangeQuery(B:TBox; remove:ByteBool=False): TPointArray;
var
  nodes:TSlackNodeArr;
  i:Int32;
begin
  nodes := self.RangeQuery_N(B);
  SetLength(Result, length(nodes));
  for i:=0 to High(nodes) do
  begin
    Result[i] := Nodes[i]^.split;
    if remove then Nodes[i]^.deleted := True;
  end;
end;


function TSlackTree.RangeQuery(query:TPoint; xRad,yRad:Double; remove:ByteBool=False): TPointArray; overload;
var
  nodes:TSlackNodeArr;
  i,c:Int32;
  SqX,SqY,mag:Double;
  pt:TPoint;
begin
  nodes := self.RangeQuery_N(
    Box(query.x-trunc(xRad), query.y-trunc(yRad), query.x+ceil(xRad), query.y+ceil(yRad))
  );
  SqX := Sqr(XRad);
  SqY := Sqr(YRad);
  mag := SqX*SqY;
  SetLength(Result, length(nodes));
  c := 0;
  if xRad <> yRad then
  begin
    for i:=0 to High(nodes) do
    begin
      pt := Nodes[i]^.split;
      if Sqr(pt.X-query.x) * SqY + Sqr(pt.Y-query.y) * SqX <= mag then
      begin
        Result[c] := pt;
        Inc(c);
        if remove then Nodes[i]^.deleted := True;
      end;
    end;
  end else
    for i:=0 to High(nodes) do
    begin
      pt := Nodes[i]^.split;
      if Sqr(pt.X-query.x) + Sqr(pt.Y-query.y) <= SqX then
      begin
        Result[c] := pt;
        Inc(c);
        if remove then Nodes[i]^.deleted := True;
      end;
    end;
  SetLength(Result, c);
end;
  
//---------| Export friendly methods |-------------------------------

procedure TSlackTree_Init(var root:TSlackTree; TPA:TPointArray); cdecl;
begin
 root.Init(TPA);
end;

procedure TSlackTree_Free(var root:TSlackTree); cdecl;
begin
  root.Free();
end;

function TSlackTree_Find(var root:TSlackTree; p:TPoint): PSlackTree; cdecl;
begin
  Result := root.Find(p);
end;

procedure TSlackTree_Delete(var root:TSlackTree); cdecl;
begin
  root.Delete();
end;

function TSlackTree_Delete2(var root:TSlackTree; pt:TPoint): Boolean; cdecl;
begin
  Result := root.Delete(pt);
end;

function TSlackTree_Nearest_N(var root:TSlackTree; pt:TPoint; notEqual:Boolean=False): PSlackTree; cdecl;
begin
  result := root.Nearest_N(pt, notEqual);
end;

function TSlackTree_Nearest(var root:TSlackTree; pt:TPoint; notEqual:Boolean=False): TPoint; cdecl;
begin
  result := root.Nearest(pt, notEqual);
end;

function TSlackTree_RangeQuery_N(var root:TSlackTree; B:TBox): TSlackNodeArr; cdecl;
begin
  Result := root.RangeQuery_N(B);
end;

function TSlackTree_RangeQuery(var root:TSlackTree; B:TBox; remove:ByteBool=False): TPointArray; cdecl;
begin
  Result := root.RangeQuery(B, remove);
end;

function TSlackTree_RangeQuery2(var root:TSlackTree; query:TPoint; xRad,yRad:double; remove:ByteBool=False): TPointArray; cdecl;
begin
  Result := root.RangeQuery(query, xRad,yRad, remove);
end;
  
end.
