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
{$rangechecks on}

interface

uses
  CoreTypes, SysUtils;
 
type
  (*
    SlackTree is a static tree for quick "spatial lookups".
    Allows quick nearest neighbor lookup, and range-query. 
    
    Might add more features in the future, and do further optimizations.

    Note: Might still exists some bugs here.
  *)

  
  PNode = ^TNode;
  TNode = packed record
    split: TPoint;
    l,r: Int32;
    deleted: Boolean;
  end;
  
  TNodeArray = Array of TNode;
  TNodeRefArray = Array of PNode;
  
  TSlackTree = packed record
  public
    data: TNodeArray;
    size: Int32;
  private
    function GetItem(i:Int32): PNode; inline;
  public
    property items[i:Int32]: PNode read GetItem; default;

    function InitBranch(): Int32; inline; //dummy

    procedure Init(var TPA:TPointArray);
    procedure Free();

    function IndexOf(p:TPoint): Int32;
    function Find(p:TPoint): PNode;

    procedure Delete(idx:Int32);
    function Delete(pt:TPoint): Boolean; overload;

    function Nearest_N(pt:TPoint; notEqual:Boolean=False): PNode;
    function Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;

    function kNearest_N(pt:TPoint; k:Int32; notEqual:Boolean=False): TNodeRefArray;
    function kNearest(pt:TPoint; k:Int32; notEqual:Boolean=False): TPointArray;

    function RangeQuery_N(B:TBox): TNodeRefArray;
    function RangeQuery(B:TBox; remove:Boolean=False): TPointArray;
    function RangeQuery(query:TPoint; xRad,yRad:Double; remove:Boolean=False): TPointArray; overload;

    function RefArray: TNodeRefArray;
  end;



procedure TSlackTree_Init(var tree:TSlackTree; var TPA:TPointArray); cdecl;
procedure TSlackTree_Free(var tree:TSlackTree); cdecl;
function TSlackTree_IndexOf(var tree:TSlackTree; p:TPoint): Int32; cdecl;
function TSlackTree_Find(var tree:TSlackTree; p:TPoint): PNode; cdecl;
procedure TSlackTree_Delete(var tree:TSlackTree; idx:Int32); cdecl;
function TSlackTree_Delete2(var tree:TSlackTree; pt:TPoint): LongBool; cdecl;
function TSlackTree_Nearest_N(var tree:TSlackTree; pt:TPoint; notEqual:LongBool=False): PNode; cdecl;
function TSlackTree_Nearest(var tree:TSlackTree; pt:TPoint; notEqual:LongBool=False): TPoint; cdecl;
function TSlackTree_RangeQuery_N(var tree:TSlackTree; B:TBox): TNodeRefArray; cdecl;
function TSlackTree_RangeQuery(var tree:TSlackTree; B:TBox; remove:LongBool=False): TPointArray;  cdecl;
function TSlackTree_RangeQuery2(var tree:TSlackTree; query:TPoint; xRad,yRad:Double; remove:LongBool=False): TPointArray; cdecl;
function TSlackTree_RefArray(var tree:TSlackTree): TNodeRefArray; cdecl;

const
  NONE = -1;

//--------------------------------------------------
implementation
uses 
  Sorting, Math;


function TSlackTree.RefArray: TNodeRefArray;
var
  i:Int32;
begin
  SetLength(Result, Length(self.data));
  for i:=0 to High(self.data) do Result[i] := @self.data[i];
end;


function TSlackTree.GetItem(i:Int32): PNode; 
begin
  Result := @self.data[i];
end;


function TSlackTree.InitBranch(): Int32;
begin
  Result := self.size;
  with self.data[result] do
  begin
    L := -1;
    R := -1;
    deleted := False;
  end;
  Inc(self.size);
end;


procedure TSlackTree.Init(var TPA:TPointArray);
  procedure __build(var node:TNode; left, right:Int32; depth:Int32=0);
  var mid: Int32;
  begin
    if (right-left < 0) then Exit(); // just nil back up..

    mid := (right+left) shr 1;
    node.split := TPASelectNth_Axis(TPA, mid, left, right, depth and 1);

    if mid-left > 0 then begin           //lower half
      node.L := self.InitBranch();
      __build(self.data[node.L], left,mid-1, depth+1);
    end;

    if right-mid > 0 then begin          //upper half
      node.R := self.InitBranch();
      __build(self.data[node.R], mid+1,right, depth+1);
    end;
  end;
begin
  SetLength(self.data, Length(TPA));
  __build(self.data[InitBranch()], 0, High(TPA));
end;


procedure TSlackTree.Free();
begin
  SetLength(self.data, 0);
  Self.Size := 0;
end;


function TSlackTree.IndexOf(p:TPoint): Int32;
  function __find(idx:Int32; depth:Int32=0): Int32;
  var 
    s:Int32;
    this:PNode;
  begin
    this := Self[idx];
    if (this^.split = p) then 
      Exit(idx);
      
    Result := NONE;
    if (depth and 1 = 0) then
      s := sign(this^.split.x - p.x)
    else
      s := sign(this^.split.y - p.y);

    case s of
       1: if this^.L <> NONE then Exit(__find(this^.L, depth+1));
      -1: if this^.R <> NONE then Exit(__find(this^.R, depth+1));
       0: begin
            if this^.R <> NONE then Result := __find(this^.R, depth+1);
            if (Result = NONE) and (this^.L <> NONE) then Result := __find(this^.L, depth+1);
            Exit();
          end;
    end;
  end;
begin
  Result := __find(0,0);
end;

function TSlackTree.Find(p:TPoint): PNode;
var i:Int32;
begin
  Result := nil;
  i := Self.IndexOf(p);
  if i <> None then Result := @self.data[i];
end;
    
procedure TSlackTree.Delete(idx:Int32);
begin
  Self[idx]^.deleted := True;
end;

function TSlackTree.Delete(pt:TPoint): Boolean; overload;
var idx:Int32;
begin
  idx := self.IndexOf(pt);
  Result := idx <> None;
  if result then
    self.data[idx].deleted := True;
end;

function TSlackTree.Nearest_N(pt:TPoint; notEqual:Boolean=False): PNode;
var
  resDist:Int32;
  resNode:PNode;
  procedure __nearest(node:Int32; depth:UInt8=0);
  var
    test,dist,delta:Int32;
    this:PNode;
  begin
    this := @self.data[node];
    
    if depth and 1 = 0 then
      delta := this^.split.x - pt.x
    else
      delta := this^.split.y - pt.y;

    if not this^.deleted then
    begin
      dist := Sqr(this^.split.x - pt.x) + Sqr(this^.split.y - pt.y);
      if (dist < resDist) and not((dist = 0) and notEqual) then
      begin
        resDist := dist;
        resNode := this;
      end;
      if resDist = 0 then Exit();
    end;

    if delta > 0 then test := this^.l else test := this^.r;
    if (test <> None) then
      __nearest(test, depth+1);

    if (Sqr(delta) >= resDist) then Exit();

    if delta > 0 then test := this^.r else test := this^.l;
    if (test <> None) then
      __nearest(test, depth+1);
  end;

begin
  resDist := High(Int32);
  resNode := nil;
  __nearest(0);
  Result := resNode;
end;


function TSlackTree.Nearest(pt:TPoint; notEqual:Boolean=False): TPoint;
var tmp:PNode;
begin
  tmp := self.Nearest_N(pt, notEqual);
  if tmp <> nil then
    Result := tmp^.split
  else
    Result := Point(-1,-1);
end;


function TSlackTree.kNearest_N(pt:TPoint; k:Int32; notEqual:Boolean=False): TNodeRefArray;
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
  arr:TNodeRefArray;
  i:Int32;
begin
  arr := kNearest_N(pt,k,notEqual);
  SetLength(result, Length(arr));
  for i:=0 to High(arr) do result[i] := arr[i]^.split;
end;



function TSlackTree.RangeQuery_N(B:TBox): TNodeRefArray;
var
  res_len, res_count: Int32;
  procedure __query(node:Int32; var res:TNodeRefArray; depth:Int32=0);
  var
    goright:ByteBool = False;
    goleft: ByteBool = False;
    this:PNode;
  begin
    this := @self.data[node];
    if depth and 1 = 0 then begin
      goleft  := B.x1 <= this^.split.x;
      goright := B.x2 >= this^.split.x;
    end else begin
      goleft  := B.y1 <= this^.split.y;
      goright := B.y2 >= this^.split.y;
    end;

    if (not this^.deleted) and
       (B.x1 <= this^.split.x) and (B.x2 >= this^.split.x) and
       (B.y1 <= this^.split.y) and (B.y2 >= this^.split.y) then
    begin
      res[res_count] := this;
      inc(res_count);

      if res_count = res_len then
      begin
        res_len := res_len*2;
        Setlength(res, res_len);
      end;
    end;

    if goleft and (this^.l <> None) then
      __query(this^.l, res, depth+1);

    if goright and (this^.r <> None) then
      __query(this^.r, res, depth+1);
  end;
begin
  res_len := 1024;
  res_count := 0;
  SetLength(result, res_len);
  __query(0, result);
  SetLength(result, res_count);
end;


function TSlackTree.RangeQuery(B:TBox; remove:Boolean=False): TPointArray;
var
  nodes:TNodeRefArray;
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


function TSlackTree.RangeQuery(query:TPoint; xRad,yRad:Double; remove:Boolean=False): TPointArray; overload;
var
  i,c:Int32;
  nodes:TNodeRefArray;
  SqX,SqY,mag:Single;
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
      if Sqr(pt.x-query.x) + Sqr(pt.y-query.y) <= SqX then
      begin
        Result[c] := pt;
        Inc(c);
        if remove then Nodes[i]^.deleted := True;
      end;
    end;
  SetLength(Result, c);
end;

  
  
  
//---------| Export friendly methods |-------------------------------

procedure TSlackTree_Init(var tree:TSlackTree; var TPA:TPointArray); cdecl;
begin
 tree.Init(TPA);
end;

procedure TSlackTree_Free(var tree:TSlackTree); cdecl;
begin
  tree.Free();
end;

function TSlackTree_IndexOf(var tree:TSlackTree; p:TPoint): Int32; cdecl;
begin
  Result := tree.IndexOf(p);
end;

function TSlackTree_Find(var tree:TSlackTree; p:TPoint): PNode; cdecl;
begin
  Result := tree.Find(p);
end;

procedure TSlackTree_Delete(var tree:TSlackTree; idx:Int32); cdecl;
begin
  tree.Delete(idx);
end;

function TSlackTree_Delete2(var tree:TSlackTree; pt:TPoint): LongBool; cdecl;
begin
  Result := tree.Delete(pt);
end;

function TSlackTree_Nearest_N(var tree:TSlackTree; pt:TPoint; notEqual:LongBool=False): PNode; cdecl;
begin
  result := tree.Nearest_N(pt, notEqual);
end;

function TSlackTree_Nearest(var tree:TSlackTree; pt:TPoint; notEqual:LongBool=False): TPoint; cdecl;
begin
  result := tree.Nearest(pt, notEqual);
end;

function TSlackTree_RangeQuery_N(var tree:TSlackTree; B:TBox): TNodeRefArray; cdecl;
begin
  Result := tree.RangeQuery_N(B);
end;

function TSlackTree_RangeQuery(var tree:TSlackTree; B:TBox; remove:LongBool=False): TPointArray; cdecl;
begin
  Result := tree.RangeQuery(B, remove);
end;

function TSlackTree_RangeQuery2(var tree:TSlackTree; query:TPoint; xRad,yRad:double; remove:LongBool=False): TPointArray; cdecl;
begin
  Result := tree.RangeQuery(query, xRad,yRad, remove);
end;

function TSlackTree_RefArray(var tree:TSlackTree): TNodeRefArray; cdecl;
begin
  Result := tree.RefArray();
end;
  
end.
