unit SimpleHeap;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
interface

uses CoreTypes, SysUtils, Variants;

type
  THeapItem = record Value:Variant; index:Int32; end;
  THeapArray = Array of THeapItem;
  
  THeapItemI = record Value:Int32; index:Int32; end;
  THeapArrayI = Array of THeapItemI;
  
  THeapItemF = record Value:Single; index:Int32; end;
  THeapArrayF = Array of THeapItemF;
  
  THeapItemD = record Value:Double; index:Int32; end;
  THeapArrayD = Array of THeapItemD;
  

(* heap using variant, for general usage *)
procedure hPush(var h:THeapArray; item:Variant; idx:Int32; HiLo:Boolean=True); Inline;
function hPop(var h:THeapArray; HiLo:Boolean=True): THeapItem; Inline;

(* Specialized heap using Integer *)
procedure hPush(var h:THeapArrayI; item:Int32; idx:Int32; HiLo:Boolean=True); Inline; overload;
function hPop(var h:THeapArrayI; HiLo:Boolean=True): THeapItemI; Inline; overload;

(* Specialized heap using Single *)
procedure hPush(var h:THeapArrayF; item:Single; idx:Int32; HiLo:Boolean=True); Inline; overload;
function hPop(var h:THeapArrayF; HiLo:Boolean=True): THeapItemF; Inline; overload;

(* Specialized heap using Double *)
procedure hPush(var h:THeapArrayD; item:Double; idx:Int32; HiLo:Boolean=True); Inline; overload;
function hPop(var h:THeapArrayD; HiLo:Boolean=True): THeapItemD; Inline; overload;



//----------------------------------------------------------------------------\\
implementation


//----------------------------------------------------------------------------\\


//---| Variant |--------------------------------------------------------------->


procedure _movedownHI(var heap:THeapArray; startpos, pos:Int32); Inline;
var 
  parentpos: Int32;
  parent,newitem:THeapItem;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value < parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;

procedure _movedownLO(var heap:THeapArray; startpos, pos:Int32); Inline;
var 
  parentpos: Int32;
  parent,newitem:THeapItem;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value > parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupHI(var heap:THeapArray; pos:Int32); Inline;
var
  endpos,startpos,childpos,rightpos:Int32;
  newitem: THeapItem;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value < heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownHI(heap, startpos, pos)
end;


procedure _moveupLO(var heap:THeapArray; pos:Int32); Inline;
var
  endpos,startpos,childpos,rightpos:Int32;
  newitem: THeapItem;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value > heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownLO(heap, startpos, pos)
end;



procedure hPush(var h:THeapArray; item:Variant; idx:Int32; HiLo:Boolean=True); Inline;
var hi:Int32;
begin
  hi := Length(h);
  SetLength(h,hi+1);
   
  h[hi].value := item;
  h[hi].index := idx;
  case HiLo of
    True: _movedownHI(h, 0, hi);
    False:_movedownLO(h, 0, hi);
  end;
end;


function hPop(var h:THeapArray; HiLo:Boolean=True): THeapItem; Inline;
var m:THeapItem;
begin
  m := h[High(h)];
  SetLength(h, high(h));
  if (High(h) >= 0) then begin
    Result := h[0];
    h[0] := m;
    case HiLo of
      True: _moveupHI(h, 0);
      False:_moveupLO(h, 0);
    end;
  end else
    Exit(m);
end; 



//----------------------------------------------------------------------------\\


//---| Integer |--------------------------------------------------------------->


procedure _movedownHI(var heap:THeapArrayI; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemI;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value < parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupHI(var heap:THeapArrayI; pos:Int32); Inline; overload;
var endpos,startpos,childpos,rightpos:Int32;
    newitem: THeapItemI;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value < heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownHI(heap, startpos, pos)
end;


procedure _movedownLO(var heap:THeapArrayI; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemI;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value > parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupLO(var heap:THeapArrayI; pos:Int32); Inline; overload;
var
  endpos,startpos,childpos,rightpos:Int32;
  newitem: THeapItemI;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value > heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownLO(heap, startpos, pos)
end;


procedure hPush(var h:THeapArrayI; item:Int32; idx:Int32; HiLo:Boolean=True); Inline; overload;
var hi:Int32;
begin
  hi := Length(h);
  SetLength(h,hi+1);
   
  h[hi].value := item;
  h[hi].index := idx;
  case HiLo of
    True: _movedownHI(h, 0, hi);
    False:_movedownLO(h, 0, hi);
  end;
end;


function hPop(var h:THeapArrayI; HiLo:Boolean=True): THeapItemI; Inline; overload;
var m:THeapItemI;
begin
  m := h[High(h)];
  SetLength(h, high(h));
  if (High(h) >= 0) then begin
    Result := h[0];
    h[0] := m;
    case HiLo of
      True: _moveupHI(h, 0);
      False:_moveupLO(h, 0);
    end;
  end else
    Exit(m);
end;



//----------------------------------------------------------------------------\\


//---| Single |---------------------------------------------------------------->


procedure _movedownHI(var heap:THeapArrayF; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemF;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value < parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupHI(var heap:THeapArrayF; pos:Int32); Inline; overload;
var
  endpos,startpos,childpos,rightpos:Int32;
  newitem: THeapItemF;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value < heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownHI(heap, startpos, pos)
end;


procedure _movedownLO(var heap:THeapArrayF; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemF;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value > parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupLO(var heap:THeapArrayF; pos:Int32); Inline; overload;
var endpos,startpos,childpos,rightpos:Int32;
    newitem: THeapItemF;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value > heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownLO(heap, startpos, pos)
end;


procedure hPush(var h:THeapArrayF; item:Single; idx:Int32; HiLo:Boolean=True); Inline; overload;
var hi:Int32;
begin
  hi := Length(h);
  SetLength(h,hi+1);
   
  h[hi].value := item;
  h[hi].index := idx;
  case HiLo of
    True: _movedownHI(h, 0, hi);
    False:_movedownLO(h, 0, hi);
  end;
end;


function hPop(var h:THeapArrayF; HiLo:Boolean=True): THeapItemF; Inline; overload;
var m:THeapItemF;
begin
  m := h[High(h)];
  SetLength(h, high(h));
  if (High(h) >= 0) then begin
    Result := h[0];
    h[0] := m;
    case HiLo of
      True: _moveupHI(h, 0);
      False:_moveupLO(h, 0);
    end;
  end else
    Exit(m);
end; 



//----------------------------------------------------------------------------\\


//---| Double |---------------------------------------------------------------->


procedure _movedownHI(var heap:THeapArrayD; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemD;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value < parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupHI(var heap:THeapArrayD; pos:Int32); Inline; overload;
var endpos,startpos,childpos,rightpos:Int32;
    newitem: THeapItemD;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value < heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownHI(heap, startpos, pos)
end;


procedure _movedownLO(var heap:THeapArrayD; startpos, pos:Int32); Inline; overload;
var 
  parentpos: Int32;
  parent,newitem:THeapItemD;
begin
  newitem := heap[pos];
  while (pos > startpos) do begin
    parentpos := (pos - 1) shr 1;
    parent := heap[parentpos];
    if (newitem.value > parent.value) then
    begin
      heap[pos] := parent;
      pos := parentpos;
      continue;
    end;
    break;
  end;
  heap[pos] := newitem;
end;


procedure _moveupLO(var heap:THeapArrayD; pos:Int32); Inline; overload;
var endpos,startpos,childpos,rightpos:Int32;
    newitem: THeapItemD;
begin
  endpos := length(heap);
  startpos := pos;
  newitem := heap[pos];

  childpos := 2 * pos + 1;
  while childpos < endpos do begin
      rightpos := childpos + 1;
      if (rightpos < endpos) and not(heap[childpos].value > heap[rightpos].value) then
          childpos := rightpos;
      heap[pos] := heap[childpos];
      pos := childpos;
      childpos := 2 * pos + 1;
  end;
  heap[pos] := newitem;
  _movedownLO(heap, startpos, pos)
end;


procedure hPush(var h:THeapArrayD; item:Double; idx:Int32; HiLo:Boolean=True); Inline; overload;
var hi:Int32;
begin
  hi := Length(h);
  SetLength(h,hi+1);
   
  h[hi].value := item;
  h[hi].index := idx;
  case HiLo of
    True: _movedownHI(h, 0, hi);
    False:_movedownLO(h, 0, hi);
  end;
end;


function hPop(var h:THeapArrayD; HiLo:Boolean=True): THeapItemD; Inline; overload;
var m:THeapItemD;
begin
  m := h[High(h)];
  SetLength(h, high(h));
  if (High(h) >= 0) then begin
    Result := h[0];
    h[0] := m;
    case HiLo of
      True: _moveupHI(h, 0);
      False:_moveupLO(h, 0);
    end;
  end else
    Exit(m);
end; 






end.

