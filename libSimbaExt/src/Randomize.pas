Unit Randomize;
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
  Math, SysUtils, CoreTypes;
  
function RandomTPA(amount:Int32; MinX,MinY,MaxX,MaxY:Int32): TPointArray;
function RandomCenterTPA(amount:Int32; CX,CY,RadX,RadY:Int32): TPointArray;
function RandomTIA(amount:Int32; Low,Hi:Int32): TIntArray;


//--------------------------------------------------
implementation


(*
  Simple random tpa, with some extra parameters compared to what SCAR offers.
*)
function RandomTPA(Amount:Int32; MinX,MinY,MaxX,MaxY:Int32): TPointArray;
var i:Integer;
begin
  SetLength(Result, Amount);
  for i:=0 to Amount-1 do
    Result[i] := Point(RandomRange(MinX, MaxX), RandomRange(MinY, MaxY));
end; 


(*
  TPA with a "gravity" that goes towards the mean (center). 
  Similar to gaussian distribution of the TPoints.
*)
function RandomCenterTPA(Amount:Int32; CX,CY,RadX,RadY:Int32): TPointArray;
var
  i:Integer;
  x,y,xstep,ystep: Single;
begin
  SetLength(Result, Amount);
  xstep := RadX / Amount;
  ystep := RadY / Amount;
  x:=0; y:=0;
  for i:=0 to Amount-1 do begin
    x := x + xstep;
    y := y + ystep;
    Result[i].x := RandomRange(Round(CX-x), Round(CX+x));
    Result[i].y := RandomRange(Round(CY-y), Round(CY+y));
  end;
end;


(*
  Simple random TIA, with some extra parameters compared to what SCAR offers.
*)
function RandomTIA(amount:Int32; low,hi:Int32): TIntArray;
var i:Integer;
begin
  SetLength(Result, Amount);
  for i:=0 to Amount-1 do
    Result[i] := RandomRange(Low,Hi);
end; 


end.
