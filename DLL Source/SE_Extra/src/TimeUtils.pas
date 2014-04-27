unit TimeUtils;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
{$mode objfpc}{$H+}
{$macro on}
{$inline on}
interface

uses SysUtils;

function EpochToTime(UnixTime: Double): TDateTime; cdecl;
function TimeSinceEpoch(OffsetSec:UInt32=0): Double; cdecl;
function FormatEpochTime(FormatString:String; UnixTime: Double): String; cdecl;
function EpochTimeToSysTime(UnixTime: Double): TSystemTime; cdecl;

//------------------------------------------------------------------------------
implementation

uses
  DateUtils, Math;

(* "Date math" and whatnots ***************************************************)

function Modulo(X,Y:Double): Double; Inline;
begin
  Result := X - Floor(X / Y) * Y;
end;

function DaysInMonth(Month, Year: UInt32): UInt32; Inline;
var
  IsLeap: Boolean;
begin
  IsLeap := (year mod 4=0) and ((year mod 100 <> 0) or (year mod 400 = 0));
  if (month = 2) then
    if IsLeap then Exit(29)
    else Exit(28);
  if Month in [4,6,9,11] then
    Exit(30);
  Exit(31);
end;

function DaysInYear(Year: UInt32): UInt32; Inline;
var
  IsLeap: Boolean;
begin
  Result := 365;
  IsLeap := (year mod 4=0) and ((year mod 100 <> 0) or (year mod 400 = 0));
  if IsLeap then
    Result := 366;
end;

function ReduceDaysToYear(var D: Double): Int32; Inline;
begin
  Result := 1970;
  while D > DaysInYear(Result) do
  begin
    D := D - DaysInYear(Result);
    Inc(Result);
  end;
end;


function ReduceDaysToMonths(var D: Double; Year: UInt32): UInt32; Inline;
begin
  Result := 1;
  while D > DaysInMonth(Result,Year) do
  begin
    D := D - DaysInMonth(Result,Year);
    Inc(Result);
  end;
end;


(* Functions exported *********************************************************)

// Converts epoch time to TDateTime
// A "bit" overvomplicated, but I don't see how it matters.
function EpochToTime(UnixTime: Double): TDateTime; cdecl;
var
  Ms, Sec, Min, Hour, Year, Month, Day: UInt32;
begin
  Ms  := Trunc(Frac(UnixTime)*1000);
  Sec := Trunc(Modulo(UnixTime, 60));
  UnixTime := UnixTime / 60;
  Min := Trunc(Modulo(UnixTime, 60));
  UnixTime := UnixTime / 60;
  Hour := Trunc(Modulo(UnixTime, 24));
  UnixTime := UnixTime / 24;

  Year := ReduceDaysToYear(UnixTime);
  Month := ReduceDaysToMonths(UnixTime, Year);
  Day := Trunc(UnixTime) + 1;
  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, Ms);
end;


// Computes the time since the unix epoch (01/01/1970)
function TimeSinceEpoch(OffsetSec:UInt32=0): Double; cdecl;
const 
  Epoch: TDateTime = 25569.0;
begin
  Result := (Now() - Epoch) * (60*60*24) - OffsetSec;
end;


// Cool and simple string formating of the give unix-UnixTime
function FormatEpochTime(FormatString:String; UnixTime: Double): String; cdecl;
begin
  Result := FormatDateTime(FormatString, EpochToTime(UnixTime));
end;


// UnitTime -> TSystemTime format
function EpochTimeToSysTime(UnixTime: Double): TSystemTime; cdecl;
var t:TDateTime; 
begin
  t := EpochToTime(UnixTime);
  DateTimeToSystemTime(t, Result);
end;






end.
