Unit Std;
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
  SysUtils,CoreTypes;

function Slice(Arr:TIntArray; Start,Stop:Int32; Step:Int32=1): TIntArray; overload; 
function Slice(Arr:TExtArray; Start,Stop:Int32; Step:Int32=1): TExtArray; overload;
function Slice(Arr:TFloatArray; Start,Stop:Int32; Step:Int32=1): TFloatArray; overload;
function Slice(Arr:TDoubleArray; Start,Stop:Int32; Step:Int32=1): TDoubleArray; overload; 
function Slice(Arr:TPointArray; Start,Stop:Int32; Step:Int32=1): TPointArray; overload; 
function Slice(Arr:TByteArray; Start,Stop:Int32; Step:Int32=1): TByteArray; overload; 
function Slice(Arr:TBoxArray; Start,Stop:Int32; Step:Int32=1): TBoxArray; overload; 
function Slice(Arr:String; Start,Stop:Int32; Step:Int32=1): String; overload; 

function Slice(Arr:T2DIntArray; Start,Stop:Int32; Step:Int32=1): T2DIntArray; overload; 
function Slice(Arr:T2DExtArray; Start,Stop:Int32; Step:Int32=1): T2DExtArray; overload; 
function Slice(Arr:T2DFloatArray; Start,Stop:Int32; Step:Int32=1): T2DFloatArray; overload; 
function Slice(Arr:T2DDoubleArray; Start,Stop:Int32; Step:Int32=1): T2DDoubleArray; overload; 
function Slice(Arr:T2DPointArray; Start,Stop:Int32; Step:Int32=1): T2DPointArray; overload; 
function Slice(Arr:T2DByteArray; Start,Stop:Int32; Step:Int32=1): T2DByteArray; overload; 
function Slice(Arr:T2DBoxArray; Start,Stop:Int32; Step:Int32=1): T2DBoxArray; overload; 
function Slice(Arr:TStringArray; Start,Stop:Int32; Step:Int32=1): TStringArray; overload; 


(* *)
function Find(Arr:TIntArray; Seq:TIntArray): Int32; overload; 
function Find(Arr:TExtArray; Seq:TExtArray): Int32; overload; 
function Find(Arr:TFloatArray; Seq:TFloatArray): Int32; overload; 
function Find(Arr:TDoubleArray; Seq:TDoubleArray): Int32; overload; 
function Find(Arr:TPointArray; Seq:TPointArray): Int32; overload; 
function Find(Arr:TByteArray; Seq:TByteArray): Int32; overload; 
function Find(Arr:TBoxArray; Seq:TBoxArray): Int32; overload; 
function Find(Arr:String; Seq:String): Int32; overload; 


(* *)
function FindAll(Arr:TIntArray; Seq:TIntArray): TIntArray; overload; 
function FindAll(Arr:TExtArray; Seq:TExtArray): TIntArray; overload; 
function FindAll(Arr:TFloatArray; Seq:TFloatArray): TIntArray; overload; 
function FindAll(Arr:TDoubleArray; Seq:TDoubleArray): TIntArray; overload; 
function FindAll(Arr:TPointArray; Seq:TPointArray): TIntArray; overload; 
function FindAll(Arr:TByteArray; Seq:TByteArray): TIntArray; overload; 
function FindAll(Arr:TBoxArray; Seq:TBoxArray): TIntArray; overload; 
function FindAll(Arr:String; Seq:String): TIntArray; overload;


//--------------------------------------------------
implementation
uses CoreMath;

{$I std/ArrSlice.pas}
{$I std/ArrFind.pas}
{$I std/ArrFindAll.pas}

end.
