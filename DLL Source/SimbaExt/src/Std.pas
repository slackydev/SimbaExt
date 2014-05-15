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
function Slice(Arr:TPointArray; Start,Stop:Int32; Step:Int32=1): TPointArray; overload;
function Slice(Arr:TByteArray; Start,Stop:Int32; Step:Int32=1): TByteArray; overload;
function Slice(Arr:TBoxArray; Start,Stop:Int32; Step:Int32=1): TBoxArray; overload;
function Slice(Arr:String; Start,Stop:Int32; Step:Int32=1): String; overload;

function Slice(Arr:T2DIntArray; Start,Stop:Int32; Step:Int32=1): T2DIntArray; overload;
function Slice(Arr:T2DExtArray; Start,Stop:Int32; Step:Int32=1): T2DExtArray; overload;
function Slice(Arr:T2DPointArray; Start,Stop:Int32; Step:Int32=1): T2DPointArray; overload;
function Slice(Arr:T2DByteArray; Start,Stop:Int32; Step:Int32=1): T2DByteArray; overload;
function Slice(Arr:T2DBoxArray; Start,Stop:Int32; Step:Int32=1): T2DBoxArray; overload;


//--------------------------------------------------
implementation
uses CoreMath;

{$I std/ArrSlice.pas}

end.
