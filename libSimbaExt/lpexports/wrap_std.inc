{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrap all standard array methods (std.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(* Array slicing -------------------------------------------------------------*)
function exp_Slice1(var SE:Pointer; const Arr:TByteArray; Start,Stop,Step:Int64): TByteArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice2(var SE:Pointer; const Arr:TIntArray; Start,Stop,Step:Int64): TIntArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice3(var SE:Pointer; const Arr:TFloatArray; Start,Stop,Step:Int64): TFloatArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice4(var SE:Pointer; const Arr:TDoubleArray; Start,Stop,Step:Int64): TDoubleArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice5(var SE:Pointer; const Arr:TExtArray; Start,Stop,Step:Int64): TExtArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice6(var SE:Pointer; const Arr:TPointArray; Start,Stop,Step:Int64): TPointArray; cdecl;
begin  Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice7(var SE:Pointer; const Arr:TBoxArray; Start,Stop,Step:Int64): TBoxArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice8(var SE:Pointer; const Arr:String; Start,Stop,Step:Int64): String; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice9(var SE:Pointer; const Arr:T2DIntArray; Start,Stop,Step:Int64): T2DIntArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice10(var SE:Pointer; const Arr:T2DExtArray; Start,Stop,Step:Int64): T2DExtArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice11(var SE:Pointer; const Arr:T2DFloatArray; Start,Stop,Step:Int64): T2DFloatArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice12(var SE:Pointer; const Arr:T2DDoubleArray; Start,Stop,Step:Int64): T2DDoubleArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice13(var SE:Pointer; const Arr:T2DPointArray; Start,Stop,Step:Int64): T2DPointArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice14(var SE:Pointer; const Arr:T2DByteArray; Start,Stop,Step:Int64): T2DByteArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice15(var SE:Pointer; const Arr:T2DBoxArray; Start,Stop,Step:Int64): T2DBoxArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice16(var SE:Pointer; const Arr:TStringArray; Start,Stop,Step:Int64): TStringArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;



(* Find item in Array --------------------------------------------------------*)
function exp_FindEx_1(var SE:Pointer; const Arr, Seq:TByteArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_2(var SE:Pointer; const Arr, Seq:TIntArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_3(var SE:Pointer; const Arr, Seq:TFloatArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_4(var SE:Pointer; const Arr, Seq:TDoubleArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_5(var SE:Pointer; const Arr, Seq:TExtArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_6(var SE:Pointer; const Arr, Seq:TPointArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_7(var SE:Pointer; const Arr, Seq:TBoxArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_8(var SE:Pointer; const Arr, Seq:TStringArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_FindEx_9(var SE:Pointer; const Arr, Seq:String): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

(* *)
function exp_Find_1(var SE:Pointer; const Arr:TByteArray; Item:Byte): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_2(var SE:Pointer; const Arr:TIntArray; Item:Int32): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_3(var SE:Pointer; const Arr:TFloatArray; Item:Single): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_4(var SE:Pointer; const Arr:TDoubleArray; Item:Double): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_5(var SE:Pointer; const Arr:TExtArray; Item:Extended): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_6(var SE:Pointer; const Arr:TPointArray; Item:TPoint): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_7(var SE:Pointer; const Arr:TBoxArray; Item:TBox): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_8(var SE:Pointer; const Arr:TStringArray; Item:String): Int32; cdecl;
begin Result := Find(Arr,Item); end;

function exp_Find_9(var SE:Pointer; const Arr:String; Item:Char): Int32; cdecl;
begin Result := Find(Arr,Item); end;



(* Find all occurances of squence of items in array --------------------------*)
function exp_FindAllEx_1(var SE:Pointer; const Arr, Seq:TByteArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_2(var SE:Pointer; const Arr, Seq:TIntArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_3(var SE:Pointer; const Arr, Seq:TFloatArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_4(var SE:Pointer; const Arr, Seq:TDoubleArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_5(var SE:Pointer; const Arr, Seq:TExtArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_6(var SE:Pointer; const Arr, Seq:TPointArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_7(var SE:Pointer; const Arr, Seq:TBoxArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_8(var SE:Pointer; const Arr, Seq:TStringArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAllEx_9(var SE:Pointer; const Arr, Seq:String): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;


(* *)
function exp_FindAll_1(var SE:Pointer; const Arr:TByteArray; Item:Byte): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_2(var SE:Pointer; const Arr:TIntArray; Item:Int32): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_3(var SE:Pointer; const Arr:TFloatArray; Item:Single): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_4(var SE:Pointer; const Arr:TDoubleArray; Item:Double): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_5(var SE:Pointer; const Arr:TExtArray; Item:Extended): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_6(var SE:Pointer; const Arr:TPointArray; Item:TPoint): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_7(var SE:Pointer; const Arr:TBoxArray; Item:TBox): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_8(var SE:Pointer; const Arr:TStringArray; Item:String): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;

function exp_FindAll_9(var SE:Pointer; const Arr:String; Item:Char): TIntArray; cdecl;
begin Result := FindAll(Arr,Item); end;
