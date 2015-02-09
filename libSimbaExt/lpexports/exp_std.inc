{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports standard array methods (std.pas)
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
(** Array slicing **)
AddMethod(@exp_Slice1,  'function SimbaExt.Slice(const Arr:TByteArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TByteArray;');
AddMethod(@exp_Slice2,  'function SimbaExt.Slice(const Arr:TIntArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TIntArray; overload;');
AddMethod(@exp_Slice3,  'function SimbaExt.Slice(const Arr:TFloatArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TFloatArray; overload;');
AddMethod(@exp_Slice4,  'function SimbaExt.Slice(const Arr:TDoubleArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TDoubleArray; overload;');
AddMethod(@exp_Slice5,  'function SimbaExt.Slice(const Arr:TExtArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TExtArray; overload;');
AddMethod(@exp_Slice6,  'function SimbaExt.Slice(const Arr:TPointArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TPointArray; overload;');
AddMethod(@exp_Slice7,  'function SimbaExt.Slice(const Arr:TBoxArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TBoxArray; overload;');
AddMethod(@exp_Slice8,  'function SimbaExt.Slice(const Arr:String; Start,Stop:Int64=High(Int64); Step:Int64=1): String; overload;');
AddMethod(@exp_Slice9,  'function SimbaExt.Slice(const Arr:T2DIntArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DIntArray; overload;');
AddMethod(@exp_Slice10, 'function SimbaExt.Slice(const Arr:T2DExtArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DExtArray; overload;');
AddMethod(@exp_Slice11, 'function SimbaExt.Slice(const Arr:T2DFloatArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DFloatArray; overload;');
AddMethod(@exp_Slice12, 'function SimbaExt.Slice(const Arr:T2DDoubleArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DDoubleArray; overload;');
AddMethod(@exp_Slice13, 'function SimbaExt.Slice(const Arr:T2DPointArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DPointArray; overload;');
AddMethod(@exp_Slice14, 'function SimbaExt.Slice(const Arr:T2DByteArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DByteArray; overload;');
AddMethod(@exp_Slice15, 'function SimbaExt.Slice(const Arr:T2DBoxArray; Start,Stop:Int64=High(Int64); Step:Int64=1): T2DBoxArray; overload;');
AddMethod(@exp_Slice16, 'function SimbaExt.Slice(const Arr:TStringArray; Start,Stop:Int64=High(Int64); Step:Int64=1): TStringArray; overload;');


(* *)
AddMethod(@exp_Find_1, 'function SimbaExt.Find(const Arr:TByteArray; Item:Byte): Int32;');
AddMethod(@exp_Find_2, 'function SimbaExt.Find(const Arr:TIntArray; Item:Int32): Int32; overload;');
AddMethod(@exp_Find_3, 'function SimbaExt.Find(const Arr:TFloatArray; Item:Single): Int32; overload;');
AddMethod(@exp_Find_4, 'function SimbaExt.Find(const Arr:TDoubleArray; Item:Double): Int32; overload;');
AddMethod(@exp_Find_5, 'function SimbaExt.Find(const Arr:TExtArray; Item:Extended): Int32; overload;');
AddMethod(@exp_Find_6, 'function SimbaExt.Find(const Arr:TPointArray; Item:TPoint): Int32; overload;');
AddMethod(@exp_Find_7, 'function SimbaExt.Find(const Arr:TBoxArray; Item:TBox): Int32; overload;');
AddMethod(@exp_Find_8, 'function SimbaExt.Find(const Arr:TStringArray; Item:String): Int32; overload;');
AddMethod(@exp_Find_9, 'function SimbaExt.Find(const Arr:String; Item:Char): Int32; overload;');

AddMethod(@exp_FindEx_1, 'function SimbaExt.Find(const Arr, Seq:TByteArray): Int32; overload;');
AddMethod(@exp_FindEx_2, 'function SimbaExt.Find(const Arr, Seq:TIntArray): Int32; overload;');
AddMethod(@exp_FindEx_3, 'function SimbaExt.Find(const Arr, Seq:TFloatArray): Int32; overload;');
AddMethod(@exp_FindEx_4, 'function SimbaExt.Find(const Arr, Seq:TDoubleArray): Int32; overload;');
AddMethod(@exp_FindEx_5, 'function SimbaExt.Find(const Arr, Seq:TExtArray): Int32; overload;');
AddMethod(@exp_FindEx_6, 'function SimbaExt.Find(const Arr, Seq:TPointArray): Int32; overload;');
AddMethod(@exp_FindEx_7, 'function SimbaExt.Find(const Arr, Seq:TBoxArray): Int32; overload;');
AddMethod(@exp_FindEx_8, 'function SimbaExt.Find(const Arr, Seq:TStringArray): Int32; overload;');
AddMethod(@exp_FindEx_9, 'function SimbaExt.Find(const Arr, Seq:String): Int32; overload;');


(* *)
AddMethod(@exp_FindAll_1, 'function SimbaExt.FindAll(const Arr:TByteArray; Item:Byte): TIntArray;');
AddMethod(@exp_FindAll_2, 'function SimbaExt.FindAll(const Arr:TIntArray; Item:Int32): TIntArray; overload;');
AddMethod(@exp_FindAll_3, 'function SimbaExt.FindAll(const Arr:TFloatArray; Item:Single): TIntArray; overload;');
AddMethod(@exp_FindAll_4, 'function SimbaExt.FindAll(const Arr:TDoubleArray; Item:Double): TIntArray; overload;');
AddMethod(@exp_FindAll_5, 'function SimbaExt.FindAll(const Arr:TExtArray; Item:Extended): TIntArray; overload;');
AddMethod(@exp_FindAll_6, 'function SimbaExt.FindAll(const Arr:TPointArray; Item:TPoint): TIntArray; overload;');
AddMethod(@exp_FindAll_7, 'function SimbaExt.FindAll(const Arr:TBoxArray; Item:TBox): TIntArray; overload;');
AddMethod(@exp_FindAll_8, 'function SimbaExt.FindAll(const Arr:TStringArray; Item:String): TIntArray; overload;');
AddMethod(@exp_FindAll_9, 'function SimbaExt.FindAll(const Arr:String; Item:Char): TIntArray; overload;');

AddMethod(@exp_FindAllEx_1, 'function SimbaExt.FindAll(const Arr, Seq:TByteArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_2, 'function SimbaExt.FindAll(const Arr, Seq:TIntArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_3, 'function SimbaExt.FindAll(const Arr, Seq:TFloatArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_4, 'function SimbaExt.FindAll(const Arr, Seq:TDoubleArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_5, 'function SimbaExt.FindAll(const Arr, Seq:TExtArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_6, 'function SimbaExt.FindAll(const Arr, Seq:TPointArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_7, 'function SimbaExt.FindAll(const Arr, Seq:TBoxArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_8, 'function SimbaExt.FindAll(const Arr, Seq:TStringArray): TIntArray; overload;');
AddMethod(@exp_FindAllEx_9, 'function SimbaExt.FindAll(const Arr, Seq:String): TIntArray; overload;');
