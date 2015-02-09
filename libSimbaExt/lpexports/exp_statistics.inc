{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports numeric (number-array related) methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@exp_SumA_u8,		'function SimbaExt.Sum(const Arr: Array of UInt8): Int64;');
AddMethod(@exp_SumA_s8,		'function SimbaExt.Sum(const Arr: Array of Int8): Int64; overload;');
AddMethod(@exp_SumA_u16,	'function SimbaExt.Sum(const Arr: Array of UInt16): Int64; overload;');
AddMethod(@exp_SumA_s16,	'function SimbaExt.Sum(const Arr: Array of Int16): Int64; overload;');
AddMethod(@exp_SumA_u32,	'function SimbaExt.Sum(const Arr: Array of UInt32): Int64; overload;');
AddMethod(@exp_SumA_s32,	'function SimbaExt.Sum(const Arr: Array of Int32): Int64; overload;');
AddMethod(@exp_SumA_u64,	'function SimbaExt.Sum(const Arr: Array of UInt64): Int64; overload;');
AddMethod(@exp_SumA_s64,	'function SimbaExt.Sum(const Arr: Array of Int64): Int64; overload;');
AddMethod(@exp_SumA_f32,	'function SimbaExt.Sum(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_SumA_f64,	'function SimbaExt.Sum(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_SumA_f80,	'function SimbaExt.Sum(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_MinMaxA_u8,	'procedure SimbaExt.MinMax(const Arr: Array of UInt8; var Min,Max:Int64); overload;'); //overloaded from ?
AddMethod(@exp_MinMaxA_s8,	'procedure SimbaExt.MinMax(const Arr: Array of Int8; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_u16,	'procedure SimbaExt.MinMax(const Arr: Array of UInt16; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_s16,	'procedure SimbaExt.MinMax(const Arr: Array of Int16; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_u32,	'procedure SimbaExt.MinMax(const Arr: Array of UInt32; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_s32,	'procedure SimbaExt.MinMax(const Arr: Array of Int32; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_u64,	'procedure SimbaExt.MinMax(const Arr: Array of UInt64; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_s64,	'procedure SimbaExt.MinMax(const Arr: Array of Int64; var Min,Max:Int64); overload;');
AddMethod(@exp_MinMaxA_f32,	'procedure SimbaExt.MinMax(const Arr: Array of Single; var Min,Max:Extended); overload;');
AddMethod(@exp_MinMaxA_f64,	'procedure SimbaExt.MinMax(const Arr: Array of Double; var Min,Max:Extended); overload;');
AddMethod(@exp_MinMaxA_f80,	'procedure SimbaExt.MinMax(const Arr: Array of Extended; var Min,Max:Extended); overload;');

AddMethod(@exp_MinA_u8,		'function SimbaExt.Min(const Arr: Array of UInt8): Int64; overload;'); //overloaded from maths
AddMethod(@exp_MinA_s8,		'function SimbaExt.Min(const Arr: Array of Int8): Int64; overload;');
AddMethod(@exp_MinA_u16,	'function SimbaExt.Min(const Arr: Array of UInt16): Int64; overload;');
AddMethod(@exp_MinA_s16,	'function SimbaExt.Min(const Arr: Array of Int16): Int64; overload;');
AddMethod(@exp_MinA_u32,	'function SimbaExt.Min(const Arr: Array of UInt32): Int64; overload;');
AddMethod(@exp_MinA_s32,	'function SimbaExt.Min(const Arr: Array of Int32): Int64; overload;');
AddMethod(@exp_MinA_u64,	'function SimbaExt.Min(const Arr: Array of UInt64): Int64; overload;');
AddMethod(@exp_MinA_s64,	'function SimbaExt.Min(const Arr: Array of Int64): Int64; overload;');
AddMethod(@exp_MinA_f32,	'function SimbaExt.Min(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_MinA_f64,	'function SimbaExt.Min(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_MinA_f80,	'function SimbaExt.Min(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_MaxA_u8,		'function SimbaExt.Max(const Arr: Array of UInt8): Int64; overload;'); //overloaded from maths
AddMethod(@exp_MaxA_s8,		'function SimbaExt.Max(const Arr: Array of Int8): Int64; overload;');
AddMethod(@exp_MaxA_u16,	'function SimbaExt.Max(const Arr: Array of UInt16): Int64; overload;');
AddMethod(@exp_MaxA_s16,	'function SimbaExt.Max(const Arr: Array of Int16): Int64; overload;');
AddMethod(@exp_MaxA_u32,	'function SimbaExt.Max(const Arr: Array of UInt32): Int64; overload;');
AddMethod(@exp_MaxA_s32,	'function SimbaExt.Max(const Arr: Array of Int32): Int64; overload;');
AddMethod(@exp_MaxA_u64,	'function SimbaExt.Max(const Arr: Array of UInt64): Int64; overload;');
AddMethod(@exp_MaxA_s64,	'function SimbaExt.Max(const Arr: Array of Int64): Int64; overload;');
AddMethod(@exp_MaxA_f32,	'function SimbaExt.Max(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_MaxA_f64,	'function SimbaExt.Max(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_MaxA_f80,	'function SimbaExt.Max(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_MeanA_u8,	'function SimbaExt.Mean(const Arr: Array of UInt8): Extended;');
AddMethod(@exp_MeanA_s8,	'function SimbaExt.Mean(const Arr: Array of Int8): Extended; overload;');
AddMethod(@exp_MeanA_u16,	'function SimbaExt.Mean(const Arr: Array of UInt16): Extended; overload;');
AddMethod(@exp_MeanA_s16,	'function SimbaExt.Mean(const Arr: Array of Int16): Extended; overload;');
AddMethod(@exp_MeanA_u32,	'function SimbaExt.Mean(const Arr: Array of UInt32): Extended; overload;');
AddMethod(@exp_MeanA_s32,	'function SimbaExt.Mean(const Arr: Array of Int32): Extended; overload;');
AddMethod(@exp_MeanA_u64,	'function SimbaExt.Mean(const Arr: Array of UInt64): Extended; overload;');
AddMethod(@exp_MeanA_s64,	'function SimbaExt.Mean(const Arr: Array of Int64): Extended; overload;');
AddMethod(@exp_MeanA_f32,	'function SimbaExt.Mean(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_MeanA_f64,	'function SimbaExt.Mean(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_MeanA_f80,	'function SimbaExt.Mean(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_VarianceA_u8,	'function SimbaExt.Variance(const Arr: Array of UInt8): Extended;');
AddMethod(@exp_VarianceA_s8,	'function SimbaExt.Variance(const Arr: Array of Int8): Extended; overload;');
AddMethod(@exp_VarianceA_u16,	'function SimbaExt.Variance(const Arr: Array of UInt16): Extended; overload;');
AddMethod(@exp_VarianceA_s16,	'function SimbaExt.Variance(const Arr: Array of Int16): Extended; overload;');
AddMethod(@exp_VarianceA_u32,	'function SimbaExt.Variance(const Arr: Array of UInt32): Extended; overload;');
AddMethod(@exp_VarianceA_s32,	'function SimbaExt.Variance(const Arr: Array of Int32): Extended; overload;');
AddMethod(@exp_VarianceA_u64,	'function SimbaExt.Variance(const Arr: Array of UInt64): Extended; overload;');
AddMethod(@exp_VarianceA_s64,	'function SimbaExt.Variance(const Arr: Array of Int64): Extended; overload;');
AddMethod(@exp_VarianceA_f32,	'function SimbaExt.Variance(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_VarianceA_f64,	'function SimbaExt.Variance(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_VarianceA_f80,	'function SimbaExt.Variance(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_StdevA_u8,	'function SimbaExt.Stdev(const Arr: Array of UInt8): Extended;');
AddMethod(@exp_StdevA_s8,	'function SimbaExt.Stdev(const Arr: Array of Int8): Extended; overload;');
AddMethod(@exp_StdevA_u16,	'function SimbaExt.Stdev(const Arr: Array of UInt16): Extended; overload;');
AddMethod(@exp_StdevA_s16,	'function SimbaExt.Stdev(const Arr: Array of Int16): Extended; overload;');
AddMethod(@exp_StdevA_u32,	'function SimbaExt.Stdev(const Arr: Array of UInt32): Extended; overload;');
AddMethod(@exp_StdevA_s32,	'function SimbaExt.Stdev(const Arr: Array of Int32): Extended; overload;');
AddMethod(@exp_StdevA_u64,	'function SimbaExt.Stdev(const Arr: Array of UInt64): Extended; overload;');
AddMethod(@exp_StdevA_s64,	'function SimbaExt.Stdev(const Arr: Array of Int64): Extended; overload;');
AddMethod(@exp_StdevA_f32,	'function SimbaExt.Stdev(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_StdevA_f64,	'function SimbaExt.Stdev(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_StdevA_f80,	'function SimbaExt.Stdev(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_ModeA_u8,	'function SimbaExt.Mode(const Arr: Array of UInt8): UInt64;');
AddMethod(@exp_ModeA_s8,	'function SimbaExt.Mode(const Arr: Array of Int8): Int64; overload;');
AddMethod(@exp_ModeA_u16,	'function SimbaExt.Mode(const Arr: Array of UInt16): UInt64; overload;');
AddMethod(@exp_ModeA_s16,	'function SimbaExt.Mode(const Arr: Array of Int16): Int64; overload;');
AddMethod(@exp_ModeA_u32,	'function SimbaExt.Mode(const Arr: Array of UInt32): UInt64; overload;');
AddMethod(@exp_ModeA_s32,	'function SimbaExt.Mode(const Arr: Array of Int32): Int64; overload;');
AddMethod(@exp_ModeA_u64,	'function SimbaExt.Mode(const Arr: Array of UInt64): UInt64; overload;');
AddMethod(@exp_ModeA_s64,	'function SimbaExt.Mode(const Arr: Array of Int64): Int64; overload;');
AddMethod(@exp_ModeA_f32,	'function SimbaExt.Mode(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_ModeA_f64,	'function SimbaExt.Mode(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_ModeA_f80,	'function SimbaExt.Mode(const Arr: Array of Extended): Extended; overload;');

AddMethod(@exp_MedianA_u8,	'function SimbaExt.Median(const Arr: Array of UInt8): Extended;');
AddMethod(@exp_MedianA_s8,	'function SimbaExt.Median(const Arr: Array of Int8): Extended; overload;');
AddMethod(@exp_MedianA_u16,	'function SimbaExt.Median(const Arr: Array of UInt16): Extended; overload;');
AddMethod(@exp_MedianA_s16,	'function SimbaExt.Median(const Arr: Array of Int16): Extended; overload;');
AddMethod(@exp_MedianA_u32,	'function SimbaExt.Median(const Arr: Array of UInt32): Extended; overload;');
AddMethod(@exp_MedianA_s32,	'function SimbaExt.Median(const Arr: Array of Int32): Extended; overload;');
AddMethod(@exp_MedianA_u64,	'function SimbaExt.Median(const Arr: Array of UInt64): Extended; overload;');
AddMethod(@exp_MedianA_s64,	'function SimbaExt.Median(const Arr: Array of Int64): Extended; overload;');
AddMethod(@exp_MedianA_f32,	'function SimbaExt.Median(const Arr: Array of Single): Extended; overload;');
AddMethod(@exp_MedianA_f64,	'function SimbaExt.Median(const Arr: Array of Double): Extended; overload;');
AddMethod(@exp_MedianA_f80,	'function SimbaExt.Median(const Arr: Array of Extended): Extended; overload;');



AddMethod(@exp_TIACombinations,	'function SimbaExt.Combinations(const Arr: TIntArray; Seq:Integer): T2DIntArray;');
AddMethod(@exp_TEACombinations,	'function SimbaExt.Combinations(const Arr: TExtArray; Seq:Integer): T2DExtArray; overload;');
