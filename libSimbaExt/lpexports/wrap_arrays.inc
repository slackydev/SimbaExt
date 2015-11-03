{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Wrappers for `Arrays`.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
//--| Int32 |-----------------------------------------------------------------\\
{$DEFINE T    := Int32}
{$DEFINE TOvf := Int64}
{$DEFINE T_WRAP := ExpDynI32Array}
{$DEFINE T_TYPE := DynIntArray}
{$DEFINE T_COMPARE := @cmpI32}

{$DEFINE ARRAY_CREATE		:= DynI32Array_Create}
{$DEFINE ARRAY_FREE		:= DynI32Array_Free}
{$DEFINE ARRAY_LENGTH		:= DynI32Array_Length}
{$DEFINE ARRAY_HIGH		:= DynI32Array_High}
{$DEFINE ARRAY_COPY		:= DynI32Array_Length}
{$DEFINE ARRAY_APPEND		:= DynI32Array_Append}
{$DEFINE ARRAY_EXTEND		:= DynI32Array_Extend}
{$DEFINE ARRAY_INDEXOF		:= DynI32Array_IndexOf}
{$DEFINE ARRAY_DELETE		:= DynI32Array_Delete}
{$DEFINE ARRAY_REMOVE		:= DynI32Array_Remove}
{$DEFINE ARRAY_POP		:= DynI32Array_Pop}
{$DEFINE ARRAY_POP2		:= DynI32Array_Pop2}
{$DEFINE ARRAY_INSERT		:= DynI32Array_Insert}
{$DEFINE ARRAY_SLICE		:= DynI32Array_Slice}
{$DEFINE ARRAY_SORT		:= DynI32Array_Sort}

{$DEFINE ARRAY_MIN		:= DynI32Array_Min}
{$DEFINE ARRAY_MAX		:= DynI32Array_Max}
{$DEFINE ARRAY_SUM		:= DynI32Array_Sum}
{$DEFINE ARRAY_MEAN		:= DynI32Array_Mean}
{$DEFINE ARRAY_MEDIAN		:= DynI32Array_Median}
{$DEFINE ARRAY_VARIANCE		:= DynI32Array_Var}
{$DEFINE ARRAY_STDEV		:= DynI32Array_Stdev}
{$DEFINE ARRAY_MODE		:= DynI32Array_Mode}
{$DEFINE ARRAY_QSELECT		:= DynI32Array_QSelect}

{$I Array_Wrap.inc}


//--| Int64 |-----------------------------------------------------------------\\
{$DEFINE T    := Int64}
{$DEFINE TOvf := Int64}
{$DEFINE T_WRAP := ExpDynI64Array}
{$DEFINE T_TYPE := DynInt64Array}
{$DEFINE T_COMPARE := @cmpI64}

{$DEFINE ARRAY_CREATE		:= DynI64Array_Create}
{$DEFINE ARRAY_FREE		:= DynI64Array_Free}
{$DEFINE ARRAY_LENGTH		:= DynI64Array_Length}
{$DEFINE ARRAY_HIGH		:= DynI64Array_High}
{$DEFINE ARRAY_COPY		:= DynI64Array_Length}
{$DEFINE ARRAY_APPEND		:= DynI64Array_Append}
{$DEFINE ARRAY_EXTEND		:= DynI64Array_Extend}
{$DEFINE ARRAY_INDEXOF		:= DynI64Array_IndexOf}
{$DEFINE ARRAY_DELETE		:= DynI64Array_Delete}
{$DEFINE ARRAY_REMOVE		:= DynI64Array_Remove}
{$DEFINE ARRAY_POP		:= DynI64Array_Pop}
{$DEFINE ARRAY_POP2		:= DynI64Array_Pop2}
{$DEFINE ARRAY_INSERT		:= DynI64Array_Insert}
{$DEFINE ARRAY_SLICE		:= DynI64Array_Slice}
{$DEFINE ARRAY_SORT		:= DynI64Array_Sort}

{$DEFINE ARRAY_MIN		:= DynI64Array_Min}
{$DEFINE ARRAY_MAX		:= DynI64Array_Max}
{$DEFINE ARRAY_SUM		:= DynI64Array_Sum}
{$DEFINE ARRAY_MEAN		:= DynI64Array_Mean}
{$DEFINE ARRAY_MEDIAN		:= DynI64Array_Median}
{$DEFINE ARRAY_VARIANCE		:= DynI64Array_Var}
{$DEFINE ARRAY_STDEV		:= DynI64Array_Stdev}
{$DEFINE ARRAY_MODE		:= DynI64Array_Mode}
{$DEFINE ARRAY_QSELECT		:= DynI64Array_QSelect}
{$I Array_Wrap.inc}


//--| Float32 |---------------------------------------------------------------\\
{$DEFINE T    := Single}
{$DEFINE TOvf := Double}
{$DEFINE T_WRAP := ExpDynF32Array}
{$DEFINE T_TYPE := DynFloatArray}
{$DEFINE T_COMPARE := @cmpF32}

{$DEFINE ARRAY_CREATE		:= DynF32Array_Create}
{$DEFINE ARRAY_FREE		:= DynF32Array_Free}
{$DEFINE ARRAY_LENGTH		:= DynF32Array_Length}
{$DEFINE ARRAY_HIGH		:= DynF32Array_High}
{$DEFINE ARRAY_COPY		:= DynF32Array_Length}
{$DEFINE ARRAY_APPEND		:= DynF32Array_Append}
{$DEFINE ARRAY_EXTEND		:= DynF32Array_Extend}
{$DEFINE ARRAY_INDEXOF		:= DynF32Array_IndexOf}
{$DEFINE ARRAY_DELETE		:= DynF32Array_Delete}
{$DEFINE ARRAY_REMOVE		:= DynF32Array_Remove}
{$DEFINE ARRAY_POP		:= DynF32Array_Pop}
{$DEFINE ARRAY_POP2		:= DynF32Array_Pop2}
{$DEFINE ARRAY_INSERT		:= DynF32Array_Insert}
{$DEFINE ARRAY_SLICE		:= DynF32Array_Slice}
{$DEFINE ARRAY_SORT		:= DynF32Array_Sort}

{$DEFINE ARRAY_MIN		:= DynF32Array_Min}
{$DEFINE ARRAY_MAX		:= DynF32Array_Max}
{$DEFINE ARRAY_SUM		:= DynF32Array_Sum}
{$DEFINE ARRAY_MEAN		:= DynF32Array_Mean}
{$DEFINE ARRAY_MEDIAN		:= DynF32Array_Median}
{$DEFINE ARRAY_VARIANCE		:= DynF32Array_Var}
{$DEFINE ARRAY_STDEV		:= DynF32Array_Stdev}
{$DEFINE ARRAY_MODE		:= DynF32Array_Mode}
{$DEFINE ARRAY_QSELECT		:= DynF32Array_QSelect}
{$I Array_Wrap.inc}


//--| Float64 |---------------------------------------------------------------\\
{$DEFINE T    := Double}
{$DEFINE TOvf := Double}
{$DEFINE T_WRAP := ExpDynF64Array}
{$DEFINE T_TYPE := DynDoubleArray}
{$DEFINE T_COMPARE := @cmpF64}

{$DEFINE ARRAY_CREATE		:= DynF64Array_Create}
{$DEFINE ARRAY_FREE		:= DynF64Array_Free}
{$DEFINE ARRAY_LENGTH		:= DynF64Array_Length}
{$DEFINE ARRAY_HIGH		:= DynF64Array_High}
{$DEFINE ARRAY_COPY		:= DynF64Array_Length}
{$DEFINE ARRAY_APPEND		:= DynF64Array_Append}
{$DEFINE ARRAY_EXTEND		:= DynF64Array_Extend}
{$DEFINE ARRAY_INDEXOF		:= DynF64Array_IndexOf}
{$DEFINE ARRAY_DELETE		:= DynF64Array_Delete}
{$DEFINE ARRAY_REMOVE		:= DynF64Array_Remove}
{$DEFINE ARRAY_POP		:= DynF64Array_Pop}
{$DEFINE ARRAY_POP2		:= DynF64Array_Pop2}
{$DEFINE ARRAY_INSERT		:= DynF64Array_Insert}
{$DEFINE ARRAY_SLICE		:= DynF64Array_Slice}
{$DEFINE ARRAY_SORT		:= DynF64Array_Sort}

{$DEFINE ARRAY_MIN		:= DynF64Array_Min}
{$DEFINE ARRAY_MAX		:= DynF64Array_Max}
{$DEFINE ARRAY_SUM		:= DynF64Array_Sum}
{$DEFINE ARRAY_MEAN		:= DynF64Array_Mean}
{$DEFINE ARRAY_MEDIAN		:= DynF64Array_Median}
{$DEFINE ARRAY_VARIANCE		:= DynF64Array_Var}
{$DEFINE ARRAY_STDEV		:= DynF64Array_Stdev}
{$DEFINE ARRAY_MODE		:= DynF64Array_Mode}
{$DEFINE ARRAY_QSELECT		:= DynF64Array_QSelect}
{$I Array_Wrap.inc}




