{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt

 Experimantal
 Exports `Arrays`.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
_TYPENAME := 'DynIntArray';
_T        := 'Int32';
_TOvf     := 'Int64';
AddMethod(@DynI32Array_Create,	'function  '+_TYPENAME+'.Create(data:Array of '+_T+'=nil; {$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF}): '+_TYPENAME+';');
AddMethod(@DynI32Array_Free,	'procedure '+_TYPENAME+'.Free({$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF});');
AddMethod(@DynI32Array_Length,	'function  '+_TYPENAME+'.Length(): SizeInt;');
AddMethod(@DynI32Array_High,	'function  '+_TYPENAME+'.High(): SizeInt;');
AddMethod(@DynI32Array_Append,	'procedure '+_TYPENAME+'.Append(item:'+_T+');');
AddMethod(@DynI32Array_Extend,	'procedure '+_TYPENAME+'.Extend(arr:'+_TYPENAME+');');
AddMethod(@DynI32Array_IndexOf,	'function  '+_TYPENAME+'.IndexOf(item:'+_T+'): Int32;');
AddMethod(@DynI32Array_Delete,	'procedure '+_TYPENAME+'.Delete(element:Int32);');
AddMethod(@DynI32Array_Remove,	'procedure '+_TYPENAME+'.Remove(item:'+_T+');');
AddMethod(@DynI32Array_Pop,	'function  '+_TYPENAME+'.Pop(): '+_T+';');
AddMethod(@DynI32Array_Pop2,	'function  '+_TYPENAME+'.Pop(idx:Int32): '+_T+'; overload;');
AddMethod(@DynI32Array_Insert,	'procedure '+_TYPENAME+'.Insert(idx:Int32; value:'+_T+');');
AddMethod(@DynI32Array_Slice,	'function  '+_TYPENAME+'.Slice(start:Int32=High(Int32); stop:Int32=High(Int32); step:Int32=1): '+_TYPENAME+';');
AddMethod(@DynI32Array_Sort,	'procedure '+_TYPENAME+'.Sort();');
AddMethod(@DynI32Array_Min,	'function '+_TYPENAME+'.Min(): '+_T+';');
AddMethod(@DynI32Array_Max,	'function '+_TYPENAME+'.Max(): '+_T+';');
AddMethod(@DynI32Array_Sum,	'function '+_TYPENAME+'.Sum(): '+_TOvf+';');
AddMethod(@DynI32Array_Mean,	'function '+_TYPENAME+'.Mean(): Double;');
AddMethod(@DynI32Array_Median,	'function '+_TYPENAME+'.Median(): '+_T+';');
AddMethod(@DynI32Array_Var,	'function '+_TYPENAME+'.Variance(): Double;');
AddMethod(@DynI32Array_Stdev,	'function '+_TYPENAME+'.Stdev(): Double;');
AddMethod(@DynI32Array_Mode,	'function '+_TYPENAME+'.Mode(): '+_T+';');
AddMethod(@DynI32Array_QSelect,	'function '+_TYPENAME+'.QuickSelect(kth:Int32): '+_T+';');

_TYPENAME := 'DynInt64Array';
_T        := 'Int64';
_TOvf     := 'Int64';
AddMethod(@DynI64Array_Create,	'function  '+_TYPENAME+'.Create(data:Array of '+_T+'=nil; {$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF}): '+_TYPENAME+';');
AddMethod(@DynI64Array_Free,	'procedure '+_TYPENAME+'.Free({$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF});');
AddMethod(@DynI64Array_Length,	'function  '+_TYPENAME+'.Length(): SizeInt;');
AddMethod(@DynI64Array_High,	'function  '+_TYPENAME+'.High(): SizeInt;');
AddMethod(@DynI64Array_Append,	'procedure '+_TYPENAME+'.Append(item:'+_T+');');
AddMethod(@DynI64Array_Extend,	'procedure '+_TYPENAME+'.Extend(arr:'+_TYPENAME+');');
AddMethod(@DynI64Array_IndexOf,	'function  '+_TYPENAME+'.IndexOf(item:'+_T+'): Int32;');
AddMethod(@DynI64Array_Delete,	'procedure '+_TYPENAME+'.Delete(element:Int32);');
AddMethod(@DynI64Array_Remove,	'procedure '+_TYPENAME+'.Remove(item:'+_T+');');
AddMethod(@DynI64Array_Pop,	'function  '+_TYPENAME+'.Pop(): '+_T+';');
AddMethod(@DynI64Array_Pop2,	'function  '+_TYPENAME+'.Pop(idx:Int32): '+_T+'; overload;');
AddMethod(@DynI64Array_Insert,	'procedure '+_TYPENAME+'.Insert(idx:Int32; value:'+_T+');');
AddMethod(@DynI64Array_Slice,	'function  '+_TYPENAME+'.Slice(start:Int32=High(Int32); stop:Int32=High(Int32); step:Int32=1): '+_TYPENAME+';');
AddMethod(@DynI64Array_Sort,	'procedure '+_TYPENAME+'.Sort();');
AddMethod(@DynI64Array_Min,	'function '+_TYPENAME+'.Min(): '+_T+';');
AddMethod(@DynI64Array_Max,	'function '+_TYPENAME+'.Max(): '+_T+';');
AddMethod(@DynI64Array_Sum,	'function '+_TYPENAME+'.Sum(): '+_TOvf+';');
AddMethod(@DynI64Array_Mean,	'function '+_TYPENAME+'.Mean(): Double;');
AddMethod(@DynI64Array_Median,	'function '+_TYPENAME+'.Median(): '+_T+';');
AddMethod(@DynI64Array_Var,	'function '+_TYPENAME+'.Variance(): Double;');
AddMethod(@DynI64Array_Stdev,	'function '+_TYPENAME+'.Stdev(): Double;');
AddMethod(@DynI64Array_Mode,	'function '+_TYPENAME+'.Mode(): '+_T+';');
AddMethod(@DynI64Array_QSelect,	'function '+_TYPENAME+'.QuickSelect(kth:Int32): '+_T+';');

_TYPENAME := 'DynFloatArray';
_T        := 'Single';
_TOvf     := 'Double';
AddMethod(@DynF32Array_Create,	'function  '+_TYPENAME+'.Create(data:Array of '+_T+'=nil; {$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF}): '+_TYPENAME+';');
AddMethod(@DynF32Array_Free,	'procedure '+_TYPENAME+'.Free({$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF});');
AddMethod(@DynF32Array_Length,	'function  '+_TYPENAME+'.Length(): SizeInt;');
AddMethod(@DynF32Array_High,	'function  '+_TYPENAME+'.High(): SizeInt;');
AddMethod(@DynF32Array_Append,	'procedure '+_TYPENAME+'.Append(item:'+_T+');');
AddMethod(@DynF32Array_Extend,	'procedure '+_TYPENAME+'.Extend(arr:'+_TYPENAME+');');
AddMethod(@DynF32Array_IndexOf,	'function  '+_TYPENAME+'.IndexOf(item:'+_T+'): Int32;');
AddMethod(@DynF32Array_Delete,	'procedure '+_TYPENAME+'.Delete(element:Int32);');
AddMethod(@DynF32Array_Remove,	'procedure '+_TYPENAME+'.Remove(item:'+_T+');');
AddMethod(@DynF32Array_Pop,	'function  '+_TYPENAME+'.Pop(): '+_T+';');
AddMethod(@DynF32Array_Pop2,	'function  '+_TYPENAME+'.Pop(idx:Int32): '+_T+'; overload;');
AddMethod(@DynF32Array_Insert,	'procedure '+_TYPENAME+'.Insert(idx:Int32; value:'+_T+');');
AddMethod(@DynF32Array_Slice,	'function  '+_TYPENAME+'.Slice(start:Int32=High(Int32); stop:Int32=High(Int32); step:Int32=1): '+_TYPENAME+';');
AddMethod(@DynF32Array_Sort,	'procedure '+_TYPENAME+'.Sort();');
AddMethod(@DynF32Array_Min,	'function '+_TYPENAME+'.Min(): '+_T+';');
AddMethod(@DynF32Array_Max,	'function '+_TYPENAME+'.Max(): '+_T+';');
AddMethod(@DynF32Array_Sum,	'function '+_TYPENAME+'.Sum(): '+_TOvf+';');
AddMethod(@DynF32Array_Mean,	'function '+_TYPENAME+'.Mean(): Double;');
AddMethod(@DynF32Array_Median,	'function '+_TYPENAME+'.Median(): '+_T+';');
AddMethod(@DynF32Array_Var,	'function '+_TYPENAME+'.Variance(): Double;');
AddMethod(@DynF32Array_Stdev,	'function '+_TYPENAME+'.Stdev(): Double;');
AddMethod(@DynF32Array_Mode,	'function '+_TYPENAME+'.Mode(): '+_T+';');
AddMethod(@DynF32Array_QSelect,	'function '+_TYPENAME+'.QuickSelect(kth:Int32): '+_T+';');

_TYPENAME := 'DynDoubleArray';
_T        := 'Double';
_TOvf     := 'Double';
AddMethod(@DynF64Array_Create,	'function  '+_TYPENAME+'.Create(data:Array of '+_T+'=nil; {$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF}): '+_TYPENAME+';');
AddMethod(@DynF64Array_Free,	'procedure '+_TYPENAME+'.Free({$IFNDEF CODEINSIGHT}procid:NativeInt=GetCurrThreadID(){$ENDIF});');
AddMethod(@DynF64Array_Length,	'function  '+_TYPENAME+'.Length(): SizeInt;');
AddMethod(@DynF64Array_High,	'function  '+_TYPENAME+'.High(): SizeInt;');
AddMethod(@DynF64Array_Append,	'procedure '+_TYPENAME+'.Append(item:'+_T+');');
AddMethod(@DynF64Array_Extend,	'procedure '+_TYPENAME+'.Extend(arr:'+_TYPENAME+');');
AddMethod(@DynF64Array_IndexOf,	'function  '+_TYPENAME+'.IndexOf(item:'+_T+'): Int32;');
AddMethod(@DynF64Array_Delete,	'procedure '+_TYPENAME+'.Delete(element:Int32);');
AddMethod(@DynF64Array_Remove,	'procedure '+_TYPENAME+'.Remove(item:'+_T+');');
AddMethod(@DynF64Array_Pop,	'function  '+_TYPENAME+'.Pop(): '+_T+';');
AddMethod(@DynF64Array_Pop2,	'function  '+_TYPENAME+'.Pop(idx:Int32): '+_T+'; overload;');
AddMethod(@DynF64Array_Insert,	'procedure '+_TYPENAME+'.Insert(idx:Int32; value:'+_T+');');
AddMethod(@DynF64Array_Slice,	'function  '+_TYPENAME+'.Slice(start:Int32=High(Int32); stop:Int32=High(Int32); step:Int32=1): '+_TYPENAME+';');
AddMethod(@DynF64Array_Sort,	'procedure '+_TYPENAME+'.Sort();');
AddMethod(@DynF64Array_Min,	'function '+_TYPENAME+'.Min(): '+_T+';');
AddMethod(@DynF64Array_Max,	'function '+_TYPENAME+'.Max(): '+_T+';');
AddMethod(@DynF64Array_Sum,	'function '+_TYPENAME+'.Sum(): '+_TOvf+';');
AddMethod(@DynF64Array_Mean,	'function '+_TYPENAME+'.Mean(): Double;');
AddMethod(@DynF64Array_Median,	'function '+_TYPENAME+'.Median(): '+_T+';');
AddMethod(@DynF64Array_Var,	'function '+_TYPENAME+'.Variance(): Double;');
AddMethod(@DynF64Array_Stdev,	'function '+_TYPENAME+'.Stdev(): Double;');
AddMethod(@DynF64Array_Mode,	'function '+_TYPENAME+'.Mode(): '+_T+';');
AddMethod(@DynF64Array_QSelect,	'function '+_TYPENAME+'.QuickSelect(kth:Int32): '+_T+';');
