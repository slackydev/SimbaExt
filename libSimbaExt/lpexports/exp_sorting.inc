{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports sorting methods
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
AddMethod(@exp_SortTBA,	'procedure SimbaExt.SortTBA(var Arr: TByteArray);');
AddMethod(@exp_SortTIA,	'procedure SimbaExt.SortTIA(var Arr: TIntArray);');
AddMethod(@exp_SortTFA,	'procedure SimbaExt.SortTFA(var Arr: TFloatArray);');
AddMethod(@exp_SortTDA,	'procedure SimbaExt.SortTDA(var Arr: TDoubleArray);');
AddMethod(@exp_SortTEA,	'procedure SimbaExt.SortTEA(var Arr: TExtendedArray);');

AddMethod(@exp_SortTPA,	       'procedure SimbaExt.SortTPA(var Arr: TPointArray);');
AddMethod(@exp_SortTPAFrom,    'procedure SimbaExt.SortTPAFrom(var Arr: TPointArray; const From:TPoint);');
AddMethod(@exp_SortTPAByRow,   'procedure SimbaExt.SortTPAByRow(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByColumn,'procedure SimbaExt.SortTPAByColumn(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByX,     'procedure SimbaExt.SortTPAByX(var Arr: TPointArray);');
AddMethod(@exp_SortTPAByY,     'procedure SimbaExt.SortTPAByY(var Arr: TPointArray);');

AddMethod(@exp_SortTSA,        'procedure SimbaExt.SortTSA(var Arr: TStringArray; CaseInsesitive:LongBool=False);');
AddMethod(@exp_SortTSANatural, 'procedure SimbaExt.SortTSANatural(var Arr: TStringArray);');

AddMethod(@exp_SortATPAFrom,    'procedure SimbaExt.SortATPAFrom(var Arr:T2DPointArray; pt:TPoint);');
AddMethod(@exp_SortATPAByLength,'procedure SimbaExt.SortATPAByLen(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByMean,  'procedure SimbaExt.SortATPAByMean(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByFirst, 'procedure SimbaExt.SortATPAByFirst(var Arr:T2DPointArray);');
AddMethod(@exp_SortATPAByIndex, 'procedure SimbaExt.SortATPAByIndex(var Arr:T2DPointArray; index:Int32);');

AddMethod(@exp_SortATBAByLength,'procedure SimbaExt.SortATBAByLen(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByMean,  'procedure SimbaExt.SortATBAByMean(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByFirst, 'procedure SimbaExt.SortATBAByFirst(var Arr:T2DByteArray);');
AddMethod(@exp_SortATBAByIndex, 'procedure SimbaExt.SortATBAByIndex(var Arr:T2DByteArray; index:Int32);');

AddMethod(@exp_SortATIAByLength,'procedure SimbaExt.SortATIAByLen(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByMean,  'procedure SimbaExt.SortATIAByMean(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByFirst, 'procedure SimbaExt.SortATIAByFirst(var Arr:T2DIntArray);');
AddMethod(@exp_SortATIAByIndex, 'procedure SimbaExt.SortATIAByIndex(var Arr:T2DIntArray; index:Int32);');

AddMethod(@exp_SortATEAByLength,'procedure SimbaExt.SortATEAByLen(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByMean,  'procedure SimbaExt.SortATEAByMean(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByFirst, 'procedure SimbaExt.SortATEAByFirst(var Arr:T2DExtArray);');
AddMethod(@exp_SortATEAByIndex, 'procedure SimbaExt.SortATEAByIndex(var Arr:T2DExtArray; index:Int32);');
