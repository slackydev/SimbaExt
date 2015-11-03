{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2014, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports types.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

AddType('TSize2D',	'packed record W,H:Int32; end;');
AddType('TSize3D',	'packed record W,H,D:Int32; end;');

AddType('EDistUnit',	'(duEuclidean, duManhatten, duChebyshev);');
AddType('EAlignAlgo',	'(EAA_BOUNDS, EAA_CHULL, EAA_BBOX);');
AddType('EThreshAlgo',	'(ETA_MEAN, ETA_MINMAX);');
AddType('ECenterAlgo',	'(ECA_BOUNDS, ECA_BBOX, ECA_MEAN, ECA_MEDIAN);');
AddType('EResizeAlgo',	'(ERA_NEAREST, ERA_BILINEAR, ERA_BICUBIC);');
AddType('EComparator',	'(CMP_LT, CMP_GT, CMP_EQ, CMP_NE, CMP_GTE, CMP_LTE);');
AddType('EColorDistance', '(ECD_RGB, ECD_RGB_SQRD, ECD_RGB_NORMED, '+
                           'ECD_HSV, ECD_HSV_SQRD, ECD_HSV_NORMED, '+
                           'ECD_XYZ, ECD_XYZ_SQRD, ECD_XYZ_NORMED, '+
                           'ECD_LAB, ECD_LAB_SQRD, ECD_LAB_NORMED, '+
                           'ECD_DELTAE, ECD_DELTAE_NORMED);');

AddType('TColorDistFunc', 'function(Color1:Pointer; Color2:Int32): Single;');
AddType('TFinder',  'packed record             '+
                    '  FCompareFunc:Pointer;   '+
                    '  FNumThreads: Int32;     '+
                    '  FColorInfo : Pointer;   '+
                    '  FColorSpace: Int32;     '+
                    'end;');

         
AddType('TSlackNode', 'packed record split: TPoint; l,r: Int32; deleted: Boolean; end;');
AddType('PSlackNode', '^TSlackNode');
AddType('TSlackArray', 'Array of TSlackNode;');
AddType('TSlackRefArray', 'Array of PSlackNode;');
AddType('TSlackTree', 'packed record data: TSlackArray; size: Int32; end;');
          
          
//---| Arrays.pas |---------------------------------------------------------------------\\
AddType('DynIntArray',    'packed record ref:Pointer; items:^TIntArray;    end;');
AddType('DynInt64Array',  'packed record ref:Pointer; items:^TInt64Array;  end;');
AddType('DynFloatArray',  'packed record ref:Pointer; items:^TFloatArray;  end;');
AddType('DynDoubleArray', 'packed record ref:Pointer; items:^TDoubleArray; end;');

