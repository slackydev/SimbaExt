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
AddType('TFinder',	'record                    '+
                        '  FCompareFunc:Pointer;   '+
                        '  FNumThreads: Int32;     '+
                        '  FColorInfo : Pointer;   '+
                        '  FColorSpace: Int32;     '+
                        '  FUseLookup : Boolean;   '+
                        '  FLookupTable : ^Single; '+
                        'end;');

AddType('TChars', 'Array of T2DIntArray;');


//AddType('SEBitmap', 'record FData: TByteArray; Width,Height: UInt32; Initialized:Boolean; Index,NativeIndex:PRGB32; end;');

(*
AddType('TObjExpo',	'type Pointer;');
AddType('SimbaExt',	'type Pointer;');
AddType('TObjMath',	'type Pointer;');
AddType('TObjTime',	'type Pointer;');
AddType('TObjOSPath',	'type Pointer;');
AddType('TObjOS',	'record path: TObjOSPath; end;');

AddVar('SERaw',		'TObjExpo');
AddVar('SE',		'SimbaExt');
AddVar('Math',		'TObjMath');
AddVar('TimeUtils',	'TObjTime');
AddVar('OS',		'TObjOS');
*)
