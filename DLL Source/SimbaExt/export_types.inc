{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=]
 Copyright (c) 2013, Jarl K. <Slacky> Holta || http://github.com/WarPie
 All rights reserved.
 For more info see: Copyright.txt
 
 Exports types.
[=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

AddType('TAlignAlgo',	  '(AA_BOUNDS, AA_CHULL, AA_BBOX);');
AddType('TThreshAlgo',	'(TA_MEAN, TA_MINMAX);');
AddType('TCenterAlgo',	'(CA_BOUNDS, CA_BBOX, CA_MEAN, CA_MEDIAN);');
AddType('TResizeAlgo',	'(RA_NEAREST, RA_BILINEAR, RA_BICUBIC);');
AddType('TCCorrMode',	  '(CC_EUCLID, CC_EUCLID_NORMED, CC_EUCLID_SQUARED, CC_CHEB, CC_CHEB_NORMED);');
AddType('TComparator',	'(__LT__, __GT__, __EQ__, __NE__, __GE__, __LE__);');
AddType('TChars',	      'Array of T2DIntArray;');
AddType('TCharsArray',	'Array of TChars;');