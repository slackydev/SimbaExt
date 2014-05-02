{*-----------------------------------------------------------------------------|
| CoreMath.pas                                                                 |
|-----------------------------------------------------------------------------*}
function exp_DistManhattan(pt1,pt2: TPoint): Extended; Cdecl;
begin
  Result := DistManhattan(pt1,pt2);
end;

function exp_DistEuclidean(pt1,pt2: TPoint): Extended; Cdecl;
begin
  Result := DistEuclidean(pt1,pt2);
end;

function exp_DistChebyshev(pt1,pt2: TPoint): Extended; Cdecl;
begin
  Result := DistChebyshev(pt1,pt2);
end;

function exp_DistOctagonal(pt1,pt2: TPoint): Extended; Cdecl;
begin
  Result := DistOctagonal(pt1,pt2);
end;

function exp_DistToLine(Pt, sA, sB: TPoint): Extended; Cdecl;
begin
  Result := DistToLine(Pt, sA, sB);
end;

function exp_Modulo(X,Y:Extended): Extended; Cdecl;
begin
  Result := Modulo(X,Y);
end;

function exp_IModulo(X,Y:Integer): Integer; Cdecl;
begin
  Result := Modulo(X,Y);
end;

function exp_InCircle(const Pt, Center: TPoint; Radius: Integer): Boolean; Cdecl;
begin
  Result := InCircle(Pt, Center, Radius);
end;

function exp_InEllipse(const Pt,Center:TPoint; YRad, XRad: Integer): Boolean; Cdecl;
begin
  Result := InEllipse(Pt, Center, YRad, XRad);
end;

function exp_InRect(const Pt:TPoint; const A,B,C,D:TPoint): Boolean; Cdecl;
begin
  Result := InRect(Pt, A,B,C,D);
end;

//
function exp_InPoly(x,y:Integer; const Poly:TPointArray): Boolean; Cdecl;
begin
  Result := InPoly(x,y, poly);
end;

function exp_InPolyR(x,y:Integer; const Poly:TPointArray): Boolean; Cdecl;
begin
  Result := InPolyR(x,y, poly);
end;

function exp_InPolyW(x,y:Integer; const Poly:TPointArray): Boolean; Cdecl;
begin
  Result := InPolyW(x,y, poly);
end;

function exp_DeltaAngle(DegA,DegB:Extended): Extended; Cdecl;
begin
  Result := DeltaAngle(DegA,DegB);
end;


{*-----------------------------------------------------------------------------|
| CoreMisc.pas                                                                 |
|-----------------------------------------------------------------------------*}
procedure exp_MoveTSA(const InArr:AnsiString; var DestArr:AnsiString; Source, Dest, Size:Integer); Cdecl;
begin
  Move(InArr, DestArr, Source, Dest, Size);
end;

procedure exp_MoveTIA(const InArr:TIntArray; var DestArr:TIntArray; Source, Dest, Size:Integer); Cdecl;
begin
  Move(InArr, DestArr, Source, Dest, Size);
end;

procedure exp_MoveTEA(const InArr:TExtArray; var DestArr:TExtArray; Source, Dest, Size:Integer); Cdecl;
begin
  Move(InArr, DestArr, Source, Dest, Size);
end;

procedure exp_MoveTPA(const InArr:TPointArray; var DestArr:TPointArray; Source, Dest, Size:Integer); Cdecl;
begin
  Move(InArr, DestArr, Source, Dest, Size);
end;


{*-----------------------------------------------------------------------------|
| Numeric.pas                                                                  |
|-----------------------------------------------------------------------------*}
function exp_SumTIA(const Arr: TIntArray): Integer; Cdecl;
begin
  Result := SumTIA(Arr);
end;

function exp_SumTEA(const Arr: TExtArray): Extended; Cdecl;
begin
  Result := SumTEA(Arr);
end;

procedure exp_TIACombinations(const Arr: TIntArray; Seq:Integer; var Result: T2DIntArray); Cdecl;
begin
  Result := TIACombinations(Arr, Seq);
end;

procedure exp_TEACombinations(const Arr: TExtArray; Seq:Integer; var Result: T2DExtArray); Cdecl;
begin
  Result := TEACombinations(Arr, Seq);
end;

procedure exp_MinMaxTIA(const Arr: TIntArray; var Min:Integer; var Max: Integer); Cdecl;
begin
  MinMaxTIA(Arr, Min,Max);
end;

procedure exp_MinMaxTEA(const Arr: TExtArray; var Min:Extended; var Max: Extended); Cdecl;
begin
  MinMaxTEA(Arr, Min,Max);
end;

//function exp_TIAMatches(const Arr1, Arr2:TIntArray; InPercent, Inversed:Boolean): Integer; Cdecl;
//begin
//  Result := TIAMatches(Arr1,Arr2, InPercent, Inversed);
//end;

//function exp_LogscaleTIA(const Freq:TIntArray; Scale: Integer; var Result:TIntArray); Cdecl;
//begin
//  Result := LogscaleTIA(Freq, Scale);
//end;





{*-----------------------------------------------------------------------------|
| Sorting.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_SortTIA(var Arr: TIntArray); Cdecl;
begin
  SortTIA(Arr);
end;

procedure exp_SortTEA(var Arr: TExtArray); Cdecl;
begin
  SortTEA(Arr);
end;

procedure exp_SortTPA(var Arr: TPointArray); Cdecl;
begin
  SortTPA(Arr);
end;

procedure exp_SortTPAFrom(var Arr: TPointArray; const From:TPoint); Cdecl;
begin
  SortTPAFrom(Arr, From);
end;

procedure exp_SortTPAByRow(var Arr: TPointArray); Cdecl;
begin
  SortTPAByRow(Arr);
end;

procedure exp_SortTPAByColumn(var Arr: TPointArray); Cdecl;
begin
  SortTPAByColumn(Arr);
end;

procedure exp_SortTPAByX(var Arr: TPointArray); Cdecl;
begin
  SortTPAByX(Arr);
end;

procedure exp_SortTPAByY(var Arr: TPointArray); Cdecl;
begin
  SortTPAByY(Arr);
end;


procedure exp_SortTSA(var Arr: TStringArray; CaseInsensitve:Boolean=False); Cdecl;
begin
  SortTSA(Arr, CaseInsensitve);
end;


procedure exp_SortTSANatural(var Arr: TStringArray); Cdecl;
begin
  SortTSANatural(Arr);
end;

procedure exp_SortATPAByLength(var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByLength(Arr);
end;

procedure exp_SortATPAByMean(var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByMean(Arr);
end;

procedure exp_SortATPAByFirst(var Arr:T2DPointArray); Cdecl;
begin
  SortATPAByFirst(Arr);
end;

procedure exp_SortATPAByIndex(var Arr:T2DPointArray; index:Int32); Cdecl;
begin
  SortATPAByIndex(Arr, index);
end;




{*-----------------------------------------------------------------------------|
| Finder.pas                                                                   |
|-----------------------------------------------------------------------------*}
procedure exp_MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray); Cdecl;
begin
  Result := MatchColorRGB(ImgArr, Color, CCMode);
end;

procedure exp_MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray); Cdecl;
begin
  Result := MatchColorXYZ(ImgArr, Color, CCMode);
end;

procedure exp_MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray); Cdecl;
begin
  Result := MatchColorLAB(ImgArr, Color, CCMode);
end;

procedure exp_MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode; var Result:T2DFloatArray); Cdecl;
begin
  Result := MatchColorLCh(ImgArr, Color, CCMode);
end;



function exp_ImFindColorTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, Tol:Integer): Boolean; Cdecl;
begin
  Result := ImFindColorTolEx(ImgArr, TPA, Color, Tol);
end;

function exp_ImFindColorsTolEx(const ImgArr:T2DIntArray; var TPA:TPointArray; Colors:TIntArray; Tol:Integer): Boolean; Cdecl;
begin
  Result := ImFindColorsTolEx(ImgArr, TPA, Colors, Tol);
end;

function exp_ImFindColorTolExLCH(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;
begin
  Result := ImFindColorTolExLCH(ImgArr, TPA, Color, ColorTol, LightTol);
end;

function exp_ImFindColorTolExLAB(const ImgArr:T2DIntArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean; Cdecl;
begin
  Result := ImFindColorTolExLAB(ImgArr, TPA, Color, ColorTol, LightTol);
end;




{*-----------------------------------------------------------------------------|
| SimpleOCR.pas                                                                |
|-----------------------------------------------------------------------------*}
function exp_ImGetText(ImgArr:T2DIntArray; Font:TChars; MinCharSpace, MinSpace, TextPixTol: Integer; Range:AnsiString): AnsiString; Cdecl;
begin
  Result := ImGetText(ImgArr, Font, MinCharSpace, MinSpace, TextPixTol, Range);
end;



{*-----------------------------------------------------------------------------|
| DensityMap.pas                                                               |
|-----------------------------------------------------------------------------*}
//Another day..






{*-----------------------------------------------------------------------------|
| PointTools.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_ScalePoint(const Center, Pt:TPoint; Radius:Integer): TPoint; Cdecl;
begin
  Result := ScalePoint(Center, Pt, Radius);
end;

function exp_SumTPA(const TPA: TPointArray): TPoint; Cdecl;
begin
  Result := SumTPA(TPA);
end;

procedure exp_TPASplitAxis(const TPA: TPointArray; var X:TIntArray; var Y:TIntArray); Cdecl;
begin
  TPASplitAxis(TPA, X,Y);
end;


procedure exp_TPAJoinAxis(const X:TIntArray; const Y:TIntArray; var TPA:TPointArray); Cdecl;
begin
  TPAJoinAxis(X,Y, TPA);
end;

procedure exp_TPAFilter(var TPA: TPointArray; const Shape:TPointArray; const TopLeft:TPoint); Cdecl;
begin
  TPAFilter(TPA, Shape, TopLeft);
end;

procedure exp_TPAFilterBounds(var TPA: TPointArray; x1,y1,x2,y2:Integer); Cdecl;
begin
  TPAFilterBounds(TPA, x1,y1,x2,y2);
end;

procedure exp_ATPAFilter(var ATPA: T2DPointArray; MinLength, MinW, MinH, MaxW, MaxH: Integer; Align:Boolean); Cdecl;
begin
  ATPAFilter(ATPA, MinLength, MinW, MinH, MaxW, MaxH, Align);
end;

procedure exp_TPAExtremes(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := TPAExtremes(TPA);
end;

procedure exp_TPABBox(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := TPABBox(TPA);
end;

function exp_TPACenter(const TPA: TPointArray; Method: TxCenterMethod; Inside:Boolean): TPoint; Cdecl;
begin
  Result := TPACenter(TPA, Method, Inside);
end;


procedure exp_GetAdjacent(var adj:TPointArray; n:TPoint; EightWay:Boolean); Cdecl;
begin
  GetAdjacent(adj, n, EightWay);
end;

function exp_TPACircularity(const TPA: TPointArray): Extended; Cdecl;
begin
  Result := TPACircularity(TPA);
end; 

function exp_TPAConvexity(const TPA: TPointArray): Extended; Cdecl;
begin
  Result := TPAConvexity(TPA);
end;

procedure exp_ReverseTPA(var TPA: TPointArray); Cdecl;
begin
  ReverseTPA(TPA);
end;

procedure exp_TPARemoveDupes(var TPA: TPointArray); Cdecl;
begin
  TPARemoveDupes(TPA);
end;


procedure exp_LongestPolyVector(const Poly:TPointArray; var A,B:TPoint); Cdecl;
begin
  LongestPolyVector(Poly, A,B);
end;

procedure exp_InvertTPA(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := InvertTPA(TPA);
end;

procedure exp_RotateTPA(const TPA: TPointArray; Radians: Extended; var Result:TPointArray); Cdecl;
begin
  Result := RotateTPA(TPA, Radians);
end;

procedure exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer; var Result:T2DPointArray); Cdecl;
begin
  Result := TPAPartition(TPA, BoxWidth, BoxHeight);
end;

procedure exp_AlignTPA(const TPA:TPointArray; Method: TxAlignMethod; var Angle:Extended; var Result:TPointArray); Cdecl;
begin
  Result := AlignTPA(TPA, Method, Angle);
end;

procedure exp_CleanSortTPA(const TPA: TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := CleanSortTPA(TPA);
end;


procedure exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean; var Result: TPointArray); Cdecl;
begin
  Result := UniteTPA(TPA1, TPA2, RemoveDupes);
end;


procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint); Cdecl;
begin
  TPALine(TPA, P1,P2);
end;

procedure exp_ConnectTPA(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := ConnectTPA(TPA);
end;

procedure exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended; var Result: TPointArray); Cdecl;
begin
  Result := ConnectTPAEx(TPA, Tension);
end;

procedure exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint; var Result:TPointArray); Cdecl;
begin
  Result := XagonPoints(Center, Sides, Dir);
end;

procedure exp_TPAEllipseBase(const Center: TPoint; RadiusX, RadiusY:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPAEllipseBase(Center, RadiusX, RadiusY);
end;

procedure exp_TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := TPAEllipse(Center, RadX,RadY, Filled);
end;

procedure exp_TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := TPACircle(Center, Radius, Filled);
end;

procedure exp_TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint; var Result:TPointArray); Cdecl;
begin
  Result := TPASimplePoly(Center, Sides, Dir);
end;

procedure exp_ConvexHull(const TPA:TPointArray; var Result: TPointArray); Cdecl;
begin
  Result := ConvexHull(TPA);
end;

procedure exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean; var Result: TPointArray); Cdecl;
begin
  Result := FloodFillTPAEx(TPA, Start, EightWay, KeepEdges);
end;


procedure exp_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := FloodFillTPA(TPA,Start,EightWay);
end;

procedure exp_TPAOutline(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := TPAOutline(TPA);
end;

procedure exp_TPABorder(const TPA:TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := TPABorder(TPA);
end;

procedure exp_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := FloodFillPolygon(Poly, EightWay);
end;

procedure exp_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean; var Result:T2DPointArray); Cdecl;
begin
  Result := ClusterTPAEx(TPA,DistX,DistY, Eightway);
end;

procedure exp_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean; var Result:T2DPointArray); Cdecl;
begin
  Result := ClusterTPA(TPA,Distance, Eightway);
end;

procedure exp_TPAEdges(const TPA: TPointArray; var Result:TPointArray); Cdecl;
begin
  Result := TPAEdges(TPA);
end;



{*-----------------------------------------------------------------------------|
| Spline.pas                                                                  |
|-----------------------------------------------------------------------------*}
//Another day..




{*-----------------------------------------------------------------------------|
| Morphology.pas                                                               |
|-----------------------------------------------------------------------------*}
procedure exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPASkeleton(TPA,FMin,FMax);
end;

procedure exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPAReduce(TPA,FMin,FMax,Iterations);
end;

procedure exp_TPAExpand(const TPA:TPointArray; Iterations:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPAExpand(TPA,Iterations);
end;






{*-----------------------------------------------------------------------------|
| MatrixTools.pas                                                                   |
|-----------------------------------------------------------------------------*}
procedure exp_NewMatrixEx(W,H, Init:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := NewMatrixEx(W,H, Init);
end;

procedure exp_NewMatrix(W,H:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := NewMatrix(W,H);
end;

procedure exp_MatInsertTPA(var Matrix:T2DIntArray; const TPA:TPointArray; Value:Integer); Cdecl;
begin
  MatInsertTPA(Matrix,TPA, Value);
end;

procedure exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean; var Result:T2DIntArray); Cdecl;
begin
  Result := TPAToMatrixEx(TPA,Init,Value,Align);
end;

procedure exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean; var Result:T2DIntArray); Cdecl;
begin
  Result := TPAToMatrix(TPA, Value, Align);
end;

procedure exp_NormalizeMat(const Mat:T2DIntArray; Alpha, Beta:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := NormalizeMat(Mat, Alpha, Beta);
end;

procedure exp_MatGetValues(const Mat:T2DIntArray; const Indices:TPointArray; var Result:TIntArray); Cdecl;
begin
  Result := MatGetValues(Mat, Indices);
end;

procedure exp_MatGetCol(const Mat:T2DIntArray; Column:Integer; var Result:TIntArray); Cdecl;
begin
  Result := MatGetCol(Mat, Column);
end;

procedure exp_MatGetRow(const Mat:T2DIntArray; Row:Integer; var Result:TIntArray); Cdecl;
begin
  Result := MatGetRow(Mat, Row);
end;

procedure exp_MatGetCols(const Mat:T2DIntArray; FromCol, ToCol:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := MatGetCols(Mat, FromCol, ToCol);
end;

procedure exp_MatGetRows(const Mat:T2DIntArray; FromRow, ToRow:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := MatGetRows(Mat, FromRow, ToRow);
end;

procedure exp_MatGetArea(const Mat:T2DIntArray; X1,Y1,X2,Y2:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := MatGetArea(Mat, X1,Y1,X2,Y2);
end;

procedure exp_MatFromTIA(const Arr:TIntArray; Width,Height:Integer; var Result: T2DIntArray); Cdecl;
begin
  Result := MatFromTIA(Arr, Width, Height);
end;

procedure exp_PadMatrix(var Matrix:T2DIntArray; HPad,WPad:Integer); Cdecl;
begin
  PadMatrix(Matrix,HPad,WPad);
end;

procedure exp_FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := FloodFillMatrix(ImgArr, Start, EightWay);
end;






{*-----------------------------------------------------------------------------|
| Imaging.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_GaussKernel(KernelRadius:Integer; Sigma:Single; var Result:T2DFloatArray); Cdecl;
begin
  Result := GaussKernel(KernelRadius, Sigma);
end;

procedure exp_ImBlurFilter(ImgArr: T2DIntArray; Block:Integer; var Result: T2DIntArray); Cdecl;
begin
  Result := ImBlurFilter(ImgArr, Block);
end;

procedure exp_ImMedianFilter(ImgArr: T2DIntArray; Block:Integer; var Result: T2DIntArray); Cdecl;
begin
  Result := ImMedianFilter(ImgArr, Block);
end;

procedure exp_ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean; var Result: T2DIntArray); Cdecl;
begin
  Result := ImBrighten(ImgArr, Amount, Legacy);
end;

procedure exp_ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended; var Result: T2DIntArray); Cdecl;
begin
  Result := ImEnhance(ImgArr, Enhancement, C);
end;

procedure exp_ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean; var Result: T2DIntArray); Cdecl;
begin
  Result := ImThreshold(ImgArr, Threshold, Alpha, Beta, Invert);
end;

procedure exp_ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TxThreshMethod; C:Integer; var Result: T2DIntArray); Cdecl;
begin
  Result := ImThresholdAdaptive(ImgArr, Alpha, Beta, Invert, Method, C);
end;


procedure exp_ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean; var Result: T2DPointArray); Cdecl;
begin
  Result := ImFindContours(ImgArr,Outlines);
end;

procedure exp_ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer; var Result: TPointArray); Cdecl;
begin
  Result := ImCEdges(ImgArr, MinDiff);
end;

procedure exp_ImSobel(const ImgArr: T2DIntArray; var Result:T2DIntArray); Cdecl;
begin
  Result := ImSobel(ImgArr); 
end;

procedure exp_ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray; var Result:T2DIntArray); Cdecl;
begin
  Result := ImConvolve(ImgArr, Mask);
end;

procedure exp_ImGaussBlur(const ImgArr: T2DIntArray; Radius: Integer; Sigma: Single; var Result:T2DIntArray); Cdecl;
begin
  Result := ImGaussBlur(ImgArr, Radius, Sigma); 
end;

procedure exp_ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TxResizeMethod); Cdecl;
begin
  ImResize(ImgArr, NewW, NewH, Method);
end;



{*-----------------------------------------------------------------------------|
| Randomize.pas                                                                 |
|-----------------------------------------------------------------------------*}
procedure exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer; var Result:TPointArray); Cdecl;
begin
  Result := RandomTPA(Amount,MinX,MinY,MaxX,MaxY);
end;


procedure exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer; var Result:TPointArray); Cdecl;
begin
  Result := RandomCenterTPA(Amount,CX,CY,RadX,RadY);
end;


procedure exp_RandomTIA(Amount:Integer; Low,Hi:Integer; var Result: TIntArray); Cdecl; 
begin
  Result := RandomTIA(Amount,Low,Hi);
end;






{*-----------------------------------------------------------------------------|
| StringTools.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_StrPosEx(const SubStr, Text:String; var Result:TIntArray); Cdecl;
begin
  Result := StrPosEx(SubStr, Text);
end;

function exp_StrPosL(const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosL(SubStr, Text);
end;

function exp_StrPosR(const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosR(Text, SubStr);
end;

function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String; Cdecl;
begin
  Result := StrReplace(Text, SubStr, RepStr, Flags);
end;


procedure exp_StrExplode(const Text, Sep: String; var Result: TStrArray); Cdecl;
begin
  Result := StrExplode(Text, Sep);
end;


{*-----------------------------------------------------------------------------|
| CornerDet.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; var Result: T2DFloatArray); Cdecl;
begin
  Result := CornerResponse(Mat, GaussDev, KSize);
end;

procedure exp_FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer; var Result: TPointArray); Cdecl;
begin
  Result := FindCornerPoints(Mat, GaussDev, KSize, Thresh, Footprint);
end;

procedure exp_FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer; var Result:TPointArray); Cdecl;
begin
  Result := FindCornerMidPoints(Mat, GaussDev, KSize, Thresh, MinDist);
end;







{*-----------------------------------------------------------------------------|
| MatrixOps.pas                                                                |
|-----------------------------------------------------------------------------*}

procedure exp_IndicesI(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

procedure exp_IndicesE(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

procedure exp_IndicesD(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

procedure exp_IndicesF(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;


{-------| Extended version of Indices |-------}
procedure exp_IndicesExI(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

procedure exp_IndicesExE(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

procedure exp_IndicesExD(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

procedure exp_IndicesExF(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator; var Result: TPointArray); Cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;


procedure exp_MinMaxI(Mat:T2DIntArray; var Min, Max:Integer); Cdecl;
begin
  MinMax(Mat, Min, Max);
end;

procedure exp_MinMaxE(Mat:T2DExtArray; var Min, Max:Extended); Cdecl;
begin
  MinMax(Mat, Min, Max);
end;

procedure exp_MinMaxD(Mat:T2DDoubleArray; var Min, Max:Double); Cdecl;
begin
  MinMax(Mat, Min, Max);
end;

procedure exp_MinMaxF(Mat:T2DFloatArray; var Min, Max:Single); Cdecl;
begin
  MinMax(Mat, Min, Max);
end;


//argmax
function exp_ArgMaxI(Mat:T2DIntArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxE(Mat:T2DExtArray): TPoint; Cdecl; 
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxD(Mat:T2DDoubleArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

function exp_ArgMaxF(Mat:T2DFloatArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

//argmin
function exp_ArgMinI(Mat:T2DIntArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinE(Mat:T2DExtArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinD(Mat:T2DDoubleArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

function exp_ArgMinF(Mat:T2DFloatArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;


{-------| Extended version of argmin/max |-------}
//argmax
function exp_ArgMaxExI(Mat:T2DIntArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExE(Mat:T2DExtArray; B:TBox): TPoint; Cdecl; 
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExD(Mat:T2DDoubleArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

function exp_ArgMaxExF(Mat:T2DFloatArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

//argmin
function exp_ArgMinExI(Mat:T2DIntArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExE(Mat:T2DExtArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExD(Mat:T2DDoubleArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

function exp_ArgMinExF(Mat:T2DFloatArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

