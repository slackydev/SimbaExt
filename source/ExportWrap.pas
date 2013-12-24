{*-----------------------------------------------------------------------------|
| math.pas                                                                     |
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

function exp_Modulo(X,Y:Extended): Extended; Cdecl;
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




{*-----------------------------------------------------------------------------|
| Finder.pas                                                                   |
|-----------------------------------------------------------------------------*}
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
//Another day..


{*-----------------------------------------------------------------------------|
| DensityMap.pas                                                               |
|-----------------------------------------------------------------------------*}
//Another day..






{*-----------------------------------------------------------------------------|
| Points.pas                                                                   |
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

procedure exp_RotateTPAEx(const TPA: TPointArray; const Center:TPoint; Radians: Extended; var Result:TPointArray); Cdecl;
begin
  Result := RotateTPAEx(TPA, Center, Radians);
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

procedure exp_TPAEllipse(var TPA:TPointArray; const Center: TPoint; RadX,RadY:Integer); Cdecl;
begin
  TPAEllipse(TPA,Center,RadX,RadY);
end;

procedure exp_TPACircle(var TPA:TPointArray; const Center: TPoint; Radius:Integer); Cdecl;
begin
  TPACircle(TPA,Center,Radius);
end;

procedure exp_TPASimplePoly(var TPA:TPointArray; const Center:TPoint; Sides:Integer; const Dir:TPoint); Cdecl;
begin
  TPASimplePoly(TPA, Center, Sides, Dir);
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
| CSpline.pas                                                                  |
|-----------------------------------------------------------------------------*}
//Another day..


{*-----------------------------------------------------------------------------|
| TPAExtShape.pas                                                              |
|-----------------------------------------------------------------------------*}
//How about never?





{*-----------------------------------------------------------------------------|
| Morphology.pas                                                               |
|-----------------------------------------------------------------------------*}
procedure exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPASkeleton(TPA,FMin,FMax);
end;

procedure exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPASkeleton(TPA,FMin,FMax);
end;

procedure exp_TPAExpand(const TPA:TPointArray; Iterations:Integer; var Result:TPointArray); Cdecl;
begin
  Result := TPAExpand(TPA,Iterations);
end;






{*-----------------------------------------------------------------------------|
| Matrix.pas                                                                   |
|-----------------------------------------------------------------------------*}
procedure exp_NewMatrixEx(W,H, Init:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := NewMatrixEx(W,H, Init);
end;

procedure exp_NewMatrix(W,H:Integer; var Result:T2DIntArray); Cdecl;
begin
  Result := NewMatrix(W,H);
end;

procedure exp_MatrixSetTPA(var Matrix:T2DIntArray; const TPA:TPointArray; Value:Integer; const Offset:TPoint); Cdecl;
begin
  MatrixSetTPA(Matrix,TPA, Value, Offset);
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

procedure exp_MatCombine(var Mat:T2DIntArray; const Mat2:T2DIntArray; Value:Integer); Cdecl;
begin
  MatCombine(Mat, Mat2, Value);
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

procedure exp_FloodFillMatrixEx(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean; var Result:TPointArray); Cdecl;
begin
  Result := FloodFillMatrixEx(ImgArr, Start, EightWay);
end;






{*-----------------------------------------------------------------------------|
| Imaging.pas                                                                  |
|-----------------------------------------------------------------------------*}
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

procedure exp_ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TxResizeMethod); Cdecl;
begin
  ImResize(ImgArr, NewW, NewH, Method);
end;





{*-----------------------------------------------------------------------------|
| Randomiz.pas                                                                 |
|-----------------------------------------------------------------------------*}
procedure exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer; var Result:TPointArray); Cdecl;
begin
  Result := RandomTPA(Amount,MinX,MinY,MaxX,MaxY);
end;


procedure exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer; var Result:TPointArray); Cdecl;
begin
  Result := RandomTPA(Amount,CX,CY,RadX,RadY);
end;


procedure exp_RandomTIA(Amount:Integer; Low,Hi:Integer; var Result: TIntArray); Cdecl; 
begin
  Result := RandomTIA(Amount,Low,Hi);
end;






{*-----------------------------------------------------------------------------|
| Strings.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_StrPosEx(const Text, SubStr:String; var Result:TIntArray); Cdecl;
begin
  Result := StrPosEx(Text, SubStr);
end;

function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String; Cdecl;
begin
  Result := StrReplace(Text, SubStr, RepStr, Flags);
end;


procedure exp_StrExplode(const Text, Sep: String; var Result: TStrArray); Cdecl;
begin
  Result := StrExplode(Text, Sep);
end;


  
 