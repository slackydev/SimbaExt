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


{* Select min of 3 values *}
function exp_Min3f(X,Y,Z:Extended): Extended; cdecl; begin Result := Min(x,y,z); end;
function exp_Min3i(X,Y,Z:Int64): Int64; cdecl; begin Result := Min(x,y,z); end;

{* Select max of 3 values *}
function exp_Max3f(X,Y,Z:Extended): Extended; cdecl; begin Result := Max(x,y,z); end;
function exp_Max3i(X,Y,Z:Int64): Int64; cdecl; begin Result := Max(x,y,z); end;


{*-----------------------------------------------------------------------------|
| Std.pas                                                                      |
|-----------------------------------------------------------------------------*}

(** Array slicing **)
function exp_Slice1(Arr:TIntArray; Start,Stop,Step:Int32): TIntArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice2(Arr:TExtArray; Start,Stop,Step:Int32): TExtArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice3(Arr:TFloatArray; Start,Stop,Step:Int32): TFloatArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice4(Arr:TDoubleArray; Start,Stop,Step:Int32): TDoubleArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice5(Arr:TPointArray; Start,Stop,Step:Int32): TPointArray; cdecl;
begin  Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice6(Arr:TByteArray; Start,Stop,Step:Int32): TByteArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice7(Arr:TBoxArray; Start,Stop,Step:Int32): TBoxArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice8(Arr:String; Start,Stop,Step:Int32): String; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice9(Arr:T2DIntArray; Start,Stop,Step:Int32): T2DIntArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice10(Arr:T2DExtArray; Start,Stop,Step:Int32): T2DExtArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice11(Arr:T2DFloatArray; Start,Stop,Step:Int32): T2DFloatArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice12(Arr:T2DDoubleArray; Start,Stop,Step:Int32): T2DDoubleArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice13(Arr:T2DPointArray; Start,Stop,Step:Int32): T2DPointArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice14(Arr:T2DByteArray; Start,Stop,Step:Int32): T2DByteArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice15(Arr:T2DBoxArray; Start,Stop,Step:Int32): T2DBoxArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;

function exp_Slice16(Arr:TStringArray; Start,Stop,Step:Int32): TStringArray; cdecl;
begin Result := Slice(Arr, Start,Stop,Step); end;


(* Find item in Array *)
function exp_Find1(Arr:TIntArray; Seq:TIntArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find2(Arr:TExtArray; Seq:TExtArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find3(Arr:TFloatArray; Seq:TFloatArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find4(Arr:TDoubleArray; Seq:TDoubleArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find5(Arr:TPointArray; Seq:TPointArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find6(Arr:TByteArray; Seq:TByteArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find7(Arr:TBoxArray; Seq:TBoxArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find8(Arr:String; Seq:String): Int32; cdecl;
begin Result := Find(Arr,Seq); end;

function exp_Find9(Arr:TStringArray; Seq:TStringArray): Int32; cdecl;
begin Result := Find(Arr,Seq); end;


(* Find items in Array *)
function exp_FindAll1(Arr:TIntArray; Seq:TIntArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll2(Arr:TExtArray; Seq:TExtArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll3(Arr:TFloatArray; Seq:TFloatArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll4(Arr:TDoubleArray; Seq:TDoubleArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll5(Arr:TPointArray; Seq:TPointArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll6(Arr:TByteArray; Seq:TByteArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll7(Arr:TBoxArray; Seq:TBoxArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll8(Arr:String; Seq:String): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

function exp_FindAll9(Arr:TStringArray; Seq:TStringArray): TIntArray; cdecl;
begin Result := FindAll(Arr,Seq); end;

{*-----------------------------------------------------------------------------|
| Numeric.pas                                                                  |
|-----------------------------------------------------------------------------*}
function exp_SumTBA(const Arr: TByteArray): Int64; Cdecl;
begin
  Result := SumTBA(Arr);
end;

function exp_SumTIA(const Arr: TIntArray): Int64; Cdecl;
begin
  Result := SumTIA(Arr);
end;

function exp_SumTEA(const Arr: TExtArray): Extended; Cdecl;
begin
  Result := SumTEA(Arr);
end;

function exp_TIACombinations(const Arr: TIntArray; Seq:Integer): T2DIntArray; cdecl;
begin
  Result := TIACombinations(Arr, Seq);
end;

function exp_TEACombinations(const Arr: TExtArray; Seq:Integer): T2DExtArray; cdecl;
begin
  Result := TEACombinations(Arr, Seq);
end;

procedure exp_MinMaxTBA(const Arr: TByteArray; var Min:Byte; var Max:Byte); Cdecl;
begin
  MinMaxTBA(Arr, Min,Max);
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

//function exp_LogscaleTIA(const Freq:TIntArray; Scale: Integer): TIntArray; cdecl;
//begin
//  Result := LogscaleTIA(Freq, Scale);
//end;





{*-----------------------------------------------------------------------------|
| Sorting.pas                                                                  |
|-----------------------------------------------------------------------------*}
procedure exp_SortTBA(var Arr: CoreTypes.TByteArray); Cdecl;
begin
  SortTBA(Arr);
end;

procedure exp_SortTIA(var Arr: TIntArray); Cdecl;
begin
  SortTIA(Arr);
end;

procedure exp_SortTFA(var Arr: TFloatArray); Cdecl;
begin
  SortTFA(Arr);
end;

procedure exp_SortTDA(var Arr: TDoubleArray); Cdecl;
begin
  SortTDA(Arr);
end;

procedure exp_SortTEA(var Arr: TExtArray); Cdecl;
begin
  SortTEA(Arr);
end;

//--------
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

//--------
procedure exp_SortTSA(var Arr: TStringArray; CaseInsensitve:Boolean=False); Cdecl;
begin
  SortTSA(Arr, CaseInsensitve);
end;

procedure exp_SortTSANatural(var Arr: TStringArray); Cdecl;
begin
  SortTSANatural(Arr);
end;

//--------
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

//--------
procedure exp_SortATBAByLength(var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByLength(Arr);
end;

procedure exp_SortATBAByMean(var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByMean(Arr);
end;

procedure exp_SortATBAByFirst(var Arr:T2DByteArray); Cdecl;
begin
  SortATBAByFirst(Arr);
end;

procedure exp_SortATBAByIndex(var Arr:T2DByteArray; index:Int32); Cdecl;
begin
  SortATBAByIndex(Arr, index);
end;

//--------
procedure exp_SortATIAByLength(var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByLength(Arr);
end;

procedure exp_SortATIAByMean(var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByMean(Arr);
end;

procedure exp_SortATIAByFirst(var Arr:T2DIntArray); Cdecl;
begin
  SortATIAByFirst(Arr);
end;

procedure exp_SortATIAByIndex(var Arr:T2DIntArray; index:Int32); Cdecl;
begin
  SortATIAByIndex(Arr, index);
end;

//--------
procedure exp_SortATEAByLength(var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByLength(Arr);
end;

procedure exp_SortATEAByMean(var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByMean(Arr);
end;

procedure exp_SortATEAByFirst(var Arr:T2DExtArray); Cdecl;
begin
  SortATEAByFirst(Arr);
end;

procedure exp_SortATEAByIndex(var Arr:T2DExtArray; index:Int32); Cdecl;
begin
  SortATEAByIndex(Arr, index);
end;



{*-----------------------------------------------------------------------------|
| Finder.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_MatchColorRGB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; cdecl;
begin
  Result := MatchColorRGB(ImgArr, Color, CCMode);
end;

function exp_MatchColorXYZ(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; cdecl;
begin
  Result := MatchColorXYZ(ImgArr, Color, CCMode);
end;

function exp_MatchColorLAB(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; cdecl;
begin
  Result := MatchColorLAB(ImgArr, Color, CCMode);
end;

function exp_MatchColorLCH(const ImgArr:T2DIntArray; Color:Integer; CCMode:TCCorrMode): T2DFloatArray; cdecl;
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

function exp_TPAExtremes(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPAExtremes(TPA);
end;

function exp_TPABBox(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPABBox(TPA);
end;

function exp_TPACenter(const TPA: TPointArray; Method: TCenterAlgo; Inside:Boolean): TPoint; Cdecl;
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

function exp_InvertTPA(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := InvertTPA(TPA);
end;

function exp_RotateTPA(const TPA: TPointArray; Radians: Extended): TPointArray; cdecl;
begin
  Result := RotateTPA(TPA, Radians);
end;

function exp_TPAPartition(const TPA:TPointArray; BoxWidth, BoxHeight:Integer): T2DPointArray; cdecl;
begin
  Result := TPAPartition(TPA, BoxWidth, BoxHeight);
end;

function exp_AlignTPA(const TPA:TPointArray; Method: TAlignAlgo; var Angle:Extended): TPointArray; cdecl;
begin
  Result := AlignTPA(TPA, Method, Angle);
end;

function exp_CleanSortTPA(const TPA: TPointArray): TPointArray; cdecl;
begin
  Result := CleanSortTPA(TPA);
end;


function exp_UniteTPA(const TPA1, TPA2: TPointArray; RemoveDupes:Boolean): TPointArray; cdecl;
begin
  Result := UniteTPA(TPA1, TPA2, RemoveDupes);
end;


procedure exp_TPALine(var TPA:TPointArray; const P1:TPoint; const P2: TPoint); Cdecl;
begin
  TPALine(TPA, P1,P2);
end;

function exp_ConnectTPA(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := ConnectTPA(TPA);
end;

function exp_ConnectTPAEx(TPA:TPointArray; Tension:Extended): TPointArray; cdecl;
begin
  Result := ConnectTPAEx(TPA, Tension);
end;

function exp_XagonPoints(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; cdecl;
begin
  Result := XagonPoints(Center, Sides, Dir);
end;

function exp_TPAEllipseBase(const Center: TPoint; RadiusX, RadiusY:Integer): TPointArray; cdecl;
begin
  Result := TPAEllipseBase(Center, RadiusX, RadiusY);
end;

function exp_TPAEllipse(const Center: TPoint; RadX,RadY:Integer; Filled:Boolean): TPointArray; cdecl;
begin
  Result := TPAEllipse(Center, RadX,RadY, Filled);
end;

function exp_TPACircle(const Center: TPoint; Radius:Integer; Filled:Boolean): TPointArray; cdecl;
begin
  Result := TPACircle(Center, Radius, Filled);
end;

function exp_TPASimplePoly(const Center:TPoint; Sides:Integer; const Dir:TPoint): TPointArray; cdecl;
begin
  Result := TPASimplePoly(Center, Sides, Dir);
end;

function exp_ConvexHull(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := ConvexHull(TPA);
end;

function exp_FloodFillTPAEx(const TPA:TPointArray; const Start:TPoint; EightWay, KeepEdges:Boolean): TPointArray; cdecl;
begin
  Result := FloodFillTPAEx(TPA, Start, EightWay, KeepEdges);
end;


function exp_FloodFillTPA(const TPA:TPointArray; const Start:TPoint; EightWay:Boolean): TPointArray; cdecl;
begin
  Result := FloodFillTPA(TPA,Start,EightWay);
end;

function exp_TPAOutline(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPAOutline(TPA);
end;

function exp_TPABorder(const TPA:TPointArray): TPointArray; cdecl;
begin
  Result := TPABorder(TPA);
end;

function exp_FloodFillPolygon(const Poly:TPointArray; EightWay:Boolean): TPointArray; cdecl;
begin
  Result := FloodFillPolygon(Poly, EightWay);
end;

function exp_ClusterTPAEx(const TPA: TPointArray; Distx,Disty: Integer; EightWay:Boolean): T2DPointArray; cdecl;
begin
  Result := ClusterTPAEx(TPA,DistX,DistY, Eightway);
end;

function exp_ClusterTPA(const TPA: TPointArray; Distance: Integer; EightWay:Boolean): T2DPointArray; cdecl;
begin
  Result := ClusterTPA(TPA,Distance, Eightway);
end;

function exp_TPAEdges(const TPA: TPointArray): TPointArray; cdecl;
begin
  Result := TPAEdges(TPA);
end;



{*-----------------------------------------------------------------------------|
| Spline.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_Spline(TPA:TPointArray; Tension:Extended; Connect:Boolean): TPointArray; cdecl;
begin
  Result := CSpline(TPA, Tension, Connect);
end;




{*-----------------------------------------------------------------------------|
| Morphology.pas                                                               |
|-----------------------------------------------------------------------------*}
function exp_TPASkeleton(const TPA:TPointArray; FMin,FMax:Integer): TPointArray; cdecl;
begin
  Result := TPASkeleton(TPA,FMin,FMax);
end;

function exp_TPAReduce(const TPA:TPointArray; FMin,FMax, Iterations:Integer): TPointArray; cdecl;
begin
  Result := TPAReduce(TPA,FMin,FMax,Iterations);
end;

function exp_TPAExpand(const TPA:TPointArray; Iterations:Integer): TPointArray; cdecl;
begin
  Result := TPAExpand(TPA,Iterations);
end;






{*-----------------------------------------------------------------------------|
| MatrixTools.pas                                                                   |
|-----------------------------------------------------------------------------*}
function exp_NewMatrixEx(W,H, Init:Integer): T2DIntArray; cdecl;
begin
  Result := NewMatrixEx(W,H, Init);
end;

function exp_NewMatrix(W,H:Integer): T2DIntArray; cdecl;
begin
  Result := NewMatrix(W,H);
end;

function exp_TPAToMatrixEx(const TPA:TPointArray; Init, Value:Integer; Align:Boolean): T2DIntArray; cdecl;
begin
  Result := TPAToMatrixEx(TPA,Init,Value,Align);
end;

function exp_TPAToMatrix(const TPA:TPointArray; Value:Integer; Align:Boolean): T2DIntArray; cdecl;
begin
  Result := TPAToMatrix(TPA, Value, Align);
end;

function exp_NormalizeMat(const Mat:T2DIntArray; Alpha, Beta:Integer): T2DIntArray; cdecl;
begin
  Result := NormalizeMat(Mat, Alpha, Beta);
end;

function exp_MatGetCols(const Mat:T2DIntArray; FromCol, ToCol:Integer): T2DIntArray; cdecl;
begin
  Result := MatGetCols(Mat, FromCol, ToCol);
end;

function exp_MatGetRows(const Mat:T2DIntArray; FromRow, ToRow:Integer): T2DIntArray; cdecl;
begin
  Result := MatGetRows(Mat, FromRow, ToRow);
end;

function exp_MatGetArea(const Mat:T2DIntArray; X1,Y1,X2,Y2:Integer): T2DIntArray; cdecl;
begin
  Result := MatGetArea(Mat, X1,Y1,X2,Y2);
end;

function exp_MatFromTIA(const Arr:TIntArray; Width,Height:Integer): T2DIntArray; cdecl;
begin
  Result := MatFromTIA(Arr, Width, Height);
end;

procedure exp_PadMatrix(var Matrix:T2DIntArray; HPad,WPad:Integer); Cdecl;
begin
  PadMatrix(Matrix,HPad,WPad);
end;

function exp_FloodFillMatrix(ImgArr:T2DIntArray; const Start:TPoint; EightWay:Boolean): TPointArray; cdecl;
begin
  Result := FloodFillMatrix(ImgArr, Start, EightWay);
end;






{*-----------------------------------------------------------------------------|
| Imaging.pas                                                                  |
|-----------------------------------------------------------------------------*}
function exp_GaussKernel(KernelRadius:Integer; Sigma:Single): T2DFloatArray; cdecl;
begin
  Result := GaussKernel(KernelRadius, Sigma);
end;

function exp_ImBlur(ImgArr: T2DIntArray; Radius:Integer): T2DIntArray; cdecl;
begin
  Result := ImBlur(ImgArr, Radius);
end;

function exp_ImMedianBlur(ImgArr: T2DIntArray; Radius:Integer): T2DIntArray; cdecl;
begin
  Result := ImMedianBlur(ImgArr, Radius);
end;

function exp_ImBrighten(ImgArr:T2DIntArray; Amount:Extended; Legacy:Boolean): T2DIntArray; cdecl;
begin
  Result := ImBrighten(ImgArr, Amount, Legacy);
end;

function exp_ImEnhance(ImgArr:T2DIntArray; Enhancement:Byte; C:Extended): T2DIntArray; cdecl;
begin
  Result := ImEnhance(ImgArr, Enhancement, C);
end;

function exp_ImThreshold(const ImgArr:T2DIntArray; Threshold, Alpha, Beta:Byte; Invert:Boolean): T2DIntArray; cdecl;
begin
  Result := ImThreshold(ImgArr, Threshold, Alpha, Beta, Invert);
end;

function exp_ImThresholdAdaptive(const ImgArr:T2DIntArray; Alpha, Beta: Byte; Invert:Boolean; Method:TThreshAlgo; C:Integer): T2DIntArray; cdecl;
begin
  Result := ImThresholdAdaptive(ImgArr, Alpha, Beta, Invert, Method, C);
end;


function exp_ImFindContours(const ImgArr:T2DIntArray; Outlines:Boolean): T2DPointArray; cdecl;
begin
  Result := ImFindContours(ImgArr,Outlines);
end;

function exp_ImCEdges(const ImgArr: T2DIntArray; MinDiff: Integer): TPointArray; cdecl;
begin
  Result := ImCEdges(ImgArr, MinDiff);
end;

function exp_ImSobel(const ImgArr: T2DIntArray): T2DIntArray; cdecl;
begin
  Result := ImSobel(ImgArr); 
end;

function exp_ImConvolve(const ImgArr:T2DIntArray; const Mask:T2DFloatArray): T2DIntArray; cdecl;
begin
  Result := ImConvolve(ImgArr, Mask);
end;

procedure exp_ImGaussBlur(const ImgArr:T2DIntArray; var Dest:T2DIntArray; Radius:Int32; Sigma:Single); cdecl;
begin
  ImGaussBlur(ImgArr, Dest, Radius, Sigma);
end;

function exp_ImBlend(ImgArr1,ImgArr2: T2DIntArray; Alpha: Single): T2DIntArray; cdecl;
begin
  Result := ImBlend(ImgArr1, ImgArr2, Alpha);
end;

procedure exp_ImResize(var ImgArr:T2DIntArray; NewW, NewH: Integer; Method:TResizeAlgo); Cdecl;
begin
  ImResize(ImgArr, NewW, NewH, Method);
end;

function exp_ImRotate(ImgArr:T2DIntArray; Angle:Single; Expand:Boolean; Bilinear:Boolean=True): T2DIntArray; Cdecl;
begin
  Result := ImRotate(ImgArr, Angle, Expand, Bilinear);
end;


{*-----------------------------------------------------------------------------|
| Randomize.pas                                                                 |
|-----------------------------------------------------------------------------*}
function exp_RandomTPA(Amount:Integer; MinX,MinY,MaxX,MaxY:Integer): TPointArray; cdecl;
begin
  Result := RandomTPA(Amount,MinX,MinY,MaxX,MaxY);
end;


function exp_RandomCenterTPA(Amount:Integer; CX,CY,RadX,RadY:Integer): TPointArray; cdecl;
begin
  Result := RandomCenterTPA(Amount,CX,CY,RadX,RadY);
end;


function exp_RandomTIA(Amount:Integer; Low,Hi:Integer): TIntArray; cdecl; 
begin
  Result := RandomTIA(Amount,Low,Hi);
end;






{*-----------------------------------------------------------------------------|
| StringTools.pas                                                              |
|-----------------------------------------------------------------------------*}
function exp_StrStrip(const Text, Chars: String): String; Cdecl;
begin
  Result := StrStrip(Text,Chars);
end;

function exp_StrStripL(const Text, Chars: String): String; Cdecl;
begin
  Result := StrStripL(Text,Chars);
end;

function exp_StrStripR(const Text, Chars: String): String; Cdecl;
begin
  Result := StrStripR(Text,Chars);
end;

function exp_StrPosEx(const SubStr, Text:String): TIntArray; cdecl;
begin
  Result := StrPosEx(SubStr, Text);
end;

function exp_StrPosL(const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosL(SubStr, Text);
end;

function exp_StrPosR(const SubStr, Text: String): Integer; Cdecl;
begin
  Result := StrPosR(SubStr, Text);
end;

function exp_StrReplace(const Text, SubStr, RepStr: String; Flags:TReplaceFlags): String; Cdecl;
begin
  Result := StrReplace(Text, SubStr, RepStr, Flags);
end;


function exp_StrExplode(const Text, Sep: String): TStrArray; cdecl;
begin
  Result := StrExplode(Text, Sep);
end;


{*-----------------------------------------------------------------------------|
| CornerDet.pas                                                                  |
|-----------------------------------------------------------------------------*}
function exp_CornerResponse(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer): T2DFloatArray; cdecl;
begin
  Result := CornerResponse(Mat, GaussDev, KSize);
end;

function exp_FindCornerPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; Footprint:Integer): TPointArray; cdecl;
begin
  Result := FindCornerPoints(Mat, GaussDev, KSize, Thresh, Footprint);
end;

function exp_FindCornerMidPoints(const Mat:T2DIntArray; GaussDev:Single; KSize:Integer; Thresh:Single; MinDist:Integer): TPointArray; cdecl;
begin
  Result := FindCornerMidPoints(Mat, GaussDev, KSize, Thresh, MinDist);
end;







{*-----------------------------------------------------------------------------|
| MatrixOps.pas                                                                |
|-----------------------------------------------------------------------------*}
function exp_IndicesB(const Mat:T2DByteArray; Value: Byte; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

function exp_IndicesI(const Mat:T2DIntArray; Value: Integer; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

function exp_IndicesE(const Mat:T2DExtArray; Value: Extended; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

function exp_IndicesD(const Mat:T2DDoubleArray; Value: Double; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;

function exp_IndicesF(const Mat:T2DFloatArray; Value: Single; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, Value, Comparator);
end;


{-------| Extended version of Indices |-------}
function exp_IndicesExB(const Mat:T2DByteArray; B:TBox; Value: Byte; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

function exp_IndicesExI(const Mat:T2DIntArray; B:TBox; Value: Integer; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

function exp_IndicesExE(const Mat:T2DExtArray; B:TBox; Value: Extended; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

function exp_IndicesExD(const Mat:T2DDoubleArray; B:TBox; Value: Double; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;

function exp_IndicesExF(const Mat:T2DFloatArray; B:TBox; Value: Single; const Comparator:TComparator): TPointArray; cdecl;
begin
  Result := Indices(Mat, B, Value, Comparator);
end;


procedure exp_MinMaxB(Mat:T2DByteArray; var Min, Max:Byte); Cdecl;
begin
  MinMax(Mat, Min, Max);
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
function exp_ArgMaxB(Mat:T2DByteArray): TPoint; Cdecl;
begin
  Result := ArgMax(Mat);
end;

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
function exp_ArgMinB(Mat:T2DByteArray): TPoint; Cdecl;
begin
  Result := ArgMin(Mat);
end;

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
function exp_ArgMaxExB(Mat:T2DByteArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMax(Mat,B);
end;

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
function exp_ArgMinExB(Mat:T2DByteArray; B:TBox): TPoint; Cdecl;
begin
  Result := ArgMin(Mat,B);
end;

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



{-------| VarMulti |-------}
function exp_VarMultiB(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean): CoreTypes.TByteArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiI(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TIntArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiE(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TExtArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiD(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TDoubleArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;

function exp_VarMultiF(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TFloatArray; cdecl;
begin
  Result := VarMulti(Mat, Count, HiLo);
end;


{-------| ArgMulti |-------}
function exp_ArgMultiB(const Mat:T2DByteArray; Count: Int32; HiLo:Boolean): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiI(const Mat:T2DIntArray; Count: Int32; HiLo:Boolean): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiE(const Mat:T2DExtArray; Count: Int32; HiLo:Boolean): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiD(const Mat:T2DDoubleArray; Count: Int32; HiLo:Boolean): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;

function exp_ArgMultiF(const Mat:T2DFloatArray; Count: Int32; HiLo:Boolean): TPointArray; cdecl;
begin
  Result := ArgMulti(Mat, Count, HiLo);
end;



{-------| CombineMatrix |-------}
function exp_CombineMatB(const Mat1, Mat2:T2DByteArray; Op:Char): T2DByteArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatI(const Mat1, Mat2:T2DIntArray; Op:Char): T2DIntArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatF(const Mat1, Mat2:T2DFloatArray; Op:Char): T2DFloatArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatD(const Mat1, Mat2:T2DDoubleArray; Op:Char): T2DDoubleArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;

function exp_CombineMatE(const Mat1, Mat2:T2DExtArray; Op:Char): T2DExtArray; cdecl;
begin Result := CombineMatrix(Mat1, Mat2, Op); end;



{-------| Get- & PutValues |-------}
function exp_GetValuesB(const Mat:T2DByteArray; const Indices:TPointArray): TByteArray; cdecl;
begin Result := GetValues(Mat, Indices); end;
 
function exp_GetValuesI(const Mat:T2DIntArray; const Indices:TPointArray): TIntArray; cdecl;
begin Result := GetValues(Mat, Indices); end;
 
function exp_GetValuesF(const Mat:T2DFloatArray; const Indices:TPointArray): TFloatArray; cdecl;
begin Result := GetValues(Mat, Indices); end;
 
function exp_GetValuesD(const Mat:T2DDoubleArray; const Indices:TPointArray): TDoubleArray; cdecl;
begin Result := GetValues(Mat, Indices); end;
 
function exp_GetValuesE(const Mat:T2DExtArray; const Indices:TPointArray): TExtArray; cdecl;
begin Result := GetValues(Mat, Indices); end;
 
//Put
procedure exp_PutValuesB(var Matrix:T2DByteArray; const Indices:TPointArray; Values:CoreTypes.TByteArray); cdecl;
begin PutValues(Matrix, Indices, Values); end;

procedure exp_PutValuesI(var Matrix:T2DIntArray; const Indices:TPointArray; Values:TIntArray); cdecl;
begin PutValues(Matrix, Indices, Values); end;

procedure exp_PutValuesF(var Matrix:T2DFloatArray; const Indices:TPointArray; Values:TFloatArray); cdecl;
begin PutValues(Matrix, Indices, Values); end;

procedure exp_PutValuesD(var Matrix:T2DDoubleArray; const Indices:TPointArray; Values:TDoubleArray); cdecl;
begin PutValues(Matrix, Indices, Values); end;

procedure exp_PutValuesE(var Matrix:T2DExtArray; const Indices:TPointArray; Values:TExtArray); cdecl;
begin PutValues(Matrix, Indices, Values); end;
