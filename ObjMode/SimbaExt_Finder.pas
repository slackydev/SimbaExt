{!DOCTOPIC} {
  Finder functions 
}

{!DOCREF} {
  @method: function se.MatchColor(const ImgArr:TIntMatrix; Color:Integer; CCMode:TCCorrMode; MatchAlgo:TMatchAlgo): T2DFloatArray;
  @desc: Correlates the color with the given image. Returns a match-array
}
function SimbaExt.MatchColor(const ImgArr:TIntMatrix; Color:Integer; CCMode:TCCorrMode; MatchAlgo:TMatchAlgo): T2DFloatArray;
begin
  case MatchAlgo of
    _RGB_: exp_MatchColorRGB(ImgArr, Color, CCMode, Result);
    _XYZ_: exp_MatchColorXYZ(ImgArr, Color, CCMode, Result);
    _LAB_: exp_MatchColorLAB(ImgArr, Color, CCMode, Result);
    _LCH_: exp_MatchColorLCH(ImgArr, Color, CCMode, Result);
  end;
end;



{!DOCREF} {
  @method: function se.ImFindColorTolEx(const ImgArr:TIntMatrix; var TPA:TPointArray; Color, Tol:Integer): Boolean;
  @desc: Deprecated
}
function SimbaExt.ImFindColorTolEx(const ImgArr:TIntMatrix; var TPA:TPointArray; Color, Tol:Integer): Boolean;
begin
  RaiseWarning('ImFindColorTolEx is deprecated and will be removed, use "se.FindColorTolEx', ERR_DEPRECATED);
  Result := exp_ImFindColorTolEx(ImgArr, TPA, Color, Tol);
end;

{!DOCREF} {
  @method: function se.ImFindColorsTolEx(const ImgArr:TIntMatrix; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;
  @desc: Deprecated
}
function SimbaExt.ImFindColorsTolEx(const ImgArr:TIntMatrix; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;
begin
  RaiseWarning('ImFindColorsTolEx is deprecated and will be removed, use "se.FindColorTolEx', ERR_DEPRECATED);
  Result := exp_ImFindColorsTolEx(ImgArr, TPA, Colors, Tol);
end;

{!DOCREF} {
  @method: function se.ImFindColorsTolExLCH(const ImgArr:TIntMatrix; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;
  @desc: Deprecated
}
function SimbaExt.ImFindColorTolExLCH(const ImgArr:TIntMatrix; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;
begin
  RaiseWarning('ImFindColorTolExLCH is deprecated and will be removed, use "se.FindColorTolEx', ERR_DEPRECATED);
  Result := exp_ImFindColorTolExLCH(ImgArr, TPA, Color, ColorTol, LightTol);
end;

{!DOCREF} {
  @method: function se.ImFindColorsTolExLAB(const ImgArr:TIntMatrix; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;
  @desc: Deprecated
}
function SimbaExt.ImFindColorTolExLAB(const ImgArr:TIntMatrix; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;
begin
  RaiseWarning('ImFindColorTolExLAB is deprecated and will be removed, use "se.FindColorTolEx"', ERR_DEPRECATED);
  Result := exp_ImFindColorTolExLAB(ImgArr, TPA, Color, ColorTol, LightTol);
end;


{!DOCREF} {
  @method: function se.FindColorTolEx(var TPA:TPointArray; Color:Integer; Area:TBox; Similarity:Single; MatchAlgo: TMatchAlgo): Boolean;
  @desc:
    Serach for a spesific color on your screen with a tolerance.
    
    LAB should be very fast compared to CTS(3) in simba. I assume around 8-10x faster in general.
    LCH which is LAB-color measured another way should also be "fast enough".
    
    @params:
      TPA:        The resulting points
      Color:      The color to search for
      Area:       A Tbox of where to search.
      Similarity: 0.0 to 1.0 where +/-1.0 should be exact match.    
      MatchAlgo:  How we measure color difference: _RGB_, _XYZ_, _LAB_ and _LCH_ 
}
function SimbaExt.FindColorTolEx(var TPA:TPointArray; Color:Integer; Area:TBox; Similarity:Single; MatchAlgo: TMatchAlgo): Boolean;
var 
  W,H:Integer;
  Img:TIntMatrix;
  Corr: TFloatMatrix;
  Bmp:Integer;
begin
  Result := False;
  GetClientDimensions(W,H);
  
  if (Area.X2 >= W) or (Area.X2 <= -1) then Area.X2 := W-1;
  if (Area.Y2 >= H) or (Area.Y2 <= -1) then Area.Y2 := H-1;
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then Exit;
  
  Bmp := BitmapFromClient(Area.X1,Area.Y1,Area.X2,Area.Y2); 
  Img := BitmapToMatrix(Bmp);
  FreeBitmap(bmp);

  case MatchAlgo of   
    _RGB_: exp_MatchColorRGB(Img, Color, CC_ChebNormed, Corr);
    _XYZ_: exp_MatchColorXYZ(Img, Color, CC_ChebNormed, Corr);
    _LAB_: exp_MatchColorLAB(Img, Color, CC_ChebNormed, Corr);
    _LCH_: exp_MatchColorLCH(Img, Color, CC_EuclidNormed, Corr);
  end;
  
  TPA := Corr.Indices(Similarity, __GE__);      
  SetLength(Img, 0);
  if (Length(TPA) < 0) then Exit;
  if (Area.X1=0) and (Area.Y1 = 0) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;
