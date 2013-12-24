{*=========================================================================================|
| Finder.pas                                                                               |
|=========================================================================================*}
function XT_ImFindColorTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, Tol:Integer): Boolean;
begin
  Result := exp_ImFindColorTolEx(ImgArr, TPA, Color, Tol);
end;

function XT_ImFindColorsTolEx(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Colors:TIntegerArray; Tol:Integer): Boolean;
begin
  Result := exp_ImFindColorsTolEx(ImgArr, TPA, Colors, Tol);
end;

function XT_ImFindColorTolExLCH(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;
begin
  Result := exp_ImFindColorTolExLCH(ImgArr, TPA, Color, ColorTol, LightTol);
end;

function XT_ImFindColorTolExLAB(const ImgArr:T2DIntegerArray; var TPA:TPointArray; Color, ColorTol, LightTol:Integer): Boolean;
begin
  Result := exp_ImFindColorTolExLAB(ImgArr, TPA, Color, ColorTol, LightTol);
end;


{*
  LAB should be very fast compared to CTS(3) in simba. I assume around 8-11x faster in general.
  LCH which is LAB-color measured another way should also be fast.
  RGB might not be as fast, most likely slower then Simba-CTS(1)
*}
function XT_FindColorTolEx(var TPA:TPointArray; Color:Integer; Area:TBox; ColorTol, LightTol:Integer; MatchAlgo: XT_MatchAlgo): Boolean;
var 
  W,H:Integer;
  Img:T2DIntegerArray;
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
    RGB: Result := XT_ImFindColorTolEx(Img, TPA, Color, ColorTol);
    LAB: Result := XT_ImFindColorTolExLAB(Img, TPA, Color, ColorTol, LightTol);
    LCH: Result := XT_ImFindColorTolExLCH(Img, TPA, Color, ColorTol, LightTol);
  end;
  SetLength(Img, 0);
  if not(Result) then Exit;
  if (Area.X1=0) and (Area.Y1 = 0) then Exit;
  OffsetTPA(TPA, Point(Area.X1, Area.Y1));
end;